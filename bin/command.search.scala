package trigs

def commandSearch(cli: CLIConfig.Search) =

  val indexer    = TrigramIndexer()
  given Progress = Progress.Quiet
  val path       = cli.path.fold(os.pwd / "index.trig")(p => os.Path(p, os.pwd))
  val index      = TrigramIndex.read(path)
  val limit      = cli.limit.getOrElse(10)

  def search(query: String): List[(Location, Float)] =
    val tokens = indexer.index(query)
    println(s"Tokens: ${tokens.mkString(", ")}")

    val res = collection.mutable.Map.empty[Location, Int].withDefaultValue(0)

    tokens.foreach: token =>
      index.locs.get(token) match
        case None => // unknown token
        case Some(value) =>
          value.foreach: loc =>
            res(loc) += 1

    res.toList
      .map((l, cnt) => l -> cnt.toFloat / tokens.length.toFloat)
      .sortBy(_._2 * -1)
  end search

  def printResults(query: String) =
    val results = search(query)
      .take(limit)

    val fileContents =
      Map.empty[os.Path, IndexedSeq[String]].withDefault(os.read.lines(_))

    results
      .foreach: (loc, score) =>
        val rp = loc.file.relativeTo(os.pwd)
        println(fansi.Bold.On(rp.toString).render + ":" + loc.line)
        println(fileContents(loc.file)(loc.line - 1))
  end printResults

  if cli.interactive then
    var exit = false

    cue4s.Prompts.sync.use: prompts =>
      println("Query (or :exit)")
      while !exit do
        val str = prompts.text("query").getOrThrow
        exit = str.equalsIgnoreCase(":exit")

        if !exit then printResults(str)
        end if

      end while
  else if cli.query.nonEmpty then printResults(cli.query.get)
  end if

end commandSearch
