package trigs

import cue4s.AnsiTerminal
import cue4s.Output

def commandSearch(cli: CLIConfig.Search) =

  val indexer    = TrigramIndexer()
  given Progress = Progress.Quiet
  val path       = cli.path.fold(os.pwd / "index.trig")(p => os.Path(p, os.pwd))
  val index      = TrigramIndex.read[os.Path](path.toNIO)
  val limit      = cli.limit.getOrElse(10)

  val fileContents = collection.mutable.Map.empty[os.Path, Option[IndexedSeq[String]]]

  def lineText(file: os.Path, line: Int): Option[String] =
    fileContents
      .getOrElseUpdate(file, util.Try(os.read.lines(file)).toOption)
      .flatMap(_.lift(line - 1))

  def printResults(query: String) =
    println(s"Tokens: ${indexer.index(query).mkString(", ")}")

    index
      .search(query, limit, lineText)
      .foreach: hit =>
        val rp = hit.id.relativeTo(os.pwd)
        println(fansi.Bold.On(rp.toString).render + ":" + hit.line)
        lineText(hit.id, hit.line).foreach(println)
  end printResults

  if cli.interactive then
    var exit = false

    cue4s.Prompts.sync.use: prompts =>
      println("Query (or :exit)")
      while !exit do
        val str = prompts.text("query").getOrThrow
        exit = str.equalsIgnoreCase(":exit")

        if !exit then
          AnsiTerminal(Output.Std).screenClear()
          printResults(str)
        end if

      end while
  else if cli.query.nonEmpty then printResults(cli.query.get)
  end if

end commandSearch
