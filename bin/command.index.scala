package trigs
import collection.mutable.Builder
import cue4s.*

def commandIndex(cli: CLIConfig.Index) =
  val acc = collection.mutable.Map
    .empty[TRIGRAM, Builder[Location, Set[Location]]]

  val indexer = TrigramIndexer()

  val ext = cli.extensions.split(",").toSet

  given Progress =
    if cli.quiet then Progress.Quiet
    else InteractiveProgress(AnsiTerminal(Output.Std), Output.Std, cli.verbose)

  def indexFile(path: os.Path, info: os.StatInfo): Unit =
    if info.isFile then
      val file = indexer.indexFile(path)

      val str = path.toString
      if str.length > 50 then progress.info("..." + str.takeRight(50))
      else progress.info(str)

      file.foreach: entry =>
        entry.tokens.foreach: tok =>
          acc.getOrElseUpdate(tok, Set.newBuilder).addOne(entry.location)
    end if
  end indexFile

  cli.path match
    case "-" =>
      io.Source
        .fromInputStream(System.in)
        .getLines()
        .map(os.Path(_, os.pwd))
        .filter(p => ext.contains(p.ext))
        .map(path => path -> os.stat(path))
        .foreach(indexFile)
    case _ =>
      os.walk.stream
        .attrs(
          path = os.Path(cli.path, os.pwd),
          skip = (p, s) => s.isFile && !ext.contains(p.ext)
        )
        .foreach(indexFile)
  end match

  progress.info("Accumulating....")

  val fin = acc.map((tg, loc) => tg -> loc.result()).toMap

  progress.info("Serialising....")
  val b = TrigramIndexSerialiser.serialise(fin)

  val destination = cli.out.fold(os.pwd / "index.trig")(p => os.Path(p, os.pwd))

  progress.info(s"Writing to $destination....")
  os.write.over(destination, b)

  progress.close()

end commandIndex
