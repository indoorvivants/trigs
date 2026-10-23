package trigs

def commandIndex(cli: CLIConfig.Index) =

  given Progress =
    if cli.quiet then Progress.Quiet
    else
      Progress.Printer.setMinLevel(
        Level.Info
      ) // InteractiveProgress(AnsiTerminal(Output.Std), Output.Std, cli.verbose)

  val ext = cli.extensions.split(",").toSet

  val builder = IndexBuilder[os.Path](
    parallelism = Runtime.getRuntime.availableProcessors,
    onIndexed = path => progress.info(s"FINISHED: $path"),
    onError =
      Some((path, exc) => progress.error(s"Failed to index $path: $exc", exc))
  )

  def enqueue(path: os.Path): Unit =
    val str = path.toString
    if str.length > 50 then progress.info("..." + str.takeRight(50))
    else progress.info(str)
    builder.addLazy(path, () => os.read.inputStream(path))

  cli.path match
    case "-" =>
      io.Source
        .fromInputStream(System.in)
        .getLines()
        .map(os.Path(_, os.pwd))
        .filter(p => ext.contains(p.ext) && os.isFile(p))
        .foreach(enqueue)
    case _ =>
      os.walk.stream
        .attrs(
          path = os.Path(cli.path, os.pwd),
          skip = (p, s) =>
            // progress.debug(s"Testing path ${p.asInstanceOf[os.Path].wrapped.getClass()}")
            s.isFile && !ext.contains(p.ext)
        )
        .foreach: (path, info) =>
          if info.isFile then enqueue(path)
  end match

  progress.info("Waiting for indexing to finish....")
  val fin = builder.result()

  progress.info("Serialising....")
  val b = TrigramIndexSerialiser.serialise(fin)

  val destination = cli.out.fold(os.pwd / "index.trig")(p => os.Path(p, os.pwd))

  progress.info(s"Writing to $destination....")
  os.write.over(destination, b)

  progress.close()

end commandIndex
