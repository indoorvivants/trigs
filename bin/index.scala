package trigs
import decline_derive.*

@Name("trigs")
enum CLIConfig derives CommandApplication:
  case Index(
      @Positional("path")
      path: String,
      @Help("comma-separated list of file extensions to index")
      @Short("e")
      extensions: String,
      @Short("q")
      quiet: Boolean,
      @Short("v")
      verbose: Boolean,
      @Short("o")
      out: Option[String]
  )
  case Search(
      @Short("p")
      path: Option[String],
      @Positional("query")
      query: Option[String],
      @Short("i")
      interactive: Boolean,
      @Short("l")
      limit: Option[Int]
  )

end CLIConfig

@main def index(args: String*) =
  CommandApplication
    .parseOrExit[CLIConfig](args) match
    case i: CLIConfig.Index  => commandIndex(i)
    case s: CLIConfig.Search => commandSearch(s)

end index
