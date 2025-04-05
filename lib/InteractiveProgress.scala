package trigs

import cue4s.*

private given AsString[fansi.Str] = _.render

class InteractiveProgress(
    terminal: Terminal,
    out: Output,
    debug: Boolean = false
) extends AutoCloseable,
      Progress:
  override def debug(msg: String): Unit =
    if debug then out.outLn(fansi.Color.Yellow(msg))
  override def error(msg: String): Unit =
    out.outLn(fansi.Color.Red(fansi.Bold.On(msg)))

  override def info(msg: String): Unit =
    terminal.moveHorizontalTo(0).eraseToEndOfLine().moveHorizontalTo(0)
    Output.Std.out(
      fansi.Color.Green("[- ") ++
        fansi.Color.LightBlue(msg) ++
        fansi.Color.Green(" -]")
    )

  override def error(msg: String, exc: Throwable): Unit =
    out.outLn(fansi.Color.Red(fansi.Bold.On(msg)))
    out.outLn(fansi.Color.Red(fansi.Bold.On(exc.toString())))
  override def close(): Unit =
    terminal.eraseEntireLine().moveHorizontalTo(0)
end InteractiveProgress
