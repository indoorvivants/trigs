package trigs

enum Level:
  case Debug, Info, Warn, Error

trait Progress:
  def info(msg: String): Unit
  def debug(msg: => String): Unit
  def error(msg: String): Unit
  def warn(msg: String): Unit
  def error(msg: String, exc: Throwable): Unit
  def close(): Unit
  def setMinLevel(level: Level): Progress =
    level match
      case Level.Debug =>
        this
      case Level.Info =>
        val old = this
        new Progress:
          export old.{debug as _, *}
          override def debug(msg: => String): Unit = ()

      case Level.Warn =>
        val old = this
        new Progress:
          export old.{debug as _, info as _, *}
          override def debug(msg: => String): Unit = ()
          override def info(msg: String): Unit     = ()
      case trigs.Level.Error =>
        val old = this
        new Progress:
          export old.{debug as _, info as _, warn as _, *}
          override def debug(msg: => String): Unit = ()
          override def info(msg: String): Unit     = ()
          override def warn(msg: String): Unit     = ()
end Progress

object Progress:
  object Quiet extends Progress:
    override def info(msg: String): Unit                  = ()
    override def debug(msg: => String): Unit              = ()
    override def warn(msg: String): Unit                  = ()
    override def error(msg: String, exc: Throwable): Unit = ()
    override def error(msg: String): Unit                 = ()
    override def close(): Unit                            = ()
  object Printer extends Progress:
    override def info(msg: String): Unit =
      System.err.println("[INFO] " + msg)
    override def debug(msg: => String): Unit =
      System.err.println("[DEBUG] " + msg)
    override def error(msg: String, exc: Throwable): Unit =
      System.err.println("[ERROR] " + msg)
    override def error(msg: String): Unit =
      System.err.println("[ERROR] " + msg)
    override def warn(msg: String): Unit =
      System.err.println("[WARN] " + msg)
    override def close(): Unit = ()
  end Printer
end Progress

inline def progress = compiletime.summonInline[Progress]
