package trigs
trait Progress:
  def info(msg: String): Unit
  def debug(msg: String): Unit
  def error(msg: String): Unit
  def error(msg: String, exc: Throwable): Unit
  def close(): Unit

object Progress:
  object Quiet extends Progress:
    override inline def info(msg: String): Unit                  = ()
    override inline def debug(msg: String): Unit                 = ()
    override inline def error(msg: String, exc: Throwable): Unit = ()
    override inline def error(msg: String): Unit                 = ()
    override inline def close(): Unit                            = ()
end Progress

inline def progress = compiletime.summonInline[Progress]
