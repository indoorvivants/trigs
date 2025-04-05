package trigs
class TrigramIndexer:
  def index(s: String): Array[TRIGRAM] =
    val length = s.length()
    if length == 0 then Array.empty[TRIGRAM]
    else if length == 1 then Array(TRIGRAM(' ', ' ', s(0)))
    else if length == 2 then
      Array(
        TRIGRAM(' ', ' ', s(0)),
        TRIGRAM(' ', s(0), s(1))
      )
    else
      val all = Array.newBuilder[TRIGRAM]

      var i = 0

      while i < length - 2 do
        val char = s(i).toLower
        val next = s(i + 1).toLower
        val last = s(i + 2).toLower

        if char.isLetterOrDigit && next.isLetterOrDigit && last.isLetterOrDigit
        then all += TRIGRAM(char, next, last)

        i += 1

      end while

      all.result().sorted
    end if
  end index

  def indexFile(file: os.Path)(using Progress): Array[IndexEntry] =
    val entries = Array.newBuilder[IndexEntry]

    os.read.lines
      .stream(file)
      .zipWithIndex
      .foreach: (line, idx) =>
        val tokens = index(line)

        if tokens.nonEmpty then
          progress.debug(
            s"${file.relativeTo(os.pwd)}:$idx   ${tokens.distinct.mkString(", ")}"
          )
          entries += IndexEntry(
            tokens,
            Location(file, idx + 1)
          )
        end if
    entries.result()
  end indexFile
end TrigramIndexer

object TrigramIndexer
