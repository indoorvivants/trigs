package trigs

class TrigramIndexer:
  def index(s: String): Array[Trigram] =
    val length = s.length()
    if length == 0 then Array.empty[Trigram]
    else if length == 1 then Array(Trigram(' ', ' ', s(0)))
    else if length == 2 then
      Array(
        Trigram(' ', ' ', s(0)),
        Trigram(' ', s(0), s(1))
      )
    else
      val all = Array.newBuilder[Trigram]

      var i = 0

      while i < length - 2 do
        val char = s(i).toLower
        val next = s(i + 1).toLower
        val last = s(i + 2).toLower

        if char.isLetterOrDigit && next.isLetterOrDigit && last.isLetterOrDigit
        then all += Trigram(char, next, last)

        i += 1

      end while

      all.result().sorted
    end if
  end index
end TrigramIndexer

object TrigramIndexer
