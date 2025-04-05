package trigs

case class Location(file: os.Path, line: Int)

case class IndexEntry(tokens: Array[TRIGRAM], location: Location):
  def render =
    s"IndexEntry(tokens = ${tokens.mkString("[", ", ", "]")})"

  def jaccardSimilarity(other: IndexEntry): Float =
    var shared     = 0
    var dissimilar = 0

    var shortIndex = 0
    var longIndex  = 0

    val (longest, shortest) =
      if other.tokens.length > this.tokens.length then (other, this)
      else (this, other)

    var overlapStarted = false
    while longIndex < longest.tokens.length do
      if shortIndex < shortest.tokens.length && longest.tokens(
          longIndex
        ) == shortest.tokens(shortIndex)
      then
        if !overlapStarted then overlapStarted = true

        shared += 1
      else dissimilar += 1
      end if

      if overlapStarted then shortIndex += 1

      longIndex += 1

    end while

    (shared.toFloat) / (shared.toFloat + dissimilar.toFloat)

  end jaccardSimilarity
end IndexEntry
