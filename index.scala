//> using option -Wunused:all
//> using dep com.lihaoyi::os-lib::0.10.2
//> using scala 3.5.0-RC2
import scala.collection.mutable.ArrayBuilder

opaque type TRIGRAM = String
object TRIGRAM:
  def apply(a: Char, b: Char, c: Char): TRIGRAM = s"$a$b$c"
  given Ordering[TRIGRAM] = Ordering.String

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

  def indexFile(file: os.Path) =
    val entries = Array.newBuilder[IndexEntry]

    os.read
      .lines(file)
      .zipWithIndex
      .foreach: (line, idx) =>
        val tokens = index(line)

        if tokens.nonEmpty then
          entries += IndexEntry(
            tokens,
            Location(file, idx + 1)
          )
        end if
    entries.result()
  end indexFile

end TrigramIndexer

case class Location(file: os.Path, line: Int)

case class IndexEntry(tokens: Array[TRIGRAM], location: Location):
  def render =
    s"IndexEntry(tokens = ${tokens.mkString("[", ", ", "]")})"

  def jaccardSimilarity(other: IndexEntry): Float =
    var shared = 0
    var dissimilar = 0

    var shortIndex = 0
    var longIndex = 0

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

@main def index(path: String, extensions: String) =
  val acc = collection.mutable.Map
    .empty[TRIGRAM, ArrayBuilder[Location]]

  val indexer = TrigramIndexer()

  val ext = extensions.split(",").toSet

  os.walk.stream
    .attrs(
      path = os.Path(path, os.pwd),
      skip = (p, s) => s.isFile && !ext.contains(p.ext)
    )
    .foreach: (path, info) =>
      if info.isFile then
        val file = indexer.indexFile(path)
        file.foreach: entry =>
          entry.tokens.foreach: tok =>
            acc.getOrElseUpdate(tok, Array.newBuilder).addOne(entry.location)

  // var stop = false

  val fin = acc.map((tg, loc) => tg -> loc.result().toSet).toMap

  def search(query: String) =
    val tokens = indexer.index(query)

    val res = collection.mutable.Map.empty[Location, Int].withDefaultValue(0)

    tokens.foreach: token =>
      fin.get(token) match
        case None => // unknown token
        case Some(value) =>
          value.foreach: loc =>
            res(loc) += 1

    res.toList
      .map((l, cnt) => l -> cnt.toFloat / tokens.length.toFloat)
      .sortBy(_._2 * -1)
  end search

  search("TrigramIndexer").take(10).foreach(println)

end index
