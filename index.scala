//> using option -Wunused:all
//> using dep com.lihaoyi::os-lib::0.10.2
//> using scala 3.5.0-RC2
import scala.collection.mutable.ArrayBuilder
import java.nio.ByteBuffer
import java.io.DataInputStream

object types:
  opaque type TRIGRAM = String
  object TRIGRAM:
    def apply(a: Char, b: Char, c: Char): TRIGRAM = s"$a$b$c"

    extension (t: TRIGRAM)
      def _1: Char = t(0)
      def _2: Char = t(1)
      def _3: Char = t(2)
    given Ordering[TRIGRAM] = Ordering.String
  end TRIGRAM
end types

import types.*

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

object TrigramIndexer

def serialiseIndex(nice: Map[TRIGRAM, Set[Location]]): Array[Byte] =
  val sorted = nice.toList.sortBy(_._1)
  val locationsIndex =
    val builder = collection.mutable.Map.empty[Location, Int]
    nice.valuesIterator
      .flatMap(_.toIterator)
      .foreach: loc =>
        builder.getOrElseUpdate(loc, builder.size)
    builder.toMap
  end locationsIndex

  val trigramIds =
    val builder = collection.mutable.Map.empty[TRIGRAM, Int]
    nice.keysIterator.foreach: trigram =>
      builder.getOrElseUpdate(trigram, builder.size)
    builder.toMap
  end trigramIds

  val locationsIndexSizeEstimate = locationsIndex.map { (loc, _) =>
    val fileNameSize = loc.file.toString.getBytes().length
    val fileNameFieldLength = 4 // Int?
    val idLength = 4
    val lineLength = 4
    fileNameSize + fileNameFieldLength + idLength + lineLength
  }.sum

  val locationCountSize = 4

  val trigramsSizeEstimate = trigramIds
    .map: (trigram, id) =>
      2 * 3 + 4 + 4
    .sum

  val trigramCountSize = 4

  val indexSizeEstimate =
    nice.map((_, locs) => 4 + 4 + locs.size * 4).sum

  val b = ByteBuffer.allocate(
    locationCountSize + locationsIndexSizeEstimate + trigramCountSize + trigramsSizeEstimate + indexSizeEstimate
  )

  b.putInt(locationsIndex.size)

  locationsIndex.foreach: (location, id) =>
    val loc = location.file.toString
    b.putInt(id)
    b.putInt(location.line)
    b.putInt(loc.length)
    b.put(loc.getBytes())

  b.putInt(trigramIds.size)

  trigramIds.foreach: (trigram, id) =>
    b.putInt(id)
    b.putChar(trigram._1)
    b.putChar(trigram._2)
    b.putChar(trigram._3)

  b.putInt(nice.size)

  nice.foreach: (trigram, locations) =>
    b.putInt(trigramIds(trigram))
    b.putInt(locations.size)
    locations.foreach: loc =>
      b.putInt(locationsIndex(loc))

  b.array()
end serialiseIndex

def readIndex(loc: os.Path) =
  deserialiseIndex(os.read.inputStream(loc))

def deserialiseIndex(bytes: java.io.InputStream): Map[TRIGRAM, Set[Location]] =
  val ds = DataInputStream(bytes)

  val numLocationEntries = ds.readInt()

  val locationIndexBuilder = Map.newBuilder[Int, Location]

  for _ <- 0 until numLocationEntries do
    val id = ds.readInt()
    val line = ds.readInt()
    val length = ds.readInt()
    val strBytes = String(ds.readNBytes(length))

    locationIndexBuilder += id -> Location(os.Path(strBytes), line)
  end for

  val locationIndex = locationIndexBuilder.result()

  val numTrigrams = ds.readInt()
  val trigramsBuilder = Map.newBuilder[Int, TRIGRAM]
  for _ <- 0 until numTrigrams do
    val id = ds.readInt()
    val c1 = ds.readChar()
    val c2 = ds.readChar()
    val c3 = ds.readChar()
    val trigram = TRIGRAM(c1, c2, c3)

    trigramsBuilder += id -> trigram
  end for

  val trigrams = trigramsBuilder.result()

  val indexBuilder = Map.newBuilder[TRIGRAM, Set[Location]]
  val numOccurrences = ds.readInt()
  for _ <- 0 until numOccurrences do
    val id = ds.readInt()
    val cnt = ds.readInt()
    val locIds = Set.tabulate(cnt)(_ => ds.readInt())

    indexBuilder += trigrams(id) -> locIds.map(locationIndex.apply)
  end for

  indexBuilder.result()
end deserialiseIndex

@main def index(pathInput: String, extensions: String) =
  val acc = collection.mutable.Map
    .empty[TRIGRAM, ArrayBuilder[Location]]

  val indexer = TrigramIndexer()

  val ext = extensions.split(",").toSet

  def indexFile(path: os.Path, info: os.StatInfo) =
    if info.isFile then
      val file = indexer.indexFile(path)
      file.foreach: entry =>
        entry.tokens.foreach: tok =>
          acc.getOrElseUpdate(tok, Array.newBuilder).addOne(entry.location)
    end if
  end indexFile

  pathInput match
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
          path = os.Path(pathInput, os.pwd),
          skip = (p, s) => s.isFile && !ext.contains(p.ext)
        )
        .foreach(indexFile)
  end match

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

  val b = serialiseIndex(fin)

  val newValue = os.pwd / "index.trig"
  os.write.over(newValue, b)
  println(readIndex(newValue))

  // var exit = false

  // while !exit do
  //   print("> ")
  //   val str = io.StdIn.readLine().trim()
  //   exit = str.equalsIgnoreCase(":exit")

  //   if !exit then search(str).take(10).foreach(println)

  // end while

end index
