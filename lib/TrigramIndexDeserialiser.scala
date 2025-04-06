package trigs
import java.io.DataInputStream
import java.io.BufferedInputStream
object TrigramIndexDeserialiser:
  def deserialiseIntoBuilder(bytes: java.io.InputStream)(using
      Progress
  ): TrigramIndexMutableBuilder =
    val ds = DataInputStream(BufferedInputStream(bytes))

    val numLocationEntries = ds.readInt()

    val locationIndexBuilder = Map.newBuilder[Int, Location]

    progress.info("reading locations index")
    for _ <- 0 until numLocationEntries do
      val id       = ds.readInt()
      val line     = ds.readInt()
      val length   = ds.readInt()
      val strBytes = String(ds.readNBytes(length))

      locationIndexBuilder += id -> Location(os.Path(strBytes), line)
    end for

    val locationIndex = locationIndexBuilder.result()

    progress.info("reading trigrams index")
    val numTrigrams     = ds.readInt()
    val trigramsBuilder = Map.newBuilder[Int, Trigram]
    for _ <- 0 until numTrigrams do
      val id      = ds.readInt()
      val c1      = ds.readChar()
      val c2      = ds.readChar()
      val c3      = ds.readChar()
      val trigram = Trigram(c1, c2, c3)

      trigramsBuilder += id -> trigram
    end for

    val trigrams = trigramsBuilder.result()

    val indexBuilder   = Map.newBuilder[Int, Set[Int]]
    val numOccurrences = ds.readInt()
    progress.info("reading occurrences index")
    for _ <- 0 until numOccurrences do
      val id     = ds.readInt()
      val cnt    = ds.readInt()
      val locIds = Set.tabulate(cnt)(_ => ds.readInt())

      // indexBuilder += trigrams(id) -> locIds.map(locationIndex.apply)
      indexBuilder += id -> locIds
    end for

    progress.info("building final index")
    TrigramIndexMutableBuilder.from(
      trigrams,
      locationIndex,
      indexBuilder.result()
    )
  end deserialiseIntoBuilder
end TrigramIndexDeserialiser
