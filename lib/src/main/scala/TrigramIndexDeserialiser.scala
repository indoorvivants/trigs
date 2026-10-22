package trigs

import java.io.BufferedInputStream
import java.io.DataInputStream

/** Reads the format written by [[TrigramIndexSerialiser]] */
object TrigramIndexDeserialiser:
  def deserialise[Id](bytes: java.io.InputStream)(using
      codec: IdCodec[Id],
      progress: Progress
  ): TrigramIndex[Id] =
    val ds = DataInputStream(BufferedInputStream(bytes))

    val magic   = ds.readInt()
    val version = ds.readInt()
    if magic != TrigramIndexSerialiser.Magic ||
      version != TrigramIndexSerialiser.Version
    then
      throw IllegalArgumentException(
        s"Not a trigs index, or one written by an incompatible version (expected format ${TrigramIndexSerialiser.Version}) - re-run `trigs index`"
      )

    progress.info("reading documents index")
    val documents = Vector.fill(ds.readInt()):
      codec.decode(ds.readNBytes(ds.readInt()))

    progress.info("reading locations index")
    val numLocations     = ds.readInt()
    val locationDocument = new Array[Int](numLocations)
    val locationLine     = new Array[Int](numLocations)
    for location <- 0 until numLocations do
      locationDocument(location) = ds.readInt()
      locationLine(location) = ds.readInt()

    progress.info("reading trigrams index")
    val locationTrigrams = new Array[Int](numLocations)
    val postings = Map.from:
      Iterator.fill(ds.readInt()):
        val trigram   = Trigram(ds.readChar(), ds.readChar(), ds.readChar())
        val locations = Array.fill(ds.readInt())(ds.readInt())
        locations.foreach(locationTrigrams(_) += 1)
        trigram -> locations

    TrigramIndex(
      documents,
      locationDocument,
      locationLine,
      locationTrigrams,
      postings
    )
  end deserialise
end TrigramIndexDeserialiser
