package trigs
import java.nio.ByteBuffer

/** Binary layout. Numbers are big-endian, ids are positions within their
  * section:
  *
  * {{{
  * magic, version
  * documents: count, (id byte length, id bytes from IdCodec)*
  * locations: count, (document id, line)*
  * trigrams:  count, (3 chars, location count, location id*)*
  * }}}
  */
object TrigramIndexSerialiser:
  val Magic   = 0x54524947 // "TRIG"
  val Version = 2

  def serialise[Id](index: TrigramIndex[Id])(using
      codec: IdCodec[Id],
      progress: Progress
  ): Array[Byte] =
    val idBytes = index.documents.iterator.map(codec.encode).toArray

    progress.info("calculating serialised index size")

    val headerSize = 2 * sizeOf[Int] // magic, version

    val documentsStorageSize = sizeOf[Int] + // count
      idBytes.iterator.map(sizeOf[Int] + _.length).sum

    val locationsStorageSize = sizeOf[Int] + // count
      index.locationLine.length * (
        sizeOf[Int] + // document id
          sizeOf[Int] // line
      )

    val trigramsStorageSize = sizeOf[Int] + // count
      index.postings.valuesIterator
        .map: locations =>
          3 * sizeOf[Char] +                 // trigram itself
            sizeOf[Int] +                    // number of locations
            (locations.length * sizeOf[Int]) // location ids
        .sum

    val totalSize =
      headerSize + documentsStorageSize + locationsStorageSize + trigramsStorageSize

    progress.info(
      s"allocating buffer for serialised index ($documentsStorageSize for documents, $locationsStorageSize for locations, $trigramsStorageSize for trigrams)"
    )
    val b = ByteBuffer.allocate(totalSize)

    b.putInt(Magic)
    b.putInt(Version)

    progress.info(s"writing documents index (${idBytes.length} entries)")
    b.putInt(idBytes.length)
    idBytes.foreach: bytes =>
      b.putInt(bytes.length)
      b.put(bytes)

    progress.info(
      s"writing locations index (${index.locationLine.length} entries)"
    )
    b.putInt(index.locationLine.length)
    for location <- index.locationLine.indices do
      b.putInt(index.locationDocument(location))
      b.putInt(index.locationLine(location))

    progress.info(
      s"writing [trigram -> set of locations] mapping (${index.postings.size} entries)"
    )
    b.putInt(index.postings.size)
    index.postings.foreach: (trigram, locations) =>
      b.putChar(trigram._1)
      b.putChar(trigram._2)
      b.putChar(trigram._3)
      b.putInt(locations.length)
      locations.foreach(b.putInt)

    assert(
      b.position == totalSize,
      s"expected: $totalSize, actual: ${b.position}"
    )

    b.array()
  end serialise
end TrigramIndexSerialiser
