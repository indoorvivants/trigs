package trigs
import java.nio.ByteBuffer
object TrigramIndexSerialiser:
  def serialise(nice: TrigramIndex)(using
      Progress
  ): Array[Byte] =
    progress.info("building locations index")
    val locationsIndex =
      val builder = collection.mutable.Map.empty[Location, Int]
      nice.locs.values
        .foreach: locs =>
          locs.foreach: loc =>
            builder.getOrElseUpdate(loc, builder.size)
      builder.toMap
    end locationsIndex

    progress.info("building trigrams ids index")
    val trigramIds =
      val builder = collection.mutable.Map.empty[Trigram, Int]
      nice.locs.keysIterator.foreach: trigram =>
        builder.getOrElseUpdate(trigram, builder.size)
      builder.toMap
    end trigramIds

    progress.info("calculating locations index size estimate")

    val trigramSize = sizeOf[Int] + // id
      3 * sizeOf[Char] // trigram itself

    val trigramsStorageSize = sizeOf[Int] + // count
      trigramSize * trigramIds.size

    val locationStorageSize =
      def locationSize(loc: Location) =
        sizeOf[Int] +   // id
          sizeOf[Int] + // line
          sizeOf[Int] + // length of file name
          loc.file.toString.getBytes().length

      sizeOf[Int] + // count
        locationsIndex.keys.toList.map(locationSize).sum
    end locationStorageSize

    val indexStorageSize = sizeOf[Int] + // count
      nice.locs.values
        .map: locs =>
          sizeOf[Int] +               // trigram id
            sizeOf[Int] +             // number of locations
            (locs.size * sizeOf[Int]) // location ids
        .sum

    progress.info(
      s"allocating buffer for serialised index ($locationStorageSize for locations, $indexStorageSize for index, $trigramsStorageSize for trigrams)"
    )
    val b = ByteBuffer.allocate(
      locationStorageSize +
        trigramsStorageSize +
        indexStorageSize
    )

    progress.info(s"writing locations index (${locationsIndex.size} entries)")
    b.putInt(locationsIndex.size)
    locationsIndex.foreach: (location, id) =>
      val loc      = location.file.toString
      val locBytes = loc.getBytes()
      b.putInt(id)
      b.putInt(location.line)
      b.putInt(locBytes.length)
      b.put(locBytes)

    assert(
      b.position == locationStorageSize,
      s"expected: $locationStorageSize, actual: ${b.position}"
    )

    progress.info(s"writing trigrams index (${trigramIds.size} entries)")
    b.putInt(trigramIds.size)
    trigramIds.foreach: (trigram, id) =>
      b.putInt(id)
      b.putChar(trigram._1)
      b.putChar(trigram._2)
      b.putChar(trigram._3)

    progress.info(
      s"writing [trigram -> set of locations] mapping (${nice.locs.size} entries)"
    )
    b.putInt(nice.locs.size)
    nice.locs.foreach: (trigram, locations) =>
      b.putInt(trigramIds(trigram))
      b.putInt(locations.size)
      locations.foreach: loc =>
        b.putInt(locationsIndex(loc))

    b.array()
  end serialise
end TrigramIndexSerialiser
