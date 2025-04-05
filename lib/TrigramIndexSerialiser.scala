package trigs
import java.nio.ByteBuffer
object TrigramIndexSerialiser:
  def serialise(nice: Map[TRIGRAM, Set[Location]])(using
      Progress
  ): Array[Byte] =
    // val sorted = nice.toList.sortBy(_._1)
    progress.info("building locations index")
    val locationsIndex =
      val builder = collection.mutable.Map.empty[Location, Int]
      nice.valuesIterator
        .flatMap(_.toIterator)
        .foreach: loc =>
          builder.getOrElseUpdate(loc, builder.size)
      builder.toMap
    end locationsIndex

    progress.info("building trigrams ids index")
    val trigramIds =
      val builder = collection.mutable.Map.empty[TRIGRAM, Int]
      nice.keysIterator.foreach: trigram =>
        builder.getOrElseUpdate(trigram, builder.size)
      builder.toMap
    end trigramIds

    progress.info("calculating locations index size estimate")
    val locationsIndexSizeEstimate = locationsIndex.map { (loc, _) =>
      val fileNameSize        = loc.file.toString.getBytes().length
      val fileNameFieldLength = sizeOf[Int] // Int?
      val idLength            = sizeOf[Int]
      val lineLength          = sizeOf[Int]
      fileNameSize + fileNameFieldLength + idLength + lineLength
    }.sum

    val locationCountSize = sizeOf[Int]

    val trigramsSizeEstimate = trigramIds
      .map: _ =>
        2 * 3 + 4 + 4
      .sum

    val trigramCountSize = sizeOf[Int]

    val indexSizeEstimate =
      nice.map((_, locs) => 4 + 4 + locs.size * sizeOf[Int]).sum

    progress.info("allocating buffer for serialised index")
    val b = ByteBuffer.allocate(
      locationCountSize + locationsIndexSizeEstimate + trigramCountSize + trigramsSizeEstimate + indexSizeEstimate
    )

    b.putInt(locationsIndex.size)

    progress.info(s"writing locations index (${locationsIndex.size} entries)")
    locationsIndex.foreach: (location, id) =>
      val loc = location.file.toString
      b.putInt(id)
      b.putInt(location.line)
      b.putInt(loc.length)
      b.put(loc.getBytes())

    b.putInt(trigramIds.size)

    progress.info(s"writing trigrams index (${trigramIds.size} entries)")
    trigramIds.foreach: (trigram, id) =>
      b.putInt(id)
      b.putChar(trigram._1)
      b.putChar(trigram._2)
      b.putChar(trigram._3)

    b.putInt(nice.size)

    progress.info(
      s"writing [trigram -> set of locations] mapping (${nice.size} entries)"
    )
    nice.foreach: (trigram, locations) =>
      b.putInt(trigramIds(trigram))
      b.putInt(locations.size)
      locations.foreach: loc =>
        b.putInt(locationsIndex(loc))

    b.array()
  end serialise
end TrigramIndexSerialiser
