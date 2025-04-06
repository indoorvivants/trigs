package trigs

trait Searchable:
  protected def indexer: TrigramIndexer
  def search(trigrams: Array[Trigram]): List[(Location, Float)]

  def search(query: String): List[(Location, Float)] =
    search(indexer.index(query))

class TrigramIndexMutableBuilder private (
    private var maxTrigramId: Int,
    private var maxLocationId: Int,
    trigrams: collection.mutable.Map[Int, Trigram],
    locations: collection.mutable.Map[Int, Location],
    index: collection.mutable.Map[Int, Set[Int]]
) extends Searchable:

  private val reverseTrigrams  = trigrams.map(_.swap)
  private val reverseLocations = locations.map(_.swap)

  override lazy val indexer = TrigramIndexer()

  override def search(trigrams: Array[Trigram]): List[(Location, Float)] =
    val res = collection.mutable.Map.empty[Int, Int].withDefaultValue(0)

    trigrams.foreach: token =>
      if reverseTrigrams.contains(token) then
        val trigramId = reverseTrigrams(token)
        index(trigramId).foreach: locId =>
          res(locId) += 1

    res.toList
      .map((l, cnt) => l -> cnt.toFloat / trigrams.length.toFloat)
      .sortBy(_._2 * -1)
      .map((l, cnt) => locations(l) -> cnt)
  end search

  inline def addTrigram(trigram: Trigram): this.type =
    if reverseTrigrams.contains(trigram) then this
    else
      maxTrigramId += 1
      trigrams += maxTrigramId   -> trigram
      reverseTrigrams += trigram -> maxTrigramId
      this

  inline def addLocation(location: Location): this.type =
    if reverseLocations.contains(location) then this
    else
      maxLocationId += 1
      locations += maxLocationId   -> location
      reverseLocations += location -> maxLocationId
      this

  inline def deleteLocation(location: Location): this.type =
    if reverseLocations.contains(location) then
      val locationId = reverseLocations(location)
      locations -= locationId
      reverseLocations -= location
      index.foreach: (trigramId, locationIds) =>
        index.update(trigramId, locationIds - locationId)

      this
    else this

  inline def deleteFile(path: os.Path): this.type =
    val (locs, ids) = reverseLocations.filter(_._1.file == path).unzip
    locations --= ids
    reverseLocations --= locs

    index.foreach: (trigramId, locs) =>
      index.update(trigramId, locs -- ids)

    this
  end deleteFile

  inline def addOccurrence(trigram: Trigram, location: Location): this.type =
    addTrigram(trigram)
    addLocation(location)
    val trigramId  = reverseTrigrams(trigram)
    val locationId = reverseLocations(location)
    index.update(
      trigramId,
      index(trigramId) + locationId
    )
    this
  end addOccurrence

  inline def addOccurrences(
      trigrams: Set[Trigram],
      locations: Set[Location]
  ): this.type =
    trigrams.foreach(addTrigram(_))
    locations.foreach(addLocation(_))
    val trigramIds  = trigrams.map(reverseTrigrams)
    val locationIds = locations.map(reverseLocations)
    trigramIds.foreach: trigramId =>
      index.update(trigramId, index(trigramId) ++ locationIds)
    this
  end addOccurrences

  def toIndex: TrigramIndex =
    TrigramIndex:
      index.toMap.map: (trigramId, locationIds) =>
        trigrams(trigramId) -> locationIds.map(locations(_))

  def toSearchable: Searchable = this

  def addEntry(entry: IndexFileEntry) =
    addOccurrences(entry.tokens.toSet, Set(entry.location))

end TrigramIndexMutableBuilder

object TrigramIndexMutableBuilder:
  def empty = new TrigramIndexMutableBuilder(
    maxTrigramId = 0,
    maxLocationId = 0,
    trigrams = collection.mutable.Map.empty[Int, Trigram],
    locations = collection.mutable.Map.empty[Int, Location],
    index = collection.mutable.Map.empty
      .withDefault(_ => Set.empty)
  )

  def from(
      trigrams: Map[Int, Trigram],
      locations: Map[Int, Location],
      index: Map[Int, Set[Int]]
  ): TrigramIndexMutableBuilder =
    new TrigramIndexMutableBuilder(
      maxTrigramId = trigrams.keys.maxOption.getOrElse(0),
      maxLocationId = locations.keys.maxOption.getOrElse(0),
      trigrams = collection.mutable.Map.from(trigrams),
      locations = collection.mutable.Map.from(locations),
      index = collection.mutable.Map.from(index).withDefault(_ => Set.empty)
    )
end TrigramIndexMutableBuilder
