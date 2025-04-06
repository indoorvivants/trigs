package trigs

class TrigramIndexMutableBuilder private (
    private var maxTrigramId: Int,
    private var maxLocationId: Int,
    trigrams: collection.mutable.Map[Int, Trigram],
    locations: collection.mutable.Map[Int, Location],
    index: collection.mutable.Map[Int, Set[Int]]
):

  private val reverseTrigrams  = trigrams.map(_.swap)
  private val reverseLocations = locations.map(_.swap)

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
      trigram: Trigram,
      locations: Set[Location]
  ): this.type =
    addTrigram(trigram)
    locations.foreach(addLocation(_))
    val trigramId = reverseTrigrams(trigram)
    index.update(trigramId, index(trigramId) ++ locations.map(reverseLocations))
    this

  def toIndex: TrigramIndex =
    TrigramIndex:
      index.toMap.map: (trigramId, locationIds) =>
        trigrams(trigramId) -> locationIds.map(locations(_))

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
      maxTrigramId = trigrams.keys.max,
      maxLocationId = locations.keys.max,
      trigrams = collection.mutable.Map.from(trigrams),
      locations = collection.mutable.Map.from(locations),
      index = collection.mutable.Map.from(index).withDefault(_ => Set.empty)
    )
end TrigramIndexMutableBuilder
