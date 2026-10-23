package trigs

import java.io.InputStream
import java.nio.file.{Files, Path}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/** Immutable trigram index over the lines of documents identified by `Id`. Safe
  * to search from multiple threads. Build one with [[IndexBuilder]].
  *
  * Internally every indexed line is a location, numbered from 0; arrays below
  * are indexed by location, and postings hold sorted location numbers.
  */
final class TrigramIndex[Id] private[trigs] (
    private[trigs] val documents: IndexedSeq[Id],
    private[trigs] val locationDocument: Array[Int],
    private[trigs] val locationLine: Array[Int],
    /** Number of distinct trigrams on each line */
    private[trigs] val locationTrigrams: Array[Int],
    private[trigs] val postings: Map[Trigram, Array[Int]],
    ranking: Ranking = Ranking
):
  private val tokenizer = TrigramIndexer()

  /** Ids of all indexed documents */
  def ids: Seq[Id] = documents

  /** The same index, ranking results with `config` */
  def withRanking(config: RankingConfig): TrigramIndex[Id] =
    new TrigramIndex(
      documents,
      locationDocument,
      locationLine,
      locationTrigrams,
      postings,
      Ranking(config)
    )

  /** Top `limit` lines by trigram similarity to `query` */
  def search(query: String, limit: Int): Seq[Hit[Id]] =
    scored(query).take(limit).map(hit)

  /** Top `limit` lines, with trigram candidates re-ranked by checking the query
    * against the line text. `lineText(id, line)` returns the text of a 1-based
    * line, or None if it's unavailable.
    */
  def search(
      query: String,
      limit: Int,
      lineText: (Id, Int) => Option[String]
  ): Seq[Hit[Id]] =
    ranking
      .rerank(
        query,
        scored(query).take(ranking.candidatePool(limit)),
        location => lineText(idOf(location), locationLine(location))
      )
      .take(limit)
      .map(hit)

  override def toString(): String =
    s"TrigramIndex(${documents.length} documents, ${locationLine.length} lines, ${postings.size} trigrams)"

  /** Contents in a comparable form, for tests */
  private[trigs] def entries: Map[Trigram, Set[(Id, Int)]] =
    postings.map: (trigram, locations) =>
      trigram -> locations.iterator
        .map(location => idOf(location) -> locationLine(location))
        .toSet

  private def scored(query: String): List[(Int, Float)] =
    ranking.score(
      tokenizer.index(query),
      postings = postings.get(_).fold(Nil)(ArraySeq.unsafeWrapArray(_)),
      trigramCount = locationTrigrams(_),
      totalLocations = locationLine.length
    )

  private def idOf(location: Int): Id = documents(locationDocument(location))

  private def hit(location: Int, score: Float): Hit[Id] =
    Hit(idOf(location), locationLine(location), score)
end TrigramIndex

object TrigramIndex:
  def empty[Id]: TrigramIndex[Id] =
    new TrigramIndex(
      Vector.empty,
      Array.empty,
      Array.empty,
      Array.empty,
      Map.empty
    )

  def read[Id: IdCodec](path: Path)(using Progress): TrigramIndex[Id] =
    val in = Files.newInputStream(path)
    try deserialise(in)
    finally in.close()

  def deserialise[Id: IdCodec](in: InputStream)(using
      Progress
  ): TrigramIndex[Id] =
    TrigramIndexDeserialiser.deserialise(in)

  /** Combines indexes; if several contain the same id, the last one wins */
  def merge[Id](indexes: Seq[TrigramIndex[Id]]): TrigramIndex[Id] =
    val owner = mutable.HashMap.empty[Id, Int]
    indexes.zipWithIndex.foreach: (index, i) =>
      index.documents.foreach(owner(_) = i)

    compact(
      indexes.zipWithIndex.map: (index, i) =>
        index -> ((document: Int) => owner(index.documents(document)) == i)
    )
  end merge

  /** Concatenates the documents of each index for which `keep` holds,
    * renumbering documents and locations. Locations are renumbered in
    * increasing order, so concatenated postings stay sorted.
    */
  private[trigs] def compact[Id](
      parts: Seq[(TrigramIndex[Id], Int => Boolean)]
  ): TrigramIndex[Id] =
    val documents        = Vector.newBuilder[Id]
    var documentCount    = 0
    val locationDocument = IntBuffer(1024)
    val locationLine     = IntBuffer(1024)
    val locationTrigrams = IntBuffer(1024)
    val postings         = mutable.HashMap.empty[Trigram, IntBuffer]

    parts.foreach: (index, keep) =>
      val documentRemap = new Array[Int](index.documents.length)
      for document <- index.documents.indices do
        if keep(document) then
          documents += index.documents(document)
          documentRemap(document) = documentCount
          documentCount += 1
        else documentRemap(document) = -1

      val locationRemap = new Array[Int](index.locationLine.length)
      for location <- index.locationLine.indices do
        val document = documentRemap(index.locationDocument(location))
        if document < 0 then locationRemap(location) = -1
        else
          locationRemap(location) = locationLine.length
          locationDocument += document
          locationLine += index.locationLine(location)
          locationTrigrams += index.locationTrigrams(location)

      index.postings.foreach: (trigram, locations) =>
        var target: IntBuffer = null
        locations.foreach: location =>
          val renumbered = locationRemap(location)
          if renumbered >= 0 then
            if target == null then
              target = postings.getOrElseUpdate(trigram, IntBuffer())
            target += renumbered

    new TrigramIndex(
      documents.result(),
      locationDocument.toArray,
      locationLine.toArray,
      locationTrigrams.toArray,
      postings.iterator
        .map((trigram, locations) => trigram -> locations.toArray)
        .toMap
    )
  end compact
end TrigramIndex
