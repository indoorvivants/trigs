package trigs

case class RankingConfig(
    /** How much a line's length (in distinct trigrams) affects its score, 0..1.
      * At 0 only (IDF-weighted) query coverage matters, at 1 a line twice as
      * long as the query scores half as much.
      */
    lengthWeight: Float = 0.25f,

    /** Boosts added to the trigram score during text verification */
    exactMatchBoost: Float = 1.0f,
    wordStartBoost: Float = 0.2f,
    wholeWordBoost: Float = 0.2f,
    caseMatchBoost: Float = 0.1f,
    allWordsBoost: Float = 0.5f,
    proximityBoost: Float = 0.3f,

    /** Penalty for indentation, reached at maxIndent leading whitespace chars,
      * so top-level definitions edge out deeply nested uses
      */
    indentPenalty: Float = 0.05f,
    maxIndent: Int = 16,

    /** How many trigram-ranked candidates to verify against line text:
      * `limit * candidatePoolFactor`, but at least `minCandidatePool`
      */
    candidatePoolFactor: Int = 10,
    minCandidatePool: Int = 50
)

object RankingConfig:
  val default = RankingConfig()

class Ranking(val config: RankingConfig):
  import config.*

  def candidatePool(limit: Int): Int =
    (limit * candidatePoolFactor).max(minCandidatePool)

  /** Scores every location containing at least one query trigram, in [0, 1].
    * `postings` must not contain duplicates, and its `size` should be cheap.
    *
    * score = coverage * (1 - lengthWeight + lengthWeight * precision), where
    * coverage is the IDF-weighted share of query trigrams found on the line,
    * and precision is the share of the line's trigrams matched by the query.
    */
  def score[L](
      query: Array[Trigram],
      postings: Trigram => Iterable[L],
      trigramCount: L => Int,
      totalLocations: Int
  ): List[(L, Float)] =
    val distinct = query.distinct
    if distinct.isEmpty then Nil
    else
      val n       = totalLocations.max(1).toFloat
      val weights = collection.mutable.Map.empty[L, Float].withDefaultValue(0f)
      val matched = collection.mutable.Map.empty[L, Int].withDefaultValue(0)
      var totalWeight = 0f

      distinct.foreach: trigram =>
        val locs = postings(trigram)
        // trigrams missing from the index get maximum weight: they lower
        // every score equally, but keep scores comparable across queries
        val idf = math.log(1 + n / locs.size.max(1)).toFloat
        totalWeight += idf
        locs.foreach: loc =>
          weights(loc) += idf
          matched(loc) += 1

      weights.toList
        .map: (loc, weight) =>
          val coverage  = weight / totalWeight
          val precision =
            (matched(loc).toFloat / trigramCount(loc).max(1)).min(1f)
          loc -> coverage * (1 - lengthWeight + lengthWeight * precision)
        .sortBy(-_._2)
    end if
  end score

  /** Re-orders trigram-ranked candidates by checking the query against the
    * actual line text. Candidates whose text is unavailable keep their score.
    */
  def rerank[L](
      query: String,
      candidates: List[(L, Float)],
      lineText: L => Option[String]
  ): List[(L, Float)] =
    val q = query.trim
    if q.isEmpty then candidates
    else
      candidates
        .map: (loc, score) =>
          loc -> lineText(loc).fold(score)(score + textBoost(q, _))
        .sortBy(-_._2)
  end rerank

  private[trigs] def textBoost(query: String, line: String): Float =
    val queryLower = query.toLowerCase
    val lineLower  = line.toLowerCase
    var boost      = 0f

    if lineLower.contains(queryLower) then
      boost += exactMatchBoost
      if line.contains(query) then boost += caseMatchBoost
      bestBoundary(lineLower, queryLower) match
        case Boundary.WholeWord => boost += wordStartBoost + wholeWordBoost
        case Boundary.WordStart => boost += wordStartBoost
        case Boundary.None      =>

    val words = queryLower.split("\\s+").filter(_.nonEmpty).distinct
    if words.length > 1 then
      boost += allWordsBoost * words.count(lineLower.contains) / words.length
      boost += proximityBoost * proximity(lineLower, words)

    boost -= indentPenalty * line.prefixLength(_.isWhitespace).min(maxIndent) /
      maxIndent

    boost
  end textBoost

  /** How tightly packed the query words are on the line, in (0, 1]: total
    * length of the words divided by the shortest span containing all of them. 0
    * if any word is missing.
    */
  private[trigs] def proximity(text: String, words: Array[String]): Float =
    // (start, end, word index) of every occurrence, ordered by start
    val occurrences =
      words.zipWithIndex
        .flatMap: (word, idx) =>
          Iterator
            .iterate(text.indexOf(word))(i => text.indexOf(word, i + 1))
            .takeWhile(_ >= 0)
            .map(start => (start, start + word.length, idx))
        .sortBy(_._1)

    val seen     = Array.fill(words.length)(0)
    var covered  = 0
    var left     = 0
    var shortest = Int.MaxValue

    // sliding window over occurrences, shrunk while it still covers all words
    occurrences.indices.foreach: right =>
      val idx = occurrences(right)._3
      if seen(idx) == 0 then covered += 1
      seen(idx) += 1

      while covered == words.length do
        val end =
          (left to right).iterator.map(occurrences(_)._2).max
        shortest = shortest.min(end - occurrences(left)._1)

        val leftIdx = occurrences(left)._3
        seen(leftIdx) -= 1
        if seen(leftIdx) == 0 then covered -= 1
        left += 1
      end while

    if shortest == Int.MaxValue then 0f
    else (words.map(_.length).sum.toFloat / shortest).min(1f)
  end proximity

  private enum Boundary:
    case None, WordStart, WholeWord

  private def isWordChar(c: Char) = c.isLetterOrDigit || c == '_'

  /** Best word alignment among all occurrences of `query` in `text` */
  private def bestBoundary(text: String, query: String): Boundary =
    var best = Boundary.None
    var from = text.indexOf(query)
    while from >= 0 && best != Boundary.WholeWord do
      val end        = from + query.length
      val startsWord =
        from == 0 || !isWordChar(text(from - 1)) || !isWordChar(query.head)
      val endsWord =
        end == text.length || !isWordChar(text(end)) || !isWordChar(query.last)

      if startsWord && endsWord then best = Boundary.WholeWord
      else if startsWord then best = Boundary.WordStart

      from = text.indexOf(query, from + 1)
    end while
    best
  end bestBoundary
end Ranking

object Ranking extends Ranking(RankingConfig.default)
