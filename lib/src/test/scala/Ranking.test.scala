package trigs

class RankingTest extends munit.FunSuite:
  given Progress = Progress.Quiet

  /** An index with a single document "test" made of `lines` */
  private def build(lines: String*): Index =
    Index(
      IndexBuilder[String]().add("test", lines.mkString("\n")).result(),
      lines
    )

  private case class Index(index: TrigramIndex[String], lines: Seq[String]):
    def ranked(query: String): Seq[Int] =
      index.search(query, 100).map(_.line)
    def scores(query: String): Seq[Float] =
      index.search(query, 100).map(_.score)
    def reranked(query: String): Seq[Int] =
      index.search(query, 100, (_, line) => lines.lift(line - 1)).map(_.line)

  test("rare trigrams outweigh common ones") {
    val index = build("abc", "xyz", "abc", "abc", "abc")

    // "xyz" occurs once, "abc" four times: the rare match must win
    assertEquals(index.ranked("abcxyz").head, 2)
  }

  test("shorter lines win when coverage is equal") {
    val index = build(
      "val searchable = something else entirely here",
      "searchable"
    )

    assertEquals(index.ranked("searchable"), List(2, 1))
  }

  test(
    "exact line match scores 1, duplicate query trigrams don't count twice"
  ) {
    assertEquals(build("abcabc").scores("abcabc"), List(1.0f))
  }

  test("line counts survive removal, merging and serialisation") {
    val builder = IndexBuilder[String]()
      .add("removed", "searchable")
      .add("kept", "searchable plus noise words")
      .add("other", "searchable here")
      .remove("removed")
    val index = builder.result()

    val merged = TrigramIndex.merge(
      Seq(
        IndexBuilder[String]()
          .add("kept", "searchable plus noise words")
          .result(),
        IndexBuilder[String]().add("other", "searchable here").result()
      )
    )
    val reloaded = TrigramIndex.deserialise[String](
      java.io.ByteArrayInputStream(TrigramIndexSerialiser.serialise(index))
    )

    val expected = index.search("searchable", 10)
    assertEquals(expected.map(_.id), List("other", "kept"))
    assertEquals(merged.search("searchable", 10), expected)
    assertEquals(reloaded.search("searchable", 10), expected)
  }

  test("text verification reorders candidates") {
    val index = build(
      "sear arch", // every trigram of "search", but not the word
      "def research()",
      "def search()"
    )

    // trigram score alone prefers the scattered trigrams on the short line
    assertEquals(index.ranked("search").head, 1)

    // exact match beats scattered trigrams, whole word beats a substring
    assertEquals(index.reranked("search"), List(3, 2, 1))
  }

  test("textBoost") {
    import Ranking.textBoost
    import Ranking.config.*
    def check(query: String, line: String, expected: Float) =
      assertEqualsFloat(textBoost(query, line), expected, 1e-5f, line)

    check("search", "nothing", 0f)
    check("search", "research", exactMatchBoost + caseMatchBoost)
    check(
      "search",
      "searchable",
      exactMatchBoost + caseMatchBoost + wordStartBoost
    )
    check(
      "search",
      "x.Search(y)",
      exactMatchBoost + wordStartBoost + wholeWordBoost
    )
    // multi-word queries: share of words present, phrase not required,
    // closer words score higher
    check(
      "file index",
      "index the file",
      allWordsBoost + proximityBoost * 9 / 14
    )
    check("file index", "index only", allWordsBoost / 2)
    // indentation is penalised up to maxIndent
    val wholeWord =
      exactMatchBoost + caseMatchBoost + wordStartBoost + wholeWordBoost
    check("search", "    search", wholeWord - indentPenalty * 4 / maxIndent)
    check("search", " " * 40 + "search", wholeWord - indentPenalty)
  }

  test("custom config") {
    val ranking = Ranking(
      RankingConfig(exactMatchBoost = 5f, caseMatchBoost = 0f)
    )
    assertEqualsFloat(ranking.textBoost("search", "research"), 5f, 1e-5f)
    assertEquals(ranking.candidatePool(10), 100)
    assertEquals(
      Ranking(RankingConfig(minCandidatePool = 500)).candidatePool(10),
      500
    )
  }

  test("proximity") {
    import Ranking.proximity
    def check(text: String, words: List[String], expected: Float) =
      assertEqualsFloat(proximity(text, words.toArray), expected, 1e-5f, text)

    check("file index", List("file", "index"), 9f / 10)
    check("index file", List("file", "index"), 9f / 10)
    check("file is not an index", List("file", "index"), 9f / 20)
    // picks the tightest pair among repeated occurrences
    check("file ... index file", List("file", "index"), 9f / 10)
    check("file only", List("file", "index"), 0f)
  }

  test("closer words rank higher") {
    val index = build(
      "read the file and then rebuild the index",
      "rebuild index file"
    )

    assertEquals(index.reranked("index file"), List(2, 1))
  }

  test("less indented lines win ties") {
    val index = build(
      "        def search()",
      "def search()"
    )

    assertEquals(index.reranked("search"), List(2, 1))
  }

  test("withRanking changes the ranking used by search") {
    val index = build("def research()", "def search()")
    val lines = (_: String, line: Int) =>
      Seq("def research()", "def search()").lift(line - 1)

    // without word-boundary boosts or length weighting, both lines contain
    // "search" and score the same
    val flat = index.index.withRanking(
      RankingConfig(wordStartBoost = 0, wholeWordBoost = 0, lengthWeight = 0)
    )

    assertEquals(
      index.index.search("search", 10, lines).map(_.line),
      List(2, 1)
    )
    assertEquals(
      flat.search("search", 10, lines).map(_.score).distinct.size,
      1
    )
  }
end RankingTest
