package trigs

class TrigramIndexMutableBuilderTest extends munit.FunSuite:
  test("add") {
    val builder = TrigramIndexMutableBuilder.empty

    builder.addTrigram(trigram"abc")
    builder.addLocation(loc("/a/b"))
    builder.addLocation(loc("/c/d"))

    builder.addOccurrence(trigram"abc", loc("/a/b"))
    builder.addOccurrence(trigram"abc", loc("/c/d"))
    builder.addOccurrence(trigram"abc", loc("/d/e"))

    builder.addOccurrences(trigram"def", Set(loc("/f/g"), loc("/g/h")))

    val index = builder.toIndex

    assertEquals(
      index.locs(trigram"abc"),
      Set(loc("/a/b"), loc("/c/d"), loc("/d/e"))
    )
    assertEquals(
      index.locs(trigram"def"),
      Set(loc("/f/g"), loc("/g/h"))
    )
  }

  private val locs = collection.mutable.Map.empty[String, Int]

  private def loc(s: String) =
    Location(os.Path(s), locs.getOrElseUpdate(s, locs.size))
end TrigramIndexMutableBuilderTest
