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

    builder.addOccurrences(Set(trigram"def"), Set(loc("/f/g"), loc("/g/h")))

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

  test("delete") {
    val builder = TrigramIndexMutableBuilder.empty

    builder.addOccurrence(trigram"abc", loc("/a/b"))
    builder.addOccurrence(trigram"abc", loc("/c/d"))
    builder.addOccurrence(trigram"abc", loc("/d/e"))

    builder.addOccurrences(Set(trigram"def"), Set(loc("/f/g"), loc("/g/h")))

    assertEquals(
      builder.toIndex.locs(trigram"abc"),
      Set(loc("/a/b"), loc("/c/d"), loc("/d/e"))
    )
    assertEquals(
      builder.toIndex.locs(trigram"def"),
      Set(loc("/f/g"), loc("/g/h"))
    )

    builder.deleteLocation(loc("/a/b"))

    assertEquals(
      builder.toIndex.locs(trigram"abc"),
      Set(loc("/c/d"), loc("/d/e"))
    )
  }

  test("delete file") {
    val builder = TrigramIndexMutableBuilder.empty

    builder.addOccurrence(trigram"abc", loc("/a/b"))
    builder.addOccurrence(trigram"abc", loc("/c/d"))

    assertEquals(
      builder.toIndex.locs(trigram"abc"),
      Set(loc("/a/b"), loc("/c/d"))
    )

    builder.deleteFile(os.Path("/a/b"))

    assertEquals(
      builder.toIndex.locs(trigram"abc"),
      Set(loc("/c/d"))
    )
  }

  test("search") {
    val builder = TrigramIndexMutableBuilder.empty

    builder.addTrigram(trigram"abc")
    builder.addLocation(loc("/a/b"))
    builder.addLocation(loc("/c/d"))

    builder.addOccurrence(trigram"abc", loc("/a/b"))
    builder.addOccurrence(trigram"abc", loc("/c/d"))
    builder.addOccurrence(trigram"abc", loc("/d/e"))

    builder.addOccurrences(Set(trigram"def"), Set(loc("/f/g"), loc("/g/h")))

    val search = builder.toSearchable

    val allLocations = builder.toIndex.locs.values.reduce(_ ++ _)

    val both = search.search("abcdef")

    assertEquals(both.map(_._1).toSet, allLocations)

    assertEquals(
      search.search("abc").map(_._1).toSet,
      Set(loc("/a/b"), loc("/c/d"), loc("/d/e"))
    )

    assertEquals(
      search.search("def").map(_._1).toSet,
      Set(loc("/f/g"), loc("/g/h"))
    )

  }

  private val locs = collection.mutable.Map.empty[String, Int]

  private def loc(s: String) =
    Location(os.Path(s), locs.getOrElseUpdate(s, locs.size))
end TrigramIndexMutableBuilderTest
