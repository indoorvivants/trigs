package trigs

import java.io.ByteArrayInputStream

class TrigramIndexSerialiserTest extends munit.FunSuite:
  given Progress = Progress.Printer
  test("empty") {
    val emptyIndex = TrigramIndexMutableBuilder.empty.toIndex
    val serialised =
      TrigramIndexSerialiser.serialise(emptyIndex)
    val deserialised =
      TrigramIndexDeserialiser
        .deserialiseIntoBuilder(new ByteArrayInputStream(serialised))
        .toIndex
    assertEquals(emptyIndex, deserialised)
  }
  test("non empty".only) {
    val indexBuilder = TrigramIndexMutableBuilder.empty

    indexBuilder.addOccurrence(trigram"hel", loc("/a/b"))
    indexBuilder.addOccurrences(
      Set(trigram"hel", trigram"wor"),
      Set(loc("/a/b"), loc("/c/d"))
    )

    val index = indexBuilder.toIndex

    val serialised =
      TrigramIndexSerialiser.serialise(index)(using Progress.Printer)
    val deserialised =
      TrigramIndexDeserialiser
        .deserialiseIntoBuilder(new ByteArrayInputStream(serialised))(using
          Progress.Quiet
        )
        .toIndex
    assertEquals(index, deserialised)
  }

  private val locs = collection.mutable.Map.empty[String, Int]

  private def loc(s: String) =
    Location(os.Path(s), locs.getOrElseUpdate(s, locs.size))
end TrigramIndexSerialiserTest
