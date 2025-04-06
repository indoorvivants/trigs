import trigs.*

class IndexEntryTest extends munit.FunSuite:
  test("IndexEntry should be created with correct values") {
    val entry = IndexFileEntry(
      Array(Trigram('a', 'b', 'c')),
      Location(os.pwd / "Test.scala", 1)
    )
  }
