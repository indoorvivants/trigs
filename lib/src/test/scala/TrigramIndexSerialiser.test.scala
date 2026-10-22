package trigs

import java.io.ByteArrayInputStream

class TrigramIndexSerialiserTest extends munit.FunSuite:
  given Progress = Progress.Quiet

  private def roundtrip[Id: IdCodec](index: TrigramIndex[Id]) =
    TrigramIndex.deserialise[Id](
      ByteArrayInputStream(TrigramIndexSerialiser.serialise(index))
    )

  private def assertSame[Id](obtained: TrigramIndex[Id], expected: TrigramIndex[Id]) =
    assertEquals(obtained.entries, expected.entries)
    assertEquals(obtained.ids, expected.ids)

  test("empty") {
    val empty = TrigramIndex.empty[String]
    assertSame(roundtrip(empty), empty)
  }

  test("string ids, including documents without indexed lines") {
    val index = IndexBuilder[String]()
      .add("hello", "hello world\nwor ld hel")
      .add("blank", "  \n")
      .result()

    assertSame(roundtrip(index), index)
  }

  test("many lines per document, non-ASCII ids, each id written once") {
    val path  = "/päth/with spaces/Файл.scala"
    val index = IndexBuilder[String]()
      .add(path, (1 to 50).map(i => f"abc line$i%03d").mkString("\n"))
      .add("/other", "\n\ndef")
      .result()

    val serialised = TrigramIndexSerialiser.serialise(index)
    val reloaded =
      TrigramIndex.deserialise[String](ByteArrayInputStream(serialised))

    assertSame(reloaded, index)
    assertEquals(
      reloaded.search("line007", 1).map(h => h.id -> h.line),
      List(path -> 7)
    )

    // the path is written once, not once per line
    val pathBytes = path.getBytes("UTF-8")
    val occurrences =
      serialised.indices.count(i => serialised.startsWith(pathBytes, i))
    assertEquals(occurrences, 1)
  }

  test("custom codecs") {
    given IdCodec[Int] = summon[IdCodec[String]].imap(_.toInt)(_.toString)
    val index          = IndexBuilder[Int]().add(42, "answer").result()

    assertEquals(roundtrip(index).search("answer", 1).map(_.id), List(42))
  }

  test("a loaded index can be extended by merging") {
    val loaded = roundtrip(IndexBuilder[String]().add("a", "abc").result())
    val more   = IndexBuilder[String]().add("b", "xyz").result()

    assertEquals(
      TrigramIndex.merge(Seq(loaded, more)).entries,
      Map(trigram"abc" -> Set("a" -> 1), trigram"xyz" -> Set("b" -> 1))
    )
  }

  test("rejects data in another format") {
    val notAnIndex = java.nio.ByteBuffer.allocate(8).putInt(1).putInt(2).array()

    intercept[IllegalArgumentException](
      TrigramIndex.deserialise[String](ByteArrayInputStream(notAnIndex))
    )
  }
end TrigramIndexSerialiserTest
