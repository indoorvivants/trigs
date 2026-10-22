package trigs

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.util.zip.ZipEntry
import java.util.zip.ZipInputStream
import java.util.zip.ZipOutputStream

class IndexBuilderTest         extends IndexBuilderSuite(parallelism = 1)
class ParallelIndexBuilderTest extends IndexBuilderSuite(parallelism = 4)

/** Behaviour that must not depend on parallelism */
abstract class IndexBuilderSuite(parallelism: Int) extends munit.FunSuite:
  private def newBuilder[Id]() = IndexBuilder[Id](parallelism)

  test("hits point at 1-based lines, skipping lines without trigrams") {
    val index = newBuilder[String]()
      .add("a", "first line\n\n  \nfoo bar\r\nbar baz\rlast foo")
      .result()

    assertEquals(
      index.search("foo", 10).map(h => h.id -> h.line).toSet,
      Set("a" -> 4, "a" -> 6)
    )
    assertEquals(index.search("baz", 10).map(_.line), List(5))
  }

  test("InputStream: UTF-8, read to the end but not closed") {
    var closed = false
    val in = new ByteArrayInputStream("über straße\nnaïve café".getBytes("UTF-8")):
      override def close(): Unit = closed = true

    val index = newBuilder[Int]().add(1, in).result()

    assertEquals(index.search("straße", 10).map(_.line), List(1))
    assertEquals(index.search("café", 10).map(_.line), List(2))
    assertEquals(in.available(), 0)
    assert(!closed)
  }

  test("indexes every entry of a ZipInputStream") {
    val zip = ByteArrayOutputStream()
    val out = ZipOutputStream(zip)
    for (name, text) <- List(
        "a/One.scala"  -> "object One:\n  def search = 1",
        "b/Two.scala"  -> "object Two",
        "c/Three.scala" -> "// searching\nobject Three"
      )
    do
      out.putNextEntry(ZipEntry(name))
      out.write(text.getBytes("UTF-8"))
      out.closeEntry()
    out.close()

    val builder = newBuilder[String]()
    val zis     = ZipInputStream(ByteArrayInputStream(zip.toByteArray))
    Iterator
      .continually(zis.getNextEntry())
      .takeWhile(_ != null)
      .foreach(entry => builder.add(entry.getName, zis))
    zis.close()

    val index = builder.result()
    assertEquals(index.ids.toSet, Set("a/One.scala", "b/Two.scala", "c/Three.scala"))
    assertEquals(
      index.search("search", 10).map(h => h.id -> h.line).toSet,
      Set("a/One.scala" -> 2, "c/Three.scala" -> 1)
    )
  }

  test("adding an id again replaces it; remove drops it") {
    val builder = newBuilder[String]()
      .add("a", "old text")
      .add("b", "other text")
      .add("a", "new text")

    val index = builder.result()
    assertEquals(index.search("old", 10), Nil)
    assertEquals(index.search("new", 10).map(_.id), List("a"))
    assertEquals(index.ids.toSet, Set("a", "b"))

    builder.remove("b").remove("missing")
    val removed = builder.result()
    assertEquals(removed.search("other", 10), Nil)
    assertEquals(removed.ids, List("a"))

    // earlier results aren't affected by later changes
    assertEquals(index.search("other", 10).map(_.id), List("b"))
  }

  test("a failed read keeps the previous document") {
    val failing = new InputStream:
      private var sent = false
      def read(): Int =
        if sent then throw java.io.IOException("broken")
        sent = true
        'x'

    val builder = newBuilder[String]().add("a", "original text")
    intercept[java.io.IOException](builder.add("a", failing))

    assertEquals(builder.result().search("original", 10).map(_.id), List("a"))
  }

  test("repeated trigrams on a line are stored once") {
    val index = newBuilder[String]().add("a", "abcabc").result()
    assertEquals(index.entries(trigram"abc"), Set("a" -> 1))
    assertEquals(index.search("abcabc", 10).map(_.score), List(1.0f))
  }

  test("merge: later indexes win for the same id") {
    val first  = newBuilder[String]().add("a", "old text").add("b", "kept text").result()
    val second = newBuilder[String]().add("a", "new text").result()

    val merged = TrigramIndex.merge(Seq(first, second))

    assertEquals(merged.search("old", 10), Nil)
    assertEquals(merged.search("new", 10).map(_.id), List("a"))
    assertEquals(merged.search("kept", 10).map(_.id), List("b"))
    assertEquals(merged.ids.toSet, Set("a", "b"))
  }

  test("empty index and queries") {
    val index = newBuilder[String]().add("a", "some text").result()

    assertEquals(TrigramIndex.empty[String].search("text", 10), Nil)
    assertEquals(newBuilder[String]().result().search("text", 10), Nil)
    assertEquals(index.search("", 10), Nil)
    assertEquals(index.search("text", 0), Nil)
  }
end IndexBuilderSuite
