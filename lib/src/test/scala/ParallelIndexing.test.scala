package trigs

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

class ParallelIndexingTest extends munit.FunSuite:
  private val files = FunFixture[Seq[Path]](
    setup = _ =>
      val dir = Files.createTempDirectory("trigs-test")
      (1 to 20).map: i =>
        Files.writeString(
          dir.resolve(s"file$i.scala"),
          (1 to 30)
            .map(line => s"val value${i * line} = search(${i}abc, line$line)")
            .mkString("\n")
        )
    ,
    teardown = files =>
      files.foreach(Files.delete)
      Files.delete(files.head.getParent)
  )

  private def sequential(files: Seq[Path]) =
    val builder = IndexBuilder[Path]()
    files.foreach(file => builder.add(file, Files.readString(file)))
    builder.result()

  // documents can end up in a different order, so compare contents
  private def assertSame[Id](
      obtained: TrigramIndex[Id],
      expected: TrigramIndex[Id],
      clue: String
  ) =
    assertEquals(obtained.entries, expected.entries, clue)
    assertEquals(obtained.ids.toSet, expected.ids.toSet, clue)

  files.test("addLazy matches sequential indexing at any parallelism") {
    files =>
      val expected = sequential(files)

      for parallelism <- List(1, 3, 8) do
        val indexed = ConcurrentLinkedQueue[Path]()
        val builder = IndexBuilder[Path](parallelism, onIndexed = indexed.add)
        files.foreach(file =>
          builder.addLazy(file, () => Files.newInputStream(file))
        )

        assertSame(builder.result(), expected, s"parallelism = $parallelism")
        assertEquals(indexed.asScala.toSet, files.toSet)
  }

  test("the last add or remove of an id wins across workers") {
    val builder = IndexBuilder[String](parallelism = 8)
    for i <- 1 to 200 do
      builder.add("a", f"version$i%04d")
      builder.add(s"other$i", "filler")
    builder.remove("other7")
    builder.add("other7", "returned")
    builder.remove("other9")

    val index = builder.result()
    assertEquals(index.search("version0200", 1).map(_.id), List("a"))
    // no trace of earlier versions: only the surviving texts' trigrams remain
    val tokenizer = TrigramIndexer()
    assertEquals(
      index.entries.keySet,
      List("version0200", "filler", "returned").flatMap(tokenizer.index).toSet
    )
    assertEquals(index.search("returned", 5).map(_.id), List("other7"))
    assert(!index.ids.contains("other9"))
    assertEquals(index.ids.size, 200) // "a" and other1..200 minus other9
  }

  test("failures go to onError and keep the previous document") {
    for parallelism <- List(1, 4) do
      val errors  = ConcurrentLinkedQueue[String]()
      val builder = IndexBuilder[String](
        parallelism,
        onError = Some((id, _) => errors.add(id))
      )
      builder.add("a", "original text")
      builder.addLazy("a", () => throw java.io.IOException("can't open"))
      builder.addLazy("b", () => throw java.io.IOException("can't open"))

      val index = builder.result()
      assertEquals(
        errors.asScala.toSet,
        Set("a", "b"),
        s"parallelism = $parallelism"
      )
      assertEquals(index.search("original", 1).map(_.id), List("a"))
      assertEquals(index.ids, List("a"))
  }

  test("without onError, result() throws failures once") {
    for parallelism <- List(1, 4) do
      val builder = IndexBuilder[String](parallelism)
        .add("ok", "fine text")
        .addLazy("x", () => throw java.io.IOException("x"))
        .addLazy("y", () => throw java.io.IOException("y"))

      val exc = intercept[IndexingFailedException](builder.result())
      assertEquals(exc.failures.map(_._1).toSet, Set[Any]("x", "y"))
      assertEquals(exc.getSuppressed.length, 1)

      // reported once; the failed documents are simply absent afterwards
      assertEquals(builder.result().ids, List("ok"))
  }

  test("the builder keeps working after result()") {
    val builder = IndexBuilder[String](parallelism = 4)
    builder.add("a", "first text")
    val first = builder.result()

    builder.add("b", "second text").add("a", "replaced")
    val second = builder.result()

    assertEquals(first.ids.toSet, Set("a"))
    assertEquals(first.search("first", 1).map(_.id), List("a"))
    assertEquals(second.ids.toSet, Set("a", "b"))
    assertEquals(second.search("first", 1), Nil)
    assertEquals(second.search("replaced", 1).map(_.id), List("a"))
  }

  test("rejects parallelism below 1") {
    intercept[IllegalArgumentException](IndexBuilder[String](parallelism = 0))
  }
end ParallelIndexingTest
