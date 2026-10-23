package trigs

import java.io.{
  ByteArrayInputStream,
  InputStream,
  InputStreamReader,
  Reader,
  StringReader
}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingQueue}
import scala.collection.mutable
import scala.util.control.NonFatal

/** Accumulates documents for a [[TrigramIndex]]. Each line of a document is
  * indexed separately, so search hits point at lines.
  *
  * For any id, the last `add` or `remove` call wins, regardless of
  * `parallelism`. The builder itself isn't thread-safe: call it from one thread
  * at a time.
  *
  * @param parallelism
  *   with more than 1, documents are indexed on that many daemon worker
  *   threads, started on the first add; `add` and `addLazy` return as soon as
  *   the document is queued
  * @param onIndexed
  *   called after each document is indexed, on the thread that indexed it
  * @param onError
  *   called when an [[addLazy]] document fails, on the thread that indexed it.
  *   Without a handler, failures are collected and thrown by [[result]] as an
  *   [[IndexingFailedException]]
  */
final class IndexBuilder[Id](
    parallelism: Int = 1,
    onIndexed: Id => Unit = (_: Id) => (),
    onError: Option[(Id, Throwable) => Unit] = None
):
  require(parallelism >= 1, s"parallelism must be at least 1, got $parallelism")

  import IndexBuilder.*

  private var nextSeq = 0L

  /** Sequence number of the last `remove` of each id */
  private val removedAt = mutable.HashMap.empty[Id, Long]

  /** Segments from workers that have been shut down by [[result]] */
  private val finished = mutable.ArrayBuffer.empty[Segment[Id]]

  /** Used when parallelism is 1 */
  private lazy val inline = SegmentBuilder[Id]()

  private var pool: Option[Pool] = None

  private val failures = ConcurrentLinkedQueue[(Id, Throwable)]()

  /** Indexes `text`, replacing any document previously added with `id` */
  def add(id: Id, text: String): this.type =
    val seq = takeSeq()
    if parallelism == 1 then
      inline.add(id, seq, StringReader(text))
      onIndexed(id)
    else workers.submit(Task(id, seq, Source.Text(text)))
    this
  end add

  /** Indexes the UTF-8 contents of `in`, replacing any document previously
    * added with `id`. Reads `in` to the end before returning, on the calling
    * thread, but doesn't close it - so it works with `ZipInputStream` entries.
    * Read errors are thrown, and keep a previous document with the same id.
    * Malformed UTF-8 is replaced with U+FFFD.
    */
  def add(id: Id, in: InputStream): this.type =
    if parallelism == 1 then
      inline.add(id, takeSeq(), InputStreamReader(in, UTF_8))
      onIndexed(id)
    else
      val bytes = in.readAllBytes()
      workers.submit(Task(id, takeSeq(), Source.Bytes(bytes)))
    this
  end add

  /** Like `add(id, in)`, but the stream is opened, read and closed by the
    * thread that indexes it, so with `parallelism` > 1 reading happens in
    * parallel too. Use it for files, or entries of a `java.util.zip.ZipFile`.
    * Failures go to `onError`, and keep a previous document with the same id.
    */
  def addLazy(id: Id, open: () => InputStream): this.type =
    val task = Task(id, takeSeq(), Source.Lazy(open))
    if parallelism == 1 then process(inline, task)
    else workers.submit(task)
    this

  def remove(id: Id): this.type =
    removedAt(id) = takeSeq()
    this

  /** Builds an index of the documents added so far, waiting for queued ones to
    * be indexed. The builder can still be used afterwards.
    *
    * @throws IndexingFailedException
    *   if [[addLazy]] documents failed and there's no `onError` handler; the
    *   failures are reported once, and left out of later results
    */
  def result(): TrigramIndex[Id] =
    pool.foreach(finished ++= _.shutdown())
    pool = None

    val failed =
      Iterator.continually(failures.poll()).takeWhile(_ != null).toList
    if failed.nonEmpty then throw IndexingFailedException(failed)

    val segments =
      if parallelism == 1 then finished.toVector :+ inline.snapshot()
      else finished.toVector

    // the latest successful add of each id
    val latest = mutable.HashMap.empty[Id, Long]
    for
      segment  <- segments
      document <- segment.succeeded
    do
      latest.updateWith(segment.index.documents(document)):
        case Some(seq) => Some(seq.max(segment.seqs(document)))
        case None      => Some(segment.seqs(document))

    def keep(segment: Segment[Id])(document: Int): Boolean =
      val id  = segment.index.documents(document)
      val seq = segment.seqs(document)
      segment.succeeded(document) &&
      latest(id) == seq &&
      removedAt.get(id).forall(_ < seq)

    TrigramIndex.compact(
      segments.map(segment => segment.index -> keep(segment))
    )
  end result

  private def takeSeq(): Long =
    val seq = nextSeq
    nextSeq += 1
    seq

  private def workers: Pool =
    pool.getOrElse:
      val started = Pool()
      pool = Some(started)
      started

  /** Indexes a task on the current thread, reporting the outcome */
  private def process(segment: SegmentBuilder[Id], task: Task[Id]): Unit =
    try
      task.source match
        case Source.Text(text) =>
          segment.add(task.id, task.seq, StringReader(text))
        case Source.Bytes(bytes) =>
          segment.add(task.id, task.seq, utf8(ByteArrayInputStream(bytes)))
        case Source.Lazy(open) =>
          val in = open()
          try segment.add(task.id, task.seq, utf8(in))
          finally in.close()
      onIndexed(task.id)
    catch case NonFatal(exc) => fail(task.id, exc)

  private def fail(id: Id, exc: Throwable): Unit =
    onError match
      case None          => failures.add(id -> exc)
      case Some(handler) =>
        try handler(id, exc)
        catch case NonFatal(handlerExc) => failures.add(id -> handlerExc)

  private def utf8(in: InputStream): Reader = InputStreamReader(in, UTF_8)

  /** Worker threads, each indexing into its own segment */
  private class Pool:
    // None tells a worker there are no more tasks
    private val queue    = LinkedBlockingQueue[Option[Task[Id]]]()
    private val segments = Array.fill(parallelism)(SegmentBuilder[Id]())
    private val threads  = segments.zipWithIndex.map: (segment, idx) =>
      val thread = Thread(() => run(segment), s"trigs-indexer-$idx")
      thread.setDaemon(true)
      thread.start()
      thread

    def submit(task: Task[Id]): Unit = queue.put(Some(task))

    /** Lets workers finish queued tasks, and returns their segments */
    def shutdown(): Seq[Segment[Id]] =
      threads.foreach(_ => queue.put(None))
      threads.foreach(_.join())
      // join makes the workers' writes visible to this thread
      segments.toSeq.map(_.snapshot())

    private def run(segment: SegmentBuilder[Id]): Unit =
      var next = queue.take()
      while next.isDefined do
        process(segment, next.get)
        next = queue.take()
  end Pool
end IndexBuilder

object IndexBuilder:
  private final case class Task[Id](id: Id, seq: Long, source: Source)

  private enum Source:
    case Text(text: String)
    case Bytes(bytes: Array[Byte])
    case Lazy(open: () => InputStream)
end IndexBuilder

/** Documents added with [[IndexBuilder.addLazy]] that couldn't be indexed */
final class IndexingFailedException(val failures: Seq[(Any, Throwable)])
    extends RuntimeException(
      s"Failed to index ${failures.size} document(s), first: ${failures.head._1}",
      failures.head._2
    ):
  failures.tail.foreach((_, exc) => addSuppressed(exc))
