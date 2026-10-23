package trigs

import java.io.{BufferedReader, Reader}
import scala.collection.{immutable, mutable}

/** A snapshot of a [[SegmentBuilder]]: an index of every document it was given,
  * with the sequence number of each add and whether it succeeded.
  * [[IndexBuilder]] decides which documents survive.
  */
private[trigs] final case class Segment[Id](
    index: TrigramIndex[Id],
    seqs: Array[Long],
    succeeded: immutable.BitSet
)

/** Single-threaded, append-only accumulator. Every add creates a new document,
  * even for an id seen before; replacing and removing is up to the caller.
  */
private[trigs] final class SegmentBuilder[Id]:
  private val tokenizer = TrigramIndexer()

  private val documents = mutable.ArrayBuffer.empty[Id]
  private val seqs      = mutable.ArrayBuffer.empty[Long]
  private val succeeded = mutable.BitSet.empty

  // per location (indexed line), numbered in the order lines are added
  private val locationDocument = IntBuffer(1024)
  private val locationLine     = IntBuffer(1024)
  private val locationTrigrams = IntBuffer(1024)

  // location numbers only increase, so every list is sorted
  private val postings = mutable.HashMap.empty[Trigram, IntBuffer]

  /** Indexes the lines of `reader`. If reading throws, the document is kept but
    * not marked as succeeded.
    */
  def add(id: Id, seq: Long, reader: Reader): Unit =
    val document = documents.length
    documents += id
    seqs += seq

    val lines      = BufferedReader(reader)
    var line       = lines.readLine()
    var lineNumber = 1
    while line != null do
      addLine(document, lineNumber, line)
      line = lines.readLine()
      lineNumber += 1

    succeeded += document
  end add

  def snapshot(): Segment[Id] =
    Segment(
      new TrigramIndex(
        documents.toVector,
        locationDocument.toArray,
        locationLine.toArray,
        locationTrigrams.toArray,
        postings.iterator
          .map((trigram, locations) => trigram -> locations.toArray)
          .toMap
      ),
      seqs.toArray,
      succeeded.toImmutable
    )

  private def addLine(document: Int, lineNumber: Int, line: String): Unit =
    val trigrams = tokenizer.index(line)
    if trigrams.nonEmpty then
      val location = locationLine.length
      var distinct = 0

      trigrams.foreach: trigram =>
        val locations = postings.getOrElseUpdate(trigram, IntBuffer())
        // a trigram repeated on this line finds it already at the end
        if locations.isEmpty || locations.last != location then
          locations += location
          distinct += 1

      locationDocument += document
      locationLine += lineNumber
      locationTrigrams += distinct
    end if
  end addLine
end SegmentBuilder
