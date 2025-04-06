package trigs
import java.io.InputStream

case class TrigramIndex(locs: Map[Trigram, Set[Location]]):
  override def toString(): String = s"Index[${locs.size} trigrams]"

object TrigramIndex:
  def read(loc: os.Path)(using Progress) =
    deserialise(os.read.inputStream(loc))

  def deserialise(is: InputStream)(using Progress) =
    TrigramIndexDeserialiser.deserialiseIntoBuilder(is).toIndex

  def builder(): TrigramIndexMutableBuilder = TrigramIndexMutableBuilder.empty
end TrigramIndex
