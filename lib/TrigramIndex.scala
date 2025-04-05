package trigs
import java.io.InputStream

case class TrigramIndex(locs: Map[TRIGRAM, Set[Location]]):
  override def toString(): String = s"Index[${locs.size} trigrams]"

object TrigramIndex:
  def read(loc: os.Path)(using Progress) =
    deserialise(os.read.inputStream(loc))

  def deserialise(is: InputStream)(using Progress) =
    TrigramIndexDeserialiser.deserialise(is)
end TrigramIndex
