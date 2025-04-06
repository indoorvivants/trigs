package trigs
import java.io.InputStream

case class TrigramIndex(locs: Map[Trigram, Set[Location]]) extends Searchable:
  override def toString(): String     = s"Index[${locs.size} trigrams]"
  override protected lazy val indexer = TrigramIndexer()
  override def search(trigrams: Array[Trigram]): List[(Location, Float)] =
    val res = collection.mutable.Map.empty[Location, Int].withDefaultValue(0)

    trigrams.foreach: token =>
      if locs.contains(token) then
        locs(token).foreach: location =>
          res(location) += 1

    res.toList
      .map((l, cnt) => l -> cnt.toFloat / trigrams.length.toFloat)
      .sortBy(_._2 * -1)
  end search
end TrigramIndex

object TrigramIndex:
  def read(loc: os.Path)(using Progress) =
    deserialise(os.read.inputStream(loc))

  def deserialise(is: InputStream)(using Progress) =
    TrigramIndexDeserialiser.deserialiseIntoBuilder(is).toIndex

  def builder(): TrigramIndexMutableBuilder = TrigramIndexMutableBuilder.empty
end TrigramIndex
