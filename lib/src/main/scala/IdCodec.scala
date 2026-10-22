package trigs

import java.nio.charset.StandardCharsets.UTF_8

/** How document ids are written to and read from a serialised index */
trait IdCodec[Id]:
  def encode(id: Id): Array[Byte]
  def decode(bytes: Array[Byte]): Id

  def imap[B](to: Id => B)(from: B => Id): IdCodec[B] =
    val self = this
    new IdCodec[B]:
      def encode(id: B): Array[Byte]   = self.encode(from(id))
      def decode(bytes: Array[Byte]): B = to(self.decode(bytes))
end IdCodec

object IdCodec:
  def apply[Id](using codec: IdCodec[Id]): IdCodec[Id] = codec

  given IdCodec[String] with
    def encode(id: String): Array[Byte]      = id.getBytes(UTF_8)
    def decode(bytes: Array[Byte]): String = String(bytes, UTF_8)
end IdCodec
