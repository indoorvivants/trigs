package trigs
opaque type TRIGRAM = String
object TRIGRAM:
  def apply(a: Char, b: Char, c: Char): TRIGRAM = s"$a$b$c"

  extension (t: TRIGRAM)
    inline def _1: Char = t(0)
    inline def _2: Char = t(1)
    inline def _3: Char = t(2)
  given Ordering[TRIGRAM] = Ordering.String
end TRIGRAM
