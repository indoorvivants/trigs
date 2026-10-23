package trigs
opaque type Trigram = String
object Trigram:
  inline def apply(a: Char, b: Char, c: Char): Trigram     = s"$a$b$c"
  private[trigs] inline def fromString(s: String): Trigram = s

  extension (t: Trigram)
    inline def _1: Char = t(0)
    inline def _2: Char = t(1)
    inline def _3: Char = t(2)
  given Ordering[Trigram] = Ordering.String
