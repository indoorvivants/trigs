package trigs

/** A matching line.
  *
  * @param line
  *   1-based line number within the document
  * @param score
  *   higher is better; in [0, 1] for trigram-only search, and higher when
  *   re-ranked against the line text
  */
case class Hit[Id](id: Id, line: Int, score: Float)
