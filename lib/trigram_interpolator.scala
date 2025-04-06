package trigs

import scala.quoted.*
extension (inline ctx: StringContext)
  transparent inline def trigram(inline args: Any*): Any =
    ${ trigramImpl('ctx, 'args) }

def trigramImpl(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using
    Quotes
): Expr[Trigram] =
  val s = strCtxExpr.valueOrAbort.parts.head
  if s.length != 3 then
    quotes.reflect.report.errorAndAbort(
      s"trigram must be exactly three characters long"
    )

  val se = Expr(s)

  '{ Trigram.fromString($se) }

end trigramImpl
