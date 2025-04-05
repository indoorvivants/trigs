package trigs

import scala.quoted.*

inline def sizeOf[T]: Int = ${ sizeOfMacro[T] }

private def sizeOfMacro[T: Type](using Quotes): Expr[Int] =
  import quotes.reflect.*, report.errorAndAbort
  Type.of[T] match
    case '[Int] => '{ 4 }
    case other =>
      errorAndAbort(s"Cannot figure out the size of ${TypeRepr.of[T].show}")
end sizeOfMacro
