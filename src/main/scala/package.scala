import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

package object typelog {
  class typelog extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any =
      macro _root_.typelog.internal.Macros.typelogAnnot
  }
  def ask[T]: Boolean =
    macro _root_.typelog.internal.Macros.ask[T]
}
