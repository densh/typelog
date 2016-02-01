package typelog
package internal

final case class Clause(head: Term, body: Seq[Term])

sealed abstract class Term
object Term {
  final case class Int(value: String) extends Term
  final case class Ext(value: String) extends Term
  final case class Var(value: String) extends Term
  final case class Functor(id: Term, args: Seq[Term]) extends Term
}
