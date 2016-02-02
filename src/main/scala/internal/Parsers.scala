package typelog
package internal

import fastparse.WhitespaceApi
import fastparse.noApi._

object Parsers {
  val (ws, api) = {
    import fastparse.all._
    val ws = NoTrace(CharIn(Array(' ', '\n', '\t')).rep)
    (ws, WhitespaceApi.Wrapper(ws))
  }
  import api._

  lazy val int: P[Term.Int] =
    P((CharIn('a' to 'z')
      |CharIn('0' to '9')
      |CharIn(Array('_'))
      ).rep(1).!).map(Term.Int)

  lazy val ext: P[Term.Ext] =
    P("`" ~ CharPred(_ != '`').rep(1).! ~ "`").map(Term.Ext)

  lazy val atom: P[Term] = int | ext

  lazy val variable: P[Term.Var] =
    P(CharIn('A' to 'Z').rep(1).!).map(Term.Var)

  lazy val functor: P[Term.Functor] =
    P(atom ~ "(" ~ term.rep(sep = ",") ~ ")").map(
      (Term.Functor.apply _).tupled)

  lazy val term: P[Term] =
    P(functor | atom | variable)

  lazy val clause: P[Clause] =
    P(ws ~ term ~ (":-" ~ term.rep(sep = ",")).? ~ "." ~ ws).map {
      case (head, None) => Clause(head, Seq())
      case (head, Some(body)) => Clause(head, body)
    }

  lazy val clauses: P[Seq[Clause]] =
    P(clause.rep ~ End)
}
