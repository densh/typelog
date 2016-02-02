package typelog
package internal

import fastparse.all.Parsed
import scala.reflect.macros.whitebox.Context

class Macros(val c: Context) {
  import c.universe._

  implicit val liftTerm: Liftable[Term] = Liftable {
    case Term.Int(id) =>
      tq"${TypeName(id)}"
    case Term.Ext(id) =>
      tq"${TypeName(id)}"
    case Term.Var(id) =>
      tq"${TypeName(id)}"
    case Term.Functor(id, args) =>
      tq"$id[..$args]"
  }

  def varsof(t: Term): Set[Term.Var] = t match {
    case _: Term.Int | _: Term.Ext =>
      Set()
    case v: Term.Var =>
      Set(v)
    case Term.Functor(_, args) =>
      args.map(arg => varsof(arg).toSeq).flatten.toSet
  }

  def compileClause(clause: Clause, name: TypeName, pred: Option[TypeName], last: TypeName) = {
    val statName = TermName(c.freshName())
    val stat = clause match {
      case Clause(head, Seq()) =>
        q"implicit val $statName: $head = null"
      case Clause(head, terms) =>
        val allvars =
          (varsof(head) ++ terms.map(varsof(_).toSeq).flatten.toSet)
        println(s"vars: $allvars")
        val vars = allvars.map { case Term.Var(id) =>
          q"type ${TypeName(id)}"
        }
        val deps = terms.map { t =>
          val name = TermName(c.freshName())
          q"val $name: $t"
        }
        q"implicit def $statName[..$vars](implicit ..$deps): $head = null"
    }
    pred.map { parent =>
      q"trait $name extends $parent { self: $last => $stat }"
    }.getOrElse {
      q"trait $name { self: $last => $stat }"
    }
  }

  def compile(clauses: Seq[Clause]) = {
    val defns = clauses.map {
      case Clause(Term.Int(id), _) =>
        Seq(id -> 0)
      case Clause(Term.Functor(Term.Int(id), args), _) =>
        Seq(id -> args.length)
      case _ =>
        Seq()
    }.flatten.toMap
    val pre = defns.map { case (id, n) =>
      val targs = (1 to n).map { i =>
        val name = TypeName("_" + i)
        q"type $name"
      }
      val name = TypeName(id)
      q"trait $name[..$targs]"
    }
    val names = clauses.map(_ => TypeName(c.freshName()))
    val preds = None +: names.init.map(Some(_))

    (pre ++ clauses.zip(names).zip(preds).map { case ((c, n), pred) =>
      compileClause(c, n, pred, names.last)
    }).toSeq :+ {
      q"object module extends ${names.last}"
    }
  }

  def typelog(code: String): Seq[Tree] = {
    println(s"code: $code")
    val Parsed.Success(clauses, _) = Parsers.clauses.parse(code)
    println(s"parsed: $clauses")
    val stats = compile(clauses).toSeq
    println(s"compiled:")
    stats.foreach(println)
    stats
  }

  def typelogAnnot(annottees: Tree*) = annottees match {
    case Seq(ModuleDef(m, n, Template(p, s, body))) =>
      val q"new $_(${code: String}).$_($_)" = c.macroApplication
      ModuleDef(m, n, Template(p, s, body ++ typelog(code)))
  }

  def ask[T: WeakTypeTag] = {
    val res = c.inferImplicitValue(weakTypeOf[T], silent = true)
    if (res.nonEmpty)
      q"{ $res; true }"
    else
      q"false"
  }
}
