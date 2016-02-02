package typelog

import org.scalatest.FunSuite

@typelog("""
  mosquito.john.stork.frog.

  is_digesting(X,Y) :- just_ate(X,Y).
  is_digesting(X,Y) :-
    just_ate(X,Z),
    is_digesting(Z,Y).

  just_ate(mosquito, john).
  just_ate(frog, mosquito).
  just_ate(stork, frog).
""")
object rec; import rec._, module._

class RecSuite extends FunSuite {
  test("is_digesting(stork, mosquito)") {
    assert(ask[is_digesting[stork, john]])
  }
}
