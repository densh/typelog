package typelog

import org.scalatest.FunSuite

@typelog("""
james1.charles1.charles2.james2.george1.
catherine.elizabeth.sophia.
male(james1).
male(charles1).
male(charles2).
male(james2).
male(george1).
female(catherine).
female(elizabeth).
female(sophia).
parent(charles1, james1).
parent(elizabeth, james1).
parent(charles2, charles1).
parent(catherine, charles1).
parent(james2, charles1).
parent(sophia, elizabeth).
parent(george1, sophia).
""")
object legacy; import legacy._, legacy.module._

class LegacySuite extends FunSuite {
  test("Was George I the parent of Charles I?") {
    assert(!ask[parent[charles1, george1]])
  }

  test("Who was Charles I's parent?") {
    assert(ask[parent[charles1, _]])
  }

  test("Who were the children of Charles I?") {
    assert(ask[parent[_, charles1]])
  }
}
