package typelog

import org.scalatest.FunSuite

class ImmSuite extends FunSuite {
  test("imm(List(Int))") {
    @typelog("""
      imm(`Int`).
      imm(`List`(T)) :- imm(T).
    """)
    object imm
    import imm._
    assert(ask[imm[List[Int]]])
  }
}
