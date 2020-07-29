package terms

import org.scalatest.FunSuite

class TheoryTreeTest extends FunSuite {
  val m: TermName = TermName("m", 2, Theory.S)
  val e: TermName = TermName("e", 0, Theory.S)
  val d: TermName = TermName("d", 0, Theory.S)
  val i: TermName = TermName("i", 1, Theory.S)
  val n: TermName = TermName("n", 2, Theory.T)
  val f: TermName = TermName("f", 0, Theory.T)
  val x: TermName = TermName("x", 0, Theory.Variable)
  val y: TermName = TermName("y", 0, Theory.Variable)
  test("Reducing theory trees") {
    assert(m(f, f).asTheoryTree.reduced == TheoryTree(Theory.S, List(TheoryTree(Theory.T, List()))))
  }
}
