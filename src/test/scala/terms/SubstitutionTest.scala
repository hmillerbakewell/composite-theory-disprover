package terms

import org.scalatest.FunSuite

class SubstitutionTest extends FunSuite {
  val m: TermName = TermName("m", 2, Theory.S)
  val e: TermTree = TermName("e", 0, Theory.S)()
  val i: TermName = TermName("i", 1, Theory.S)
  val n: TermName = TermName("n", 2, Theory.T)
  val f: TermTree = TermName("f", 0, Theory.T)()
  val x: TermTree = TermName("x", 0, Theory.Variable)()
  val y: TermTree = TermName("y", 0, Theory.Variable)()

  test("Apply substitution") {
    val t1 = m(e,x)
    val t2 = m(y,m(x,y))
    val sub = Substitution(Map(x.symbol -> i(y), y.symbol ->e))
    assert(t1.substitute(sub) == m(e,i(y)))
    assert(t2.substitute(sub) == m(e, m(i(y),e)))
  }

  test("Generate substitutions") {
    val depth2 = TermTree.generateWithVariablesInAnyOrder(List(x.symbol,y.symbol,m, e.symbol),2)
    println(Substitution.generate(List(x.symbol,y.symbol),depth2))
    assert(Substitution.generate(List(x.symbol,y.symbol),depth2).length == math.pow(depth2.length,2))
  }
}
