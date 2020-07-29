package terms

import org.scalatest.FunSuite

class UnificationProblemTest extends FunSuite {

  def STerm(name: String, arity: Int): TermName = TermName(name, arity, Theory.S)

  def TTerm(name: String, arity: Int): TermName = TermName(name, arity, Theory.T)

  def V(n: Int): TermName = TermName(n.toString, 0, Theory.Variable)

  test("example unification") {
    val x = V(1)
    val y = V(2)
    val f = STerm("f", 1)
    val g = STerm("g", 2)
    val a = STerm("a", 0)
    val u = UnificationProblem(
      Set(
        x ~ f(a),
        g(x, x) ~ g(x, y)
      )
    )
    val reduced = u.reduce()
    val substitutionOption = u.reduceToSubstitution
    assert(substitutionOption.nonEmpty)
    val map = substitutionOption.get.map
    assert(map.getOrElse(x, x) == f(a))
    assert(map.getOrElse(y, y) == f(a))
  }
}
