package terms

import org.scalatest.FunSuite
import terms.TermTree.generateWithVariablesInAnyOrder

class TermTreeTest extends FunSuite {
  val m: TermName = TermName("m", 2, Theory.S)
  val e: TermName = TermName("e", 0, Theory.S)
  val d: TermName = TermName("d", 0, Theory.S)
  val i: TermName = TermName("i", 1, Theory.S)
  val n: TermName = TermName("n", 2, Theory.T)
  val f: TermName = TermName("f", 0, Theory.T)
  val x: TermName = TermName("x", 0, Theory.Variable)
  val y: TermName = TermName("y", 0, Theory.Variable)
  test("Ordering theories") {
    assert(Theory.Variable < Theory.T)
    assert(Theory.T < Theory.S)
    assert(x < f)
    assert(f < e)
  }
  test("Ordering depth") {
    assert(m(e,e) < m(m(d,e),e))
  }
  test("Ordering arities") {
    assert(e < m(e,e))
  }
  test("Ordering children") {
    assert(m(e,e) == m(e,e))
    assert(m(e,d) < m(e,e))
  }
  test("Ordering alphabetically") {
    assert(d < e)
  }

  test("Generate only one variable term"){
    val terms = TermTree.generateWithVariablesInOrder(List(x,y),2)
    assert(terms.length == 1)
  }
  test("Flattening works"){
    assert(m(x,m(y,y)).flattened == List(m,x,m,y,y))
    assert(m(x,m(y,y)).flattened.filter(_.isVariable) == List(x,y,y))
  }

  test("Generate terms with variables in any order"){

    val symbols = List(x,y,m,e)
    val maxDepth = 2
    val allTerms = generateWithVariablesInAnyOrder(symbols, maxDepth)
    assert(allTerms.contains(m(x,m(x,x))))
    assert(allTerms.contains(m(x,m(x,y))))
    assert(allTerms.contains(m(x,m(y,x))))
    assert(allTerms.contains(m(x,m(y,y))))
    assert(allTerms.contains(m(y,m(x,x))))
  }

  test("Generate terms only with first variable x"){
    val symbols = List(x,y,m,e)
    val maxDepth = 2
    val terms = TermTree.generateWithVariablesInOrder(symbols,maxDepth)
    assert(terms.contains(m(x,m(x,x))))
    assert(terms.contains(m(x,m(x,y))))
    assert(terms.contains(m(x,m(y,x))))
    assert(terms.contains(m(x,m(y,y))))
    assert(terms.forall(tt => {
      val varsUsed = tt.flattened.filter(_.isVariable)
      varsUsed.headOption.isEmpty || varsUsed.head == x
    }))
  }
}
