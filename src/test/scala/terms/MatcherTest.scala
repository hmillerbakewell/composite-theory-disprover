package terms

import org.scalatest.FunSuite

class MatcherTest extends FunSuite {
  test("Find most simple match") {
    val e = TermName("e", 0, Theory.S)
    println(Matcher.find(e(), e()))
    assert(Matcher.find(e(), e()).length == 1)
  }
  test("Find variable match") {
    val e = TermName("e", 0, Theory.S)
    val x = TermName("x", 0, Theory.Variable)
    println(Matcher.find(x(), e()))
    assert(Matcher.find(x(), e()).length == 1)
  }
  test("Find variable match with renaming") {
    val x = TermName("x", 0, Theory.Variable)
    println(Matcher.find(x(), x()))
    assert(Matcher.find(x(), x()).length == 1)
  }
  test("Find variable clash") {
    val x = TermName("x", 0, Theory.Variable)
    val f = TermName("f", 2, Theory.S)
    val e = TermName("e", 0, Theory.S)
    val e2 = TermName("e2", 0, Theory.S)
    val matches = Matcher.find(f(x, x), f(e, e))
    println(matches)
    assert(matches.length == 1)
    val clash = Matcher.find(f(x, x), f(e, e2))
    assert(clash.isEmpty)
  }
  test("Find multiple matches") {
    val e = TermName("e", 0, Theory.S)
    val f = TermName("f", 2, Theory.S)
    println(Matcher.find(e(), f(e(), e())))
    assert(Matcher.find(e(), f(e(), e())).length == 2)
  }
  test("Find and replace") {
    val e = TermName("e", 0, Theory.S)
    val g = TermName("g", 0, Theory.S)
    val f = TermName("f", 2, Theory.S)
    val replacements = Matcher.find(e(), f(e(), e())).map(
      m => m.replaceWith(g())
    )
    assert(replacements == List(f(g, e), f(e, g)))
  }

  test("reducing to normal forms") {
    val e = TermName("e", 0, Theory.S)
    val g = TermName("g", 0, Theory.S)
    val f = TermName("f", 2, Theory.S)
    val nfs = Matcher.findNormalForms(f(g, g), Set(e ~ g, f(e, g) ~ g))
    println(nfs)
  }
  test("Each equivalence class contains something in normal form") {

    def variables(n: Int) = (for (x <- 1 to n) yield {
      TermName(x.toString, 0, Theory.Variable)
    }).toList

    val S = AlgebraicTheory.Boom(true, true, true, false)
    val T = AlgebraicTheory.Boom(true, true, false, false)
    val V = variables(4)

    val equations: Set[Equation] = S.labeledAxioms(Theory.S).union(T.labeledAxioms(Theory.T))

    // generate terms with up to 4 variables
    val smallTermsInU = TermTree.generateWithVariablesInOrder(
      V.concat(S.labeledSignature(Theory.S)).concat(T.labeledSignature(Theory.T)), 2)


    val equivalenceClasses = Matcher.partitionEquivalenceClasses(smallTermsInU.toSet, equations)
    equivalenceClasses.filter(
      e => e.exists(Matcher.isInNormalForm(_, equations))
    )
    equivalenceClasses.foreach(e => {
      assert(e.exists(Matcher.isInNormalForm(_, equations)))
    })
  }
}
