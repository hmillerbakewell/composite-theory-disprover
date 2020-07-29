package terms

import terms.Theory.Theory

case class AlgebraicTheory(name: String, signature: Set[TermName], axioms: Set[Equation]) {
  def labeledSignature(label: Theory): Set[TermName] = {
    signature.map { case TermName(name, arity, _) => TermName(name, arity, label) }
  }

  def labeledAxioms(label: Theory): Set[Equation] = {
    axioms.map { case Equation(lhs, rhs) => Equation(lhs.relabelNonVariables(label), rhs.relabelNonVariables(label)) }
  }
}

object AlgebraicTheory {
  private val x = TermName("x", 0, Theory.Variable)
  private val y = TermName("y", 0, Theory.Variable)
  private val z = TermName("z", 0, Theory.Variable)

  def Boom(unital: Boolean, associative: Boolean, commutative: Boolean, idempotent: Boolean): AlgebraicTheory = {
    val e = TermName("e", 0)
    val m = TermName("m", 2)
    AlgebraicTheory(
      "Boom" + {
        if (unital) "U" else ""
      } + {
        if (associative) "A" else ""
      } + {
        if (commutative) "C" else ""
      } + {
        if (idempotent) "I" else ""
      }
      , Set(e, m), {
        if (unital) Set(m(e, x) ~ x, m(x, e) ~ x) else Set()
      }.concat {
        if (associative) Set(m(m(x, y), z) ~ m(x, m(y, z))) else Set()
      }.concat {
        if (commutative) Set(m(x, y) ~ m(y, x)) else Set()
      }.concat {
        if (idempotent) Set(m(x, x) ~ x) else Set()
      }
    )
  }

  val PointedSets: AlgebraicTheory = AlgebraicTheory("Pointed sets", Set(TermName("e", 0)), Set())
  val Monoids: AlgebraicTheory = {
    val e = TermName("e", 0)
    val m = TermName("m", 2)
    AlgebraicTheory("Monoids", Set(e, m),
      Set(
        m(e, x) ~ x,
        m(x, e) ~ x,
        m(x, m(y, z)) ~ m(m(x, y), z)
      ))
  }
  val CommutativeMonoids: AlgebraicTheory = {
    val m = TermName("m", 2)
    AlgebraicTheory(
      "Commutative monoids",
      Monoids.signature,
      Monoids.axioms + (m(x, y) ~ m(y, x))
    )
  }
  val JoinSemilattices: AlgebraicTheory = {
    val m = TermName("m", 2)
    AlgebraicTheory(
      "Join semilattices",
      CommutativeMonoids.signature,
      CommutativeMonoids.axioms + (m(x, x) ~ x)
    )
  }
  val AbelianGroups: AlgebraicTheory = {
    val m = TermName("m", 2)
    val i = TermName("i", 1)
    val e = TermName("e", 0)
    AlgebraicTheory(
      "Abelian groups",
      CommutativeMonoids.signature + i,
      CommutativeMonoids.axioms + (m(i(x), x) ~ e) + (m(x, i(x)) ~ e)
    )
  }
}
