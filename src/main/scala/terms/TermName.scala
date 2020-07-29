package terms
import scala.language.implicitConversions
import terms.Theory.Theory

case class TermName(name: String, arity: Int, theory: Theory = Theory.Undeclared) {
  assert(arity >= 0)

  def isVariable: Boolean = theory == Theory.Variable

  def apply(subtrees: TermTree*): TermTree = TermTree(this, subtrees.toList)
}

object TermName {
  implicit def termName2termTree(termName: TermName) : TermTree = termName()
}
