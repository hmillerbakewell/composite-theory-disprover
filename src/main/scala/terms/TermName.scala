package terms

import terms.Theory.Theory

case class TermName(name: String, arity: Int, theory: Theory) {
  assert(arity >= 0)

  def apply(subtrees: TermTree*): TermTree = TermTree(this, subtrees.toList)
}
