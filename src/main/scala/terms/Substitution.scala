package terms

case class Substitution(map: Map[TermName, TermTree]) {
  def applyToTopLevel(termTree: TermTree): TermTree = {
    if (map.contains(termTree.symbol)) {
      map(termTree.symbol)
    } else {
      termTree
    }
  }

  override def toString: String = map.map(ab => ab._1() -> ab._2).mkString("sub(",",",")")
}

object Substitution {
  def generate(variables: List[TermName], allowedTermTrees: List[TermTree]): List[Substitution] = {
    def tupleTermTrees(length: Int): List[List[TermTree]] = length match {
      case 0 => List(List())
      case n => allowedTermTrees.flatMap(t =>tupleTermTrees(length-1).map(tt => t :: tt))
    }

    val allTermTreeLists = tupleTermTrees(variables.size)
    for (tt <- allTermTreeLists) yield {
      Substitution(variables.zip(tt).toMap)
    }
  }
}