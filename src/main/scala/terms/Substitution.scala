package terms

case class Substitution(map: Map[TermName, TermTree]) {
  def applyToTopLevel(termTree: TermTree): TermTree = {
    if(map.contains(termTree.symbol)) {
      map(termTree.symbol)
    } else{
      termTree
    }
  }
}