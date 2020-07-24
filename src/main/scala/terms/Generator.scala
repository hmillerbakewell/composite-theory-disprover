package terms

object Generator {
  def generateTermTrees(symbols: List[TermName], maxDepth: Int): List[TermTree] = {
    maxDepth match {
      case 0 => symbols.filter( _.arity == 0).map(x => TermTree(x,List()))
      case n =>
        val smallerTerms = generateTermTrees(symbols, n - 1)

        // Should be memo-ized to save runtime?
        def chooseMSmallerTerms(m: Int): List[List[TermTree]] = m match {
          case 0 => List(List[TermTree]())
          case _ => smallerTerms.flatMap(x => chooseMSmallerTerms(m-1).map(y => x :: y))
        }

        symbols.flatMap(x => chooseMSmallerTerms(x.arity).map(y => TermTree(x,y)))
    }
  }
}
