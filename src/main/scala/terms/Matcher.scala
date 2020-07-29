package terms

case class Match(source: TermTree, target: TermTree, position: List[Int], substitution: Substitution) {
  def replaceWith(replacementTermTree: TermTree): TermTree = {
    def traverseAndReplace(nextChild: TermTree, currentPosition: List[Int]): TermTree = {
      if (currentPosition == position) {
        Matcher.consistentRename(replacementTermTree, target).substitute(substitution)
      } else {
        TermTree(nextChild.symbol, nextChild.children.zipWithIndex.map(
          ci => traverseAndReplace(ci._1, ci._2 :: currentPosition)
        ))
      }
    }

    traverseAndReplace(target, currentPosition = List())
  }

  override def toString: String =
    s"Match($source onto $target at ${position.mkString("", ":", ":root")} using ${substitution})"
}

object Matcher {
  def isInNormalForm(start: TermTree, equations: Set[Equation]): Boolean = {
    immediateReductions(start, equations).isEmpty
  }

  def findNormalForms(start: TermTree, equations: Set[Equation]): Set[TermTree] =
    findNormalFormsInternal(Set(start), equations.flatMap(e => Set(e, e.reverse)))

  def immediateReductions(start: TermTree, equations: Set[Equation]): Set[TermTree] = {
    val es = equations.flatMap(e => Set(e, e.reverse))
    val successfulReductions = es.flatMap(e => {
      val listOfMatches = find(e.lhs, start)
      listOfMatches.map(_.replaceWith(e.rhs))
    }).filter(_ < start)
    successfulReductions
  }


  private def findNormalFormsInternal(start: Set[TermTree], equations: Set[Equation]): Set[TermTree] = {
    // For each term in the starting list we apply all equations in all ways,
    // we keep only those results that are reductions
    val next = start.flatMap(s => {
      val reds = immediateReductions(s, equations)
      if (reds.isEmpty) {
        Set(s)
      } else reds
    })
    if (next != start) {
      findNormalFormsInternal(next, equations)
    } else {
      next
    }
  }

  def consistentRename(source: TermTree, target: TermTree): TermTree = {
    def enoughPrimes(length: Int): String = length match {
      case 0 => ""
      case n => "'" + enoughPrimes(length - 1)
    }

    val prependedString = enoughPrimes(target.variables.map(x => x.name.length).maxOption.getOrElse(0))
    source.prependVariableNames(prependedString)
  }

  def mostGeneralTerms(terms: Set[TermTree]): Set[TermTree] = {
    def reduce(termToTest: TermTree, generalTerms: Set[TermTree]): Set[TermTree] = {
      if (!generalTerms.contains(termToTest)) return generalTerms
      generalTerms.filter(Matcher.find(termToTest, _).isEmpty) + termToTest
    }

    terms.foldLeft(terms) {
      case (acc, next) => reduce(next, acc)
    }

  }

  def find(source: TermTree, target: TermTree): List[Match] = {
    // Need to rename variables
    val relabeledSource = consistentRename(source, target)
    val matchesHere = rootMatch(relabeledSource, target)
    val matchesChildren = target.children.map(
      c => rootMatch(relabeledSource, c)
    ).zipWithIndex.flatMap(ci => ci._1.map {
      case Match(s, t, p, sub) => Match(s, target, ci._2 :: p, sub)
    })
    matchesHere.concat(matchesChildren)
  }

  private def rootMatch(source: TermTree, target: TermTree): List[Match] = {
    val unificationRequirements = notYetUnifiedMatch(source, target)
    if (unificationRequirements.isEmpty) return List()
    val unificationProblem = UnificationProblem(unificationRequirements.get.toSet)
    val sub = unificationProblem.reduceToSubstitution
    if (sub.isEmpty) return List()
    List(Match(source, target, List(), sub.get))
  }

  private def notYetUnifiedMatch(source: TermTree, target: TermTree): Option[List[Equation]] = {
    if (source.symbol.theory == Theory.Variable) {
      return Some(List(source.symbol() ~ target))
    }
    if (source.symbol == target.symbol) {
      return source.children.zip(target.children).map(
        cc => notYetUnifiedMatch(cc._1, cc._2)
      ).fold(Some(List())) {
        (acc, next) => if (next.isEmpty || acc.isEmpty) None else Some(acc.get.concat(next.get))
      }
    }
    None
  }

  def partitionEquivalenceClasses(terms: Set[TermTree], equations: Set[Equation]): Set[Set[TermTree]] = {
    def merge(joiningElements: Set[TermTree], eqs: Set[Set[TermTree]]): Set[Set[TermTree]] = {
      val (toBeJoined, separate) = eqs.partition(eq => eq.intersect(joiningElements).nonEmpty)
      (separate + toBeJoined.flatten).filter(_.nonEmpty)
    }

    terms.foldLeft(terms.map(Set(_))) {
      (acc: Set[Set[TermTree]], next: TermTree) =>
        merge(Matcher.immediateReductions(next, equations) + next, acc)
    }
  }
}
