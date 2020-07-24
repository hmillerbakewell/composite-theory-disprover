package terms

case class TermTree(symbol: TermName, children: List[TermTree] = List()) {
  assert(children.length == symbol.arity)
  lazy val depth: Int = children.length match {
    case 0 => 0
    case _ => children.map(_.depth).max + 1
  }

  def substitute(substitution: Substitution): TermTree = {
    if (substitution.map.contains(symbol)) {
      substitution.map(symbol)
    } else {
      TermTree(symbol, children.map(_.substitute(substitution)))
    }
  }

  def matchedOntoBy(other: TermTree): Boolean =
    (other.symbol.theory == Theory.Variable) ||
      ((symbol == other.symbol) && children.zip(other.children).forall(x => x._1.matchedOntoBy(x._2)))

  override def toString: String = children.length match {
    case 0 => symbol.name
    case _ => children.mkString(s"${symbol.name}(", ",", ")")
  }

  lazy val variables: Set[TermName] = {
    symbol.theory match {
      case Theory.Variable => Set(symbol)
      case _ => children.flatMap(_.variables).toSet
    }
  }

  def ~(other: TermTree): Equation = {
    Equation(this, other)
  }

  lazy val asTheoryTree: TheoryTree = TheoryTree(symbol.theory, children.map(_.asTheoryTree))
}

