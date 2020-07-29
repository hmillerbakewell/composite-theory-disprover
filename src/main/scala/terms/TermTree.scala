package terms

import terms.Theory.Theory

case class TermTree(symbol: TermName, children: List[TermTree] = List())
  extends Ordered[TermTree] {
  assert(children.length == symbol.arity)
  lazy val depth: Int = children.length match {
    case 0 => 0
    case _ => children.map(_.depth).max + 1
  }

  def relabelNonVariables(newLabel: Theory): TermTree = {
    TermTree(
      symbol match {
        case TermName(_, _, Theory.Variable) => symbol
        case TermName(n, a, _) => TermName(n, a, newLabel)
      },
      children.map(_.relabelNonVariables(newLabel))
    )
  }

  def substitute(substitution: Substitution): TermTree = {
    if (substitution.map.contains(symbol)) {
      substitution.map(symbol)
    } else {
      TermTree(symbol, children.map(_.substitute(substitution)))
    }
  }

  def prependVariableNames(string: String): TermTree = {
    if (symbol.theory == Theory.Variable) {
      TermName(string + symbol.name, 0, Theory.Variable)()
    } else {
      TermTree(symbol, children.map(_.prependVariableNames(string)))
    }
  }

  override def compare(that: TermTree): Int = {
    // TermTrees are ordered!
    // shallower < deeper
    // lower arity < higher arity
    // alphabetic ordering on term name
    if (depth == that.depth) {
      if (children.length == that.children.length) {
        if (symbol == that.symbol) {
          // Compare children lexicographically
          children.zip(that.children).map(x => x._1.compare(x._2)).filterNot(_ == 0).headOption.getOrElse(0)
        } else {
          symbol.name.compare(that.symbol.name)
        }
      } else {
        children.length - that.children.length
      }
    } else {
      depth - that.depth
    }

  }

  override def toString: String = {
    val theoryPrepend = if (symbol.theory == Theory.Variable || symbol.theory == Theory.Undeclared) "" else symbol.theory
    children.length match {
      case 0 => s"${theoryPrepend}${symbol.name}"
      case _ => children.mkString(s"${theoryPrepend}${symbol.name}(", ",", ")")
    }
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

  lazy val flattened: List[TermName] = {
    symbol :: children.flatMap(_.flattened)
  }

  lazy val asTheoryTree: TheoryTree = TheoryTree(symbol.theory, children.map(_.asTheoryTree))
}


object TermTree {

  def generateWithVariablesInOrder(symbols: List[TermName], maxDepth: Int): List[TermTree] = {

    @scala.annotation.tailrec
    def matchHead(vs: List[TermName], ts: List[TermName]): Boolean = {
      ts.isEmpty || (vs.head == ts.head && matchHead(vs.tail, ts.filterNot(_ == vs.head)))
    }

    val V = symbols.filter(_.isVariable)
    generateWithVariablesInAnyOrder(symbols, maxDepth).filter(tt => matchHead(V, tt.flattened.filter(_.isVariable)))
  }

  def generateWithVariablesInAnyOrder(symbols: List[TermName], maxDepth: Int): List[TermTree] = {
    maxDepth match {
      case 0 =>
        symbols.filter(_.arity == 0).map(_.apply())
      case n =>
        val smallerTerms = generateWithVariablesInAnyOrder(symbols, n - 1)

        // Should be memo-ized to save runtime?
        def chooseMSmallerTerms(m: Int): List[List[TermTree]] = m match {
          case 0 => List(List[TermTree]())
          case _ => smallerTerms.flatMap(x => chooseMSmallerTerms(m - 1).map(y => x :: y))
        }

        symbols.flatMap(x => chooseMSmallerTerms(x.arity).map(y => TermTree(x, y)))
    }
  }
}
