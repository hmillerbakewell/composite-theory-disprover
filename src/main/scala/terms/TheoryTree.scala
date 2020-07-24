package terms

import terms.Theory.Theory

case class TheoryTree (symbol : Theory, children: List[TheoryTree]) {
  lazy val depth: Int = children.length match {
    case 0 => 0
    case _ => children.map(_.depth).max + 1
  }

  override def toString: String = children.length match {
    case 0 => symbol.toString
    case _ => children.mkString(s"${symbol.toString}(",",",")")
  }

  lazy val reduced : TheoryTree = {
    val reducedChildren = children.map(_.reduced)
    TheoryTree(symbol, reducedChildren.flatMap({
      case TheoryTree(`symbol`, xs) => xs
      case TheoryTree(Theory.Variable, xs) => xs
      case y => List(y)
    }).distinct)
  }
}

