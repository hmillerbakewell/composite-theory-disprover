package terms

case class UnificationProblem(equations: Set[Equation]) {

  override def toString: String = equations.mkString("unify{", ",", "}")

  def delete(equation: Equation): UnificationProblem = UnificationProblem(equations - equation)

  def decompose(equation: Equation): UnificationProblem = {
    val toAdd = equation.lhs.children.zip(equation.rhs.children).map(
      x => Equation(x._1, x._2)
    )
    UnificationProblem(
      equations - equation ++ toAdd
    )
  }

  def orient(equation: Equation): UnificationProblem = UnificationProblem(
    equations - equation + Equation(equation.rhs, equation.lhs)
  )

  def eliminate(equation: Equation): UnificationProblem = {
    val substitution = Substitution(Map(equation.lhs.symbol -> equation.rhs))
    UnificationProblem(
      (equations - equation).map(x => Equation(
        x.lhs.substitute(substitution),
        x.rhs.substitute(substitution)
      )) + equation)
  }

  lazy val solved: Boolean = {
    val varsUsedOnLeft = equations.map(e => e.lhs.symbol)
    equations.forall(e => e.lhs.symbol.theory == Theory.Variable) &&
      varsUsedOnLeft.size == equations.size &&
      equations.forall(_.rhs.variables.intersect(varsUsedOnLeft).isEmpty)
  }

  def reduce(): Option[UnificationProblem] = {
    val r = tryReduce
    if (r.solved) Some(r) else None
  }

  def reduceToSubstitution: Option[Substitution] = {
    val reduced = reduce()
    if (reduced.isEmpty) return None
    val equations = reduced.get.equations
    Some(Substitution(equations.map(e => e.lhs.symbol -> e.rhs).toMap))
  }

  def tryReduce: UnificationProblem = {
    val eliminateOption = equations.find(_.canEliminate).map(eliminate)
    if (eliminateOption.nonEmpty && eliminateOption.get != this) {
      return eliminateOption.get.tryReduce
    }
    val deleteOption = equations.find(_.canDelete).map(delete)
    if (deleteOption.nonEmpty) {
      return deleteOption.get.tryReduce
    }
    val decomposeOption = equations.find(_.canDecompose).map(decompose)
    if (decomposeOption.nonEmpty && decomposeOption.get != this) {
      return decomposeOption.get.tryReduce
    }
    val orientOption = equations.find(_.canOrient).map(orient)
    if (orientOption.nonEmpty && orientOption.get != this) {
      return orientOption.get.tryReduce
    }
    this
  }
}

case class Equation(lhs: TermTree, rhs: TermTree) {
  override def toString: String = s"$lhs ~ $rhs"

  lazy val canDelete: Boolean = {
    lhs == rhs
  }

  def reverse: Equation = Equation(rhs, lhs)

  lazy val canDecompose: Boolean = {
    lhs.symbol == rhs.symbol
  }

  lazy val canOrient: Boolean = {
    rhs.symbol.theory == Theory.Variable && lhs.symbol.theory != Theory.Variable
  }

  lazy val canEliminate: Boolean = {
    lhs.symbol.theory == Theory.Variable && !rhs.variables.contains(lhs.symbol)
  }
}