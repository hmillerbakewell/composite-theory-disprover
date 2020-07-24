import terms._

object main extends App {
  def STerm(name: String, arity: Int) = TermName(name, arity, Theory.S)

  def TTerm(name: String, arity: Int) = TermName(name, arity, Theory.T)

  def V(n: Int) = TermName(n.toString, 0, Theory.Variable)

  def variables(n: Int) = (for (x <- 1 to n) yield {
    TermName(x.toString, 0, Theory.Variable)
  }).toList

  val S = List(STerm("m", 2), STerm("e", 0))
  val T = List(TTerm("n", 2), TTerm("f", 0))

  val exampleSTerm = TermTree(
    STerm("m", 2),
    List(STerm("e", 0)(), TermName("1", 0, Theory.Variable)()))

  if (false) {
    Generator.generateTermTrees(variables(4).concat(S).concat(T), 2).foreach(x =>
      println(s"$x + :: ${x.asTheoryTree} :: ${x.asTheoryTree.reduced}")
    )
  }
  if (true) {
    val x = V(1)()
    val y = V(2)()
    val f = STerm("f", 1)
    val g = STerm("g", 2)
    val a = STerm("a", 0)()
    val u =UnificationProblem(
      Set(
        x ~ f(a),
        g(x, x) ~ g(x, y)
      )
    )
    println(u)
    println(u.reduce())
  }

}