import terms._

object main extends App {

  def variables(n: Int) = (for (x <- 1 to n) yield {
    TermName(x.toString, 0, Theory.Variable)
  }).toList

  val S = AlgebraicTheory.Boom(unital = true, associative = true, commutative = false, idempotent = false)
  val T = AlgebraicTheory.Boom(unital = true, associative = true, commutative = true, idempotent = false)
  val V = variables(4)

  val equations: Set[Equation] = S.labeledAxioms(Theory.S).union(T.labeledAxioms(Theory.T))
  println(s"Investigating whether there is a composite theory of ${S} distributing over ${T}")

  // generate terms with up to 4 variables
  val smallTermsInU = TermTree.generateWithVariablesInOrder(
    V.concat(S.labeledSignature(Theory.S)).concat(T.labeledSignature(Theory.T)), 2)

  println("We are checking the following small terms in U, which are already in reduced form:")
  println(smallTermsInU)

  // small substitutions only to start with
  val smallSubstitutionDepth = 0
  val tinyTermsInU = TermTree.generateWithVariablesInAnyOrder(
    V.
      concat(S.labeledSignature(Theory.S)).
      concat(T.labeledSignature(Theory.T)), smallSubstitutionDepth)

  val equivalenceClasses = Matcher.partitionEquivalenceClasses(smallTermsInU.toSet, equations)
  val equivalenceClassesNF: Map[Set[TermTree], Set[TermTree]] =
    equivalenceClasses.map(eq => eq -> eq.filter(t => Matcher.isInNormalForm(t, equations))).toMap

  if (!true) {
    println("These form the following equivalence classes:")
    equivalenceClasses.foreach(println)
  }

  // for each small term of the form s[t] we generate all substitutions
  val desiredTheoryType = TheoryTree(Theory.S, List(TheoryTree(Theory.T, List())))
  val termsOfFormST = smallTermsInU.
    filter(Matcher.isInNormalForm(_, equations)).
    filter(_.asTheoryTree.reduced == desiredTheoryType)


  if (true) {
    println(s"These are the reduced terms of form ${desiredTheoryType}:")
    println(termsOfFormST)
  }

  val noRepeatedVariables = termsOfFormST.filter(
    tt => tt.flattened.filter(_.isVariable).distinct == tt.flattened.filter(_.isVariable)
  )


  if (true) {
    println(s"These are the reduced terms of form ${desiredTheoryType} without repeated variables:")
    println(noRepeatedVariables)
  }

  val subs = Substitution.generate(V, tinyTermsInU.filter(Matcher.isInNormalForm(_, equations)))

  println(s"We will now check each of these terms under every substitution of depth ${smallSubstitutionDepth}," +
    s" only printing those substitutions that result in a term equal to a T(S) term when in normal form.")

  noRepeatedVariables.foreach(t => {
    println(s"Looking at term ${t}")
    subs.filter(_.map.keys.toSet.subsetOf(t.variables)).foreach(sigma => {
      val tToNFs = Matcher.findNormalForms(t.substitute(sigma),equations)
      val eq = equivalenceClasses.find(_.intersect(tToNFs).nonEmpty)
      if (eq.nonEmpty) {
        val NFs = equivalenceClassesNF(eq.get)
        if (NFs.exists(nf => nf.symbol.theory == Theory.T || nf.symbol.isVariable)) {
          println(s"${t} :: Under substitution ${sigma} yields an equivalence class with T terms ${
            NFs.mkString("{",",","}")
          }")
        }
      }
    }
    )
  })
}