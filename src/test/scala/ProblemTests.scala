import data.Problems
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}
import types.Types._

class ProblemTests extends Properties("Problems") {

  val problems = Gen.oneOf(Problems.tai25, Problems.sko90, Problems.tai150)

  property("optimalSolutionHasOptimalCost") = forAll (problems) { case (problem: ProblemData, optimalSolution: OptimalSolution) =>
    val (_, optCost, solution) = optimalSolution
    println(problem)

    algorithms.cost(problem, solution) == optCost
  }

}
