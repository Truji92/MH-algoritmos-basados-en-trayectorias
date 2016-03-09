import algorithms.Greedy
import data.Problems
import org.scalacheck.Prop._
import org.scalacheck.{Gen, Properties}
import types.Types._

class AlgorithmsTests extends Properties("Algorithms"){

  val problems = Gen.oneOf(Problems.tai25, Problems.sko90, Problems.tai150)

  property("GreedyIsNotBetterThanOptimal") = forAll (problems) {
    case (problem: ProblemData, optimalSolution: OptimalSolution) =>
      val (_, optCost, solution) = optimalSolution

      val greedySolution = Greedy(problem)

      optCost <= algorithms.cost(problem, greedySolution)
  }

}
