package algorithms

import data.Problems
import types.Types.ProblemData

import scala.util.Random

/**
  * Created by alejandro on 23/04/16.
  */
object VNS {

  val maxK = 5

  def apply(inputs: ProblemData, random: Random) = {
    val (n, _,_ ) = inputs

    val initialSolution = randomSolution(n, random)

    var k = 1
    var best = LocalSearch(inputs, random, startFrom = Some(initialSolution))
    var bestCost = cost(inputs, best)

    while(k <= maxK) {
      val mutated = mutate(best, n/(9-k), random)

      val newSol = LocalSearch(inputs, random, startFrom = Some(mutated))
      val newCost = cost(inputs, newSol)

      if (newCost < bestCost){
        best = newSol
        bestCost = newCost
        k = 1
      } else {
        k += 1
      }
    }

    best
  }
}
