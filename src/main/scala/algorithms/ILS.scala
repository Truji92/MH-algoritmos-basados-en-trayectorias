package algorithms

import data.Problems
import types.Types.{Solution, ProblemData}

import scala.util.Random

/**
  * Created by alejandro on 23/04/16.
  */
object ILS {

  def apply(inputs: ProblemData, random: Random) = {
    val (n, _, _) = inputs

    val mutationSize = n/4

    val rsol = randomSolution(n, random)

    var best = LocalSearch(inputs, random, startFrom = Some(rsol))
    var bestCost = cost(inputs, best)

    for (i <- 1 until 10) {
      val mutated = mutate(best, mutationSize, random)
      val newSol = LocalSearch(inputs, random, startFrom = Some(mutated))
      val newCost = cost(inputs, newSol)

      if(newCost < bestCost) {
        best = newSol
        bestCost = newCost
      }
    }

    best
  }

}
