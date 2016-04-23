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

    def mutate(solution: Solution) = {
      val subListStart = random.nextInt(solution.length - mutationSize)

      val subList = solution.slice(subListStart, subListStart + mutationSize)

      val newSubList = random.shuffle(subList.toList).toArray

      val newSolution = solution.clone()
      for (i <- subListStart until subListStart + mutationSize) {
        newSolution.update(i, newSubList(i-subListStart))
      }
      newSolution
    }

    val rsol = randomSolution(n, random)

    var best = LocalSearch(inputs, random, startFrom = Some(rsol))
    var bestCost = cost(inputs, best)

    for (i <- 1 until 10) {
      val mutated = mutate(best)
      val newSol = LocalSearch(inputs, random, startFrom = Some(mutated))
      val newCost = cost(inputs, newSol)

      if(newCost < bestCost) {
        best = newSol
        bestCost = newCost
      }
    }

    best
  }

  def randomSolution(size: Int, random: Random) = {
    val sol = 1 to size
    random.shuffle(sol).toArray
  }


}
