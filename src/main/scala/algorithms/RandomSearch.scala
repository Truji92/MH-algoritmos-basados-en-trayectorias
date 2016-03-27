package algorithms

import types.Types.{Solution, ProblemData}

import scala.util.Random

object RandomSearch {

  val iterationsFactor = 1600

  def apply(inputs: ProblemData, random: Random) = {
    val (n, _, _)= inputs

    val iterations = iterationsFactor*n

    def iterate(best: Solution, bestCost: Int, iteration: Int): Solution = iteration match {
      case 0 => best
      case _ =>
        val newSolution = generateRandomSolution(n, random)
        val newCost = cost(inputs, newSolution)
        if (newCost <= bestCost) iterate(best, bestCost, iteration-1)
        else iterate(newSolution, newCost, iteration-1)
    }

    val initial = generateRandomSolution(n, random)
    iterate(initial, cost(inputs, initial), iterations)
  }

  def generateRandomSolution(size: Int, random: Random): Solution = {
    val sol = 1 to size
    random.shuffle(sol).toArray
  }
}
