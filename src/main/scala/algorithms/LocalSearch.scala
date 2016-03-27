package algorithms

import types.Types._

import scala.util.Random

object LocalSearch {

  def apply(inputs: ProblemData, random: Random) = {
    val (n, m1, m2) = inputs

    def findBestNeighbourg(current: Solution) = {
      val neighbourgs = generateAllNeighbourgs(current)

      neighbourgs.par minBy(cost(inputs, _))
    }

    def findBestSolution(current: Solution, currentCost: Int): Solution = {
      val best = findBestNeighbourg(current)

      val bestCost = cost(inputs, best)

      if (bestCost < currentCost)
        findBestSolution(best, bestCost)
      else
        current
    }

    val initialSolution = generateRandomSolution(n, random)

    findBestSolution(initialSolution, cost(inputs, initialSolution))
  }

  def generateAllNeighbourgs(solution: Solution) = {
    val n = solution.length
    val indexs = 0 until n

    val switchs = indexs.combinations(2)
    switchs map {
      case Vector(i, j) =>
        val newS = solution updated(i, solution(j))
        newS update (j, solution(i))
        newS
    } toList
  }

  def generateRandomSolution(size: Int, random: Random): Solution = {
    val sol = 1 to size
    random.shuffle(sol).toArray
  }

}
