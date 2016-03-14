package algorithms

import types.Types._

object LocalSearch {

  def apply(inputs: ProblemData) = {
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

    val initialSolution = generateRandomSolution(n)

    findBestSolution(initialSolution, cost(inputs, initialSolution))
  }

  def generateAllNeighbourgs(solution: Solution) = {
    val n = solution.length
    val indexs = 0 until n

    val switchs = indexs.combinations(2)
    switchs map {
      case Vector(i, j) =>
        solution updated(i, solution(j)) updated (j, solution(i))
    } toList
  }

  def generateRandomSolution(size: Int): Solution = {
    val sol = 1 to size
    util.Random.shuffle(sol).toArray
  }

}
