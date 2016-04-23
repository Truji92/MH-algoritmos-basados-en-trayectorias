import types.Types.{Solution, OptimalSolution, ProblemData}

import scala.util.Random

package object algorithms {

  def cost(problem: ProblemData, solution: Solution): Int = {
    val (nItems, m1, m2) = problem
    val sumatory = for {
      i <- 0 until nItems
      j <- 0 until nItems
    } yield m1(i)(j) * m2(solution(i)-1)(solution(j)-1)
    sumatory.sum
  }

  def mutate(solution: Solution, mutationSize: Int, random: Random) = {
    val subListStart = random.nextInt(solution.length - mutationSize)

    val subList = solution.slice(subListStart, subListStart + mutationSize)

    val newSubList = random.shuffle(subList.toList).toArray

    val newSolution = solution.clone()
    for (i <- subListStart until subListStart + mutationSize) {
      newSolution.update(i, newSubList(i-subListStart))
    }
    newSolution
  }

  def randomSolution(size: Int, random: Random) = {
    val sol = 1 to size
    random.shuffle(sol).toArray
  }

}
