import types.Types.{Solution, OptimalSolution, ProblemData}

package object algorithms {

  def cost(problem: ProblemData, solution: Solution): Int = {
    val (nItems, m1, m2) = problem
    val sumatory = for {
      i <- 0 until nItems
      j <- 0 until nItems
    } yield m1(i)(j) * m2(solution(i)-1)(solution(j)-1)
    sumatory.sum
  }

}
