package algorithms

import types.Types.{Solution, ProblemData}

object Greedy {

  def apply(problem: ProblemData): Solution = {
    val (n, m1, m2) = problem
    val indexs = 1 to n

    val m1Potential = m1.map(_.sum) zip indexs
    val m2Potential = m2.map(_.sum) zip indexs

    val m1Order = m1Potential sortWith {
      case ((pot1, _),(pot2, _)) => pot1 > pot2
    }

    val m2Order = m2Potential sortWith {
      case ((pot1, _),(pot2, _)) => pot1 < pot2
    }

    val res = Array.ofDim[Int](n)
    for (pair <- m1Order zip m2Order) {
      pair match {
        case ((_,i),(_,j)) => res.update(i-1, j)
      }
    }
    res
  }
}
