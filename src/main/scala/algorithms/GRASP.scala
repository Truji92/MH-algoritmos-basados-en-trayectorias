package algorithms

import data.Problems
import types.Types._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  *
  */
object GRASP {

  val n_trys = 10

  def apply(inputs: ProblemData, random: Random) = {
    val (n, m1, m2) = inputs

    val n_candidates = math.ceil(0.1 * n) toInt


    def generateSolution = {
      val indexs = 1 to n

      val m1Potential = m1.map(_.sum) zip indexs
      val m2Potential = m2.map(_.sum) zip indexs

      val m1Order = m1Potential sortWith {
        case ((pot1, _),(pot2, _)) => pot1 >= pot2
      }

      val m2Order = m2Potential sortWith {
        case ((pot1, _),(pot2, _)) => pot1 <= pot2
      }

      def addTupleToSolution(m1: ArrayBuffer[(Int, Int)], m2: ArrayBuffer[(Int, Int)], sol: Solution):Solution =
        if(m1.isEmpty && m2.isEmpty) sol
        else {
          val cand_1 = m1.take(n_candidates)
          val cand_2 = m2.take(n_candidates)

          val selected_1 = cand_1(random.nextInt(cand_1.length))
          val selected_2 = cand_2(random.nextInt(cand_2.length))

          (selected_1, selected_2) match {
            case ((_, i), (_, j)) => sol.update(i - 1, j)
          }

          m1 -= selected_1
          m2 -= selected_2

          addTupleToSolution(m1, m2, sol)
          }

      addTupleToSolution(m1Order.to[ArrayBuffer], m2Order.to[ArrayBuffer], Array.ofDim[Int](n))
    }

    def compare(best: (Solution, Int), newStart: Solution) = best match {
      case (best, bestCost) =>
        val newSol = LocalSearch(inputs, random, Some(newStart))
        val newCost = cost(inputs, newSol)
        if (newCost < bestCost) (newSol, newCost)
        else (best, bestCost)
    }

    val initial = generateSolution
    val initialAndCost = (initial, cost(inputs, initial))

    (1 to 10).map(_ => generateSolution).foldLeft(initialAndCost )(compare)._1
  }


}
