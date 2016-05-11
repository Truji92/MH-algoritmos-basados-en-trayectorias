package algorithms

import data.Problems
import types.Types._

import scala.language.postfixOps
import scala.util.Random

/**
  *
  */
object Genetic {

  val initial_size = 50 //30-200
  val mutation_prob = 0.1 // 0.05 - 0.2

  def apply(inputs: ProblemData, random: Random) = {
    val (n, _, _) = inputs

    def generate_initial_population =
      1 to initial_size map(_ => randomSolution(n, random)) toList

    def cruzar(padre1: Solution, padre2: Solution) = {
      val lowIndex = random.nextInt(n-1)
      val highIndex = lowIndex+1 + random.nextInt(n - lowIndex-1)

      val padre1Gens = padre1.slice(lowIndex, highIndex)
      val (p2Start, p2End)= padre2.filterNot(padre1Gens.contains(_)).splitAt(lowIndex)

      println(s"($lowIndex, $highIndex) " + padre1.mkString(",")+ "||||"+ padre2.mkString(","))

      p2Start ++ padre1Gens ++ p2End
    }

  }

  def main(args: Array[String]) {
  }

}
