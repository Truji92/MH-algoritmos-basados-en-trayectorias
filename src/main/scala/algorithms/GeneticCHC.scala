package algorithms

import types.Types._

import scala.util.Random

/**
  *
  */
object GeneticCHC {

  val population_size = 150

  def apply(inputs: ProblemData, random: Random) = {
    val (n, _, _) = inputs

    val umbral_distancia = n/4

    val initialPopulation =
      1 to population_size map(_ => {
      val sol = randomSolution(n, random)
      (sol, cost(inputs, sol))
    }) toArray

    def nextGeneration(population: Population) = {

    }

    def cruzar(padre1: Solution, padre2: Solution) =
      if (hammingDistance(padre1, padre2) > umbral_distancia){
        val lowIndex = random.nextInt(n-1)
        val highIndex = lowIndex+1 + random.nextInt(n - lowIndex-1)

        val padre1Gens = padre1.slice(lowIndex, highIndex)
        val (p2Start, p2End)= padre2.filterNot(padre1Gens.contains(_)).splitAt(lowIndex)

        val child = p2Start ++ padre1Gens ++ p2End
        Some((child, cost(inputs, child)))
      } else None
    
    def hammingDistance(first: Solution, second: Solution) =
      (first zip second).foldLeft(0) {
        case (acc, (i1, i2)) => if (i1 == i2) acc + 1 else acc
      }

  }
}
