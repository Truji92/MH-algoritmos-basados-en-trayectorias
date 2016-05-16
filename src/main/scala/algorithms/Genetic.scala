package algorithms

import data.Problems
import types.Types._

import scala.language.{implicitConversions, postfixOps}
import scala.util.Random

/**
  *
  */
object Genetic {

  val population_size = 155 //30-200
  val cruze_prob = 0.9
  val mutation_prob = 0.2 // 0.05 - 0.2
  //val mutation_prob = 0.05 // 0.05 - 0.2
  val elite_size = 5
  val tournament_size = 0.1 * population_size toInt
  val generations = 300

  def apply(inputs: ProblemData, random: Random) = {
    val (n, _, _) = inputs

    val initial_population =
      1 to population_size map(_ => {
        val sol = randomSolution(n, random)
        (sol, cost(inputs, sol))
       }) toArray

    def cruzar(padre1: Solution, padre2: Solution) = {
      val lowIndex = random.nextInt(n-1)
      val highIndex = lowIndex+1 + random.nextInt(n - lowIndex-1)

      val padre1Gens = padre1.slice(lowIndex, highIndex)
      val (p2Start, p2End)= padre2.filterNot(padre1Gens.contains(_)).splitAt(lowIndex)

      val child = p2Start ++ padre1Gens ++ p2End
      (child, cost(inputs, child))
    }

    def mutate(individuo: Individuo) =
      if (random.nextFloat() <= mutation_prob) {
        val newSol = algorithms.mutate(individuo._1, n*0.2 toInt, random)
        (newSol, cost(inputs, newSol))
      }
      else individuo

    def getElite(population: Population) =
      population sortBy(_._2) take elite_size

    def tournament(population: Population) = {
      1 to tournament_size map(_ => {
        val index = random.nextInt(population_size)
        population(index)
      }) sortBy(_._2) take 2
    }

    def nextGeneration(population: Population) = {
      val elite = getElite(population)

      val nextPeople = 1 to (population_size - elite_size) / 2 flatMap (_ => {
        val Seq(p1, p2) = tournament(population)
        val descendants = if (random.nextFloat() <= cruze_prob)
          Array(cruzar(p1, p2), cruzar(p2, p1))
        else Array(p1, p2)

        descendants map mutate
      })

      elite ++ nextPeople
    }

    val lastPopulation = (1 to generations).foldLeft(initial_population){
      case (pop, i) => nextGeneration(pop)
    }

    lastPopulation minBy(_._2)
  }

  def main(args: Array[String]): Unit = {
    val p = Problems.tai150

    val opt = p._2._2

    val sol = Genetic(p._1, new Random)._2
    println(s"Genetico $sol")
    println(s"Optimo $opt ")

  }

}
