package algorithms

import data.Problems
import types.Types._

import scala.util.Random

/**
  *
  */
object GeneticCHC {

  val population_size = 140

  val generations = 500

  def apply(inputs: ProblemData, random: Random) = {
    val (n, _, _) = inputs

    var umbral_distancia: Int = n/4

    val initialPopulation =
      1 to population_size map(_ => {
      val sol = randomSolution(n, random)
      (sol, cost(inputs, sol))
    }) toArray

    def nextGeneration(population: Population): Population = {
      val children = random.shuffle(population.toSeq).grouped(2).map {
        case Seq(p1,p2) => cruzar(p1,p2)
        case Seq(_) => None
      } collect {
        case Some(child) => child
      }

      //println(s"children lenght ${children.length}")

      if (children isEmpty) umbral_distancia -= 1

      //println(umbral_distancia)

      if (umbral_distancia < 0) {
        umbral_distancia = n/4
        println("reset")
        diverge(population)
      }
      else elitistCombination(population, children)
    }

    def elitistCombination(oldPop: Population, children: Iterator[Individuo]) =
      (oldPop ++ children) sortBy(_._2) take oldPop.length

    def cruzar(padre1: Solution, padre2: Solution) =
      if (hammingDistance(padre1, padre2)/2 > umbral_distancia){
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

    def diverge(population: Population): Population = {
      val newPop = (1 until population.length).map(_ => {
        val newSol = randomSolution(n, random)
        (newSol, cost(inputs, newSol))
      })
      val best = population.minBy(_._2)

      (newPop :+ best) toArray
    }

    val lastGeneration = (1 to generations).foldLeft(initialPopulation)((pop,_) => nextGeneration(pop))

    lastGeneration.minBy(_._2)
  }

  def main(args: Array[String]) {
    val p = Problems.tai25
    val ge = GeneticCHC(p._1, new Random())._2
    val opt = p._2._2

    println(s"Genetic: $ge")
    println(s"Opt: $opt")
  }
}
