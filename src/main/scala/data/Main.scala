package data

import algorithms._
import types.Types.ProblemData
import java.io._

import scala.util.Random

object Main {
  def main(args: Array[String]) {
    Problems.tai150 match {
      case (inputs, solution) =>
        println("*******************************************************************")
        //println("Simulated Annealing:  " + cost(inputs, SimulatedAnnealing(inputs)))
        //println("Local Search:         " + cost(inputs, LocalSearch(inputs)))
        println("Tabu:                 " + cost(inputs, TabuSearch(inputs, new Random)))
        println("Greedy:               " + cost(inputs, Greedy(inputs)))
        //println("Random Search:        " + cost(inputs, RandomSearch(inputs)))
        println("Optimal:              " + solution._2)
        println("*******************************************************************")
    }
    //generateTestingFiles()
  }

  def generateTestingFiles() = {

    //================================================
    var pw = new PrintWriter(new File("results150" ))

    pw.write("SEED, LOCALSEARCH, RANDOMSEARCH, SIMANNEALING \n")

    for (i <- 1 to 10) {
      val (inputs, _) = Problems.tai150
      val local = LocalSearch(inputs, new Random(i))
      pw.write(i + ", " + cost(inputs, local))

      val random = RandomSearch(inputs, new Random(i))
      pw.write(", " + cost(inputs, random))

      val simu = SimulatedAnnealing(inputs, new Random(i))
      pw.write(", " + cost(inputs, simu) + "\n")
    }

    pw.close()
    //================================================


    //================================================
    pw = new PrintWriter(new File("results90" ))

    pw.write("SEED, LOCALSEARCH, RANDOMSEARCH, SIMANNEALING \n")

    for (i <- 1 to 10) {
      val (inputs, _) = Problems.sko90
      val local = LocalSearch(inputs, new Random(i))
      pw.write(i + ", " + cost(inputs, local))

      val random = RandomSearch(inputs, new Random(i))
      pw.write(", " + cost(inputs, random))

      val simu = SimulatedAnnealing(inputs, new Random(i))
      pw.write(", " + cost(inputs, simu) + "\n")
    }

    pw.close()
    //================================================



    //================================================
    pw = new PrintWriter(new File("results25" ))

    pw.write("SEED, LOCALSEARCH, RANDOMSEARCH, SIMANNEALING \n")

    for (i <- 1 to 10) {
      val (inputs, _) = Problems.tai25
      val local = LocalSearch(inputs, new Random(i))
      pw.write(i + ", " + cost(inputs, local))

      val random = RandomSearch(inputs, new Random(i))
      pw.write(", " + cost(inputs, random))

      val simu = SimulatedAnnealing(inputs, new Random(i))
      pw.write(", " + cost(inputs, simu) + "\n")
    }

    pw.close()
    //================================================

  }
}
