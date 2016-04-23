package data

import algorithms._
import types.Types.ProblemData
import java.io._

import scala.util.Random

object Main {
  def main(args: Array[String]) {
    Problems.tai25 match {
      case (inputs, solution) =>
        println("*******************************************************************")
        //println("Simulated Annealing:  " + cost(inputs, SimulatedAnnealing(inputs)))
        //println("Local Search:         " + cost(inputs, LocalSearch(inputs)))
        //println("Tabu:                 " + cost(inputs, TabuSearch(inputs, new Random)))
        println("Greedy:               " + cost(inputs, Greedy(inputs)))
        //println("Random Search:        " + cost(inputs, RandomSearch(inputs)))
        println("Optimal:              " + solution._2)
        println("GRASP:                " + cost(inputs, GRASP(inputs, new Random)))
        println("ILS:                  " + cost(inputs, ILS(inputs, new Random)))
        println("VNS:                  " + cost(inputs, VNS(inputs, new Random)))
        println("*******************************************************************")
    }
    //generateTabuFiles()
    //generateTestingFiles()
  }

  def generateTabuFiles() = {
    //================================================
    var pw = new PrintWriter(new File("Tabu150" ))
    println("=== 150 ===")
    pw.write("SEED, TABUSEARCH \n")

    for (i <- 1 to 10) {
      val (inputs, _) = Problems.tai150

      val tabu = TabuSearch(inputs, new Random(i))
      pw.write(", " + cost(inputs, tabu) + "\n")

      println(i)
    }

    pw.close()
    //================================================


    //================================================
    pw = new PrintWriter(new File("Tabu90" ))
    println("=== 90 ===")
    pw.write("SEED, LOCALSEARCH, RANDOMSEARCH, SIMANNEALING \n")

    for (i <- 1 to 10) {
      val (inputs, _) = Problems.sko90

      val tabu = TabuSearch(inputs, new Random(i))
      pw.write(", " + cost(inputs, tabu) + "\n")

      println(i)

    }

    pw.close()
    //================================================



    //================================================
    pw = new PrintWriter(new File("Tabu25" ))
    println("=== 25 ===")
    pw.write("SEED, LOCALSEARCH, RANDOMSEARCH, SIMANNEALING \n")

    for (i <- 1 to 10) {
      val (inputs, _) = Problems.tai25

      val tabu = TabuSearch(inputs, new Random(i))
      pw.write(", " + cost(inputs, tabu) + "\n")

      println(i)
    }

    pw.close()
    //================================================

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
