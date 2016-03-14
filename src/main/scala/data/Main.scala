package data

import algorithms._

object Main {
  def main(args: Array[String]) {
    Problems.tai25 match {
      case (inputs, solution) =>
        println("SimuAnneali: " + cost(inputs, SimulatedAnnealing(inputs)))
        println("LocalSearch: " + cost(inputs, LocalSearch(inputs)))
        //println(cost(inputs, Greedy(inputs)))
        println(solution._2)
    }
  }
}
