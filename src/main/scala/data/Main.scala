package data
import algorithms.cost

object Main {
  def main(args: Array[String]) {

    val (in, optimal) = Problems.tai150
val tic = System.currentTimeMillis()
    val coste = cost(in, optimal._3)
    val time = System.currentTimeMillis() - tic

    println("TIME: " + time)
    println("Coste Calculado: " + coste)
    println("Coste Real: " + optimal._2)

  }
}
