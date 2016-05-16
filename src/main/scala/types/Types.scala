package types

object Types {
  type Matrix = Array[Array[Int]]
  type ProblemData = (Int, Matrix, Matrix)
  type OptimalSolution = (Int, Int, Solution)
  type Solution = Array[Int]

  type Individuo = (Solution, Int)
  type Population = Array[Individuo]

  implicit def individuo2Solution(individuo: Individuo): Solution = individuo._1
}
