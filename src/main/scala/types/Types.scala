package types

object Types {
  type Matrix = Array[Array[Int]]
  type ProblemData = (Int, Matrix, Matrix)
  type OptimalSolution = (Int, Int, Solution)
  type Solution = Array[Int]
}
