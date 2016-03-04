package types

object Types {
  type Matrix = List[List[Int]]
  type ProblemData = (Int, Matrix, Matrix)
  type OptimalSolution = (Int, Int, Solution)
  type Solution = List[Int]
}
