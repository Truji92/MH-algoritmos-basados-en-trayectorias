package algorithms

import types.Types.{Solution, ProblemData}

import scala.util.Random

object SimulatedAnnealing {

  val iterationFactor = 80
  val mu = 0.3
  val phi = 0.3
  val maxNeighbours = 20

  def apply(inputs: ProblemData, random: Random) = {
    val (n, _, _) = inputs
    val maxIterations = iterationFactor * n

    val initialSolution = Greedy(inputs)
    val initialCost = cost(inputs, initialSolution)

    val T0 = initialTemp(initialCost)

    def coolingIteration(T: Double, best: Solution, bestCost: Int, currentSol: Solution, currentCost: Int, iteration: Int): Solution = {

      def analizeNeighbours(best: Solution, bestCost: Int, currentSol: Solution, currentCost: Int, neighboursLeft: Int): (Solution, Int, Solution, Int) = {
        if (neighboursLeft == 0) (best, bestCost, currentSol, currentCost)
        else {
          val newS = generateNeighbour(currentSol, n, random)
          val newCost = cost(inputs, newS)

          if(newCost < bestCost)
            analizeNeighbours(newS, newCost, newS, newCost, neighboursLeft - 1)
          else if (random.nextDouble() <= acceptanceProbability(currentCost, newCost, T))
            analizeNeighbours(best, bestCost, newS, newCost, neighboursLeft - 1)
          else analizeNeighbours(best, bestCost, currentSol, currentCost, neighboursLeft - 1)

        }
      }

      if (iteration == maxIterations) best
      else {
        val (b, bC, c, cC) = analizeNeighbours(best, bestCost, currentSol, currentCost, maxNeighbours)
        coolingIteration(cool(T, iteration), b, bC, c, cC, iteration + 1)
      }
    }

    coolingIteration(T0, initialSolution, initialCost, initialSolution, initialCost, 1)
  }

  def initialTemp(initialCost: Int) = initialCost * mu / (- math.log(phi))

  def acceptanceProbability(currentCost: Int, newCost: Int, T: Double) = {
    val diff = newCost - currentCost
    if (diff < 0) 1.0
    else math.exp(-diff/T)
  }

  def generateNeighbour(sol: Solution, n: Int, random: Random) = {
    val i = random.nextInt(n)
    val j = random.nextInt(n)
    val newSol = sol updated(i, sol(j))
    newSol update(j, sol(i))
    newSol
  }

  def cool(T: Double, k: Int) = T / (1 + k)

}
