package algorithms

import types.Types._

import scala.util.Random


object TabuSearch {

  type TabuAction = (Int, Int) // (i x(i))

  def apply(inputs: ProblemData, random: Random) = {
    val (n, m1, m2) = inputs
    val memory = new LongMemory(n)

    val reInitIterations = 8 * n
    val maxReinitializations = 4
    val neightboursPerIteration = 40

    def reinitialize(best: Solution) = {
      val prob = random.nextDouble()
      if (prob < 0.25) generateRandomSol(random, n)
      else if (prob < 0.75) memory.generateInfrecuenteSol
      else best
    }

    def find(initialSol: Solution, best: Solution, bestCost: Int, tabuList: TabuList, iterations: Int): (Solution, Solution, Int) = {

      def generateNeightbours(sol: Solution) = {

        val n = sol.length
        val indexs = 0 until n

        val switchs = random.shuffle(indexs.combinations(2)) take neightboursPerIteration
        switchs map {
          case Vector(i, j) =>
            val newS = sol updated(i, sol(j))
            newS update (j, sol(i))

            val tabuActions = List ( (i, newS(i)), (j, newS(j)) )
            (newS, tabuActions)
        } toList
      }

      def bestNeightbour(solution: Solution) = {
        val all = generateNeightbours(solution)

        val allSortedByCost = all map {
          case (neig, tabuActions) => (neig, tabuActions, cost(inputs, neig))
        } sortBy{ case (_,_,cost) => cost }

        allSortedByCost find {
          case(neig, tabuActions, cost) =>
            cost < bestCost ||
            tabuActions.forall(!tabuList.isTabu(_))
        }
      }

      if (iterations == 0) (initialSol, best, bestCost)
      else {
        bestNeightbour(initialSol) match {
          case Some(solutionInfo) => {
            val (sol, tabuActions, cost) = solutionInfo

            tabuList.addAll(tabuActions)
            tabuActions foreach {case (i, xi) => memory increment(i, xi)}

            if (cost < bestCost) find(sol, sol, cost, tabuList, iterations - 1)
            else find(sol, best, bestCost, tabuList, iterations - 1)
          }
          case None =>
            find(initialSol, best, bestCost, tabuList, iterations - 1)
        }
      }
    }

    def iterate(currentSol: Solution, best: Solution, bestCost: Int, reInitCount: Int, tabuList: TabuList): Solution = {
      if (reInitCount == maxReinitializations) best
      else {
        val (_, newBest, newBestCost) = find(currentSol, best, bestCost, tabuList, reInitIterations)
        val newStart = reinitialize(newBest)

        val prob = random.nextDouble()
        if (prob < 0.5) tabuList.grow() else tabuList.decrease()
        tabuList.clear()

        iterate(newStart, newBest, newBestCost, reInitCount + 1, tabuList)
      }
    }

    val initialSol = Greedy(inputs)

    iterate(initialSol, initialSol, cost(inputs, initialSol), 0, new TabuList(n/2))

  }

  def generateRandomSol(random: Random, size: Int) = random.shuffle(1 to size).toArray

}

class TabuList(InitialSize: Int) {

  import TabuSearch.TabuAction

  var elements = Array.ofDim[TabuAction](16*InitialSize)
  var size = 2*InitialSize
  var count = 0

  def addAll(actions: List[TabuAction]) = {
    actions foreach add
  }

  private def add(action: TabuAction) = {
    elements.update(count % size, action)
    count = count + 1
  }

  def isTabu(action: TabuAction) = elements.take(size).contains(action)

  def grow() = size = size + size / 2

  def decrease() = size = size / 2

  def clear() = {
    elements = Array.ofDim[TabuAction](16*size)
    count = 0
  }

}

class LongMemory(size: Int) {
  val frecc: Matrix = Array.ofDim[Int](size, size)

  def increment(i: Int, j: Int) = frecc(i)(j-1) += 1

  def generateInfrecuenteSol: Solution = {
    var posibilities = ( for {
      i <- 0 until size
      j <- 0 until size
    } yield (i, j, frecc(i)(j)) ) sortBy(t => t._3)

    val solution = 1 to size map( _ => -1) toArray

    while (solution.contains(-1)) {
      posibilities = posibilities dropWhile {
        case (i, j, f) => solution.contains(j) || solution(i) != -1
      }

      val (i, j, _) = posibilities(0)
      solution update(i, j)
    }

    solution map (_ + 1)
  }

}