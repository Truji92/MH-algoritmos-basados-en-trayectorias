package data

import types.Types._

import scala.io.Source

object Parser {

  def parseInputFile(path: String): ProblemData = {
    val lines = loadResource(path).getLines.toList

    val nItems::fullMatrixs = lines flatMap parseNumbersFromLine
    val m1::m2::_ = fullMatrixs.grouped(nItems).toList.grouped(nItems).toList
    (nItems, m1, m2)
  }

  def parseSolutionFile(path: String): OptimalSolution = {
    val lines = loadResource(path).getLines.toList

    val nItems::cost::values = lines flatMap parseNumbersFromLine
    (nItems, cost, values)
  }

  private def parseNumbersFromLine(line: String) = {
    line.split("""\s+""").filterNot(_ == "").map(_.trim.toInt)
  }

  private def loadResource(path: String) = Source.fromURL(getClass.getResource(path))
}
