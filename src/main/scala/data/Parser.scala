package data

import types.Types._

import scala.io.Source

object Parser {

  def parseInputFile(path: String): ProblemData = {
    val lines = loadResource(path).getLines.toList

    val nItems::fullMatrixs = lines flatMap parseNumbersFromLine
    val m1::m2::_ = fullMatrixs.grouped(nItems).map(_.toArray).toList.grouped(nItems).toList
    (nItems, m1.toArray, m2.toArray)
  }

  def parseSolutionFile(path: String): OptimalSolution = {
    val lines = loadResource(path).getLines.toList

    val nItems::cost::values = lines flatMap parseNumbersFromLine
    (nItems, cost, values.toArray)
  }

  private def parseNumbersFromLine(line: String) = {
    line.split("""\s+""").filterNot(_ == "").map(_.trim.toInt)
  }

  private def loadResource(path: String) = Source.fromURL(getClass.getClassLoader.getResource(path))
}
