package data

object Problems {
  import Parser._

  private val baseUrl = "QAP datos/"

  private def problem(name: String) =
    (parseInputFile(baseUrl + name + ".dat"), parseSolutionFile(baseUrl + name + ".sln"))

  lazy val tai25 = problem("tai25b")

  lazy val tai150 = problem("tai150b")

  lazy val sko90 = problem("sko90")
}
