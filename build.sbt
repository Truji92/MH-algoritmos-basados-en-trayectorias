name := "MH-algoritmos-basados-en-trayectorias"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "50", "-workers", "1", "-verbosity", "1")
