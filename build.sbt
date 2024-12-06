ThisBuild / scalaVersion := "2.13.12"

val scalaTest = libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19"

lazy val adventureOfCode = (project in file("adventure-of-code")).settings(scalaTest)
lazy val topInterview150 = (project in file("top-interview-150")).settings(scalaTest)
lazy val hackerRank = (project in file("hacker-rank")).settings(scalaTest)
lazy val leetcode75 = (project in file("leetcode-75")).settings(scalaTest)
lazy val leetcodePatterns = (project in file("leetcode-patterns")).settings(scalaTest)