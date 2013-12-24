scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.5",
  "org.scalaz" %% "scalaz-effect" % "7.0.5",
  "org.scalaz" %% "scalaz-typelevel" % "7.0.5",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.0.5" % "test"
)

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"
