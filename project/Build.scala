import sbt._, Keys._

object CheminotDB extends Build {

  lazy val bot = Project(
    id = "cheminot-db",
    base = file("."),
    settings = Defaults.defaultSettings ++ Seq(
      version := "0.1",
      scalaVersion := "2.11.7",
      resolvers ++= Seq(
        "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/",
        "Local ivy repository" at s"""file://${Path.userHome.absolutePath}/.ivy2/local"""
      ),
      libraryDependencies ++= Seq(
        "com.propensive" %% "rapture" % "2.0.0-SNAPSHOT" exclude("com.propensive", "rapture-json-lift_2.11"),
        "com.typesafe.play" %% "anorm" % "2.4.0-M2",
        "com.github.tototoshi" %% "scala-csv" % "1.2.2",
        "org.xerial" % "sqlite-jdbc" % "3.8.6",
        "org.scala-stm" %% "scala-stm" % "0.7"
      ),
      scalacOptions ++= Seq("-encoding", "UTF-8", "-feature", "-Xlint", "-Ywarn-unused-import", "-Ywarn-dead-code")
    )
  ).settings(com.github.retronym.SbtOneJar.oneJarSettings: _*)
}
