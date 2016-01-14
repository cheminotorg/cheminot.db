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
        "anormcypher" at "http://repo.anormcypher.org/"
      ),
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-json" % "2.4.0-M2",
        "com.typesafe.play" %% "anorm" % "2.4.0-M2",
        "commons-io" % "commons-io" % "2.4",
        "com.github.scopt" %% "scopt" % "3.2.0",
        "org.scalaj" %% "scalaj-http" % "1.1.4",
        "com.github.tototoshi" %% "scala-csv" % "1.2.2",
        "org.xerial" % "sqlite-jdbc" % "3.8.6"
      ),
      scalacOptions ++= Seq("-language:_", "-deprecation", "-unchecked", "-encoding", "UTF-8")
    )
  ).settings(com.github.retronym.SbtOneJar.oneJarSettings: _*)
}
