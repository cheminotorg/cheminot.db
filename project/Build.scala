import sbt._, Keys._

object CheminotDB extends Build {

  lazy val bot = Project(
    id = "cheminot-db",
    base = file("."),
    settings = Defaults.defaultSettings ++ Seq(
      version := "0.1",
      scalaVersion := "2.11.7",
      resolvers += "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/",
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-json" % "2.4.0-M2",
        "com.typesafe.play" %% "anorm" % "2.4.0-M2",
        "commons-io" % "commons-io" % "2.4",
        "org.xerial" % "sqlite-jdbc" % "3.8.6",
        "com.github.scopt" %% "scopt" % "3.2.0",
        "org.scalaj" %% "scalaj-http" % "1.1.4",
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
        "com.google.protobuf" % "protobuf-java" % "3.0.0-beta-1"
      ),
      scalacOptions ++= Seq("-language:_", "-deprecation", "-unchecked", "-encoding", "UTF-8")
    )
  ).settings(com.github.retronym.SbtOneJar.oneJarSettings: _*)
}
