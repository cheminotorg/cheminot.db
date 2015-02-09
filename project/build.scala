import sbt._, Keys._

object CheminotDB extends Build {

  lazy val bot = Project(
    id = "cheminot-db",
    base = file("."),
    settings = Defaults.defaultSettings ++ Seq(
      version := "0.1",
      scalaVersion := "2.11.4",
      resolvers += "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/",
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-json" % "2.4.0-M1",
        "com.typesafe.play" %% "anorm" % "2.4.0-M1",
        "commons-codec" % "commons-codec" % "1.9",
        "org.xerial" % "sqlite-jdbc" % "3.8.6",
        "com.github.scopt" %% "scopt" % "3.2.0"
      ),
      scalacOptions ++= Seq("-language:_", "-deprecation", "-unchecked"))
  ).settings(com.github.retronym.SbtOneJar.oneJarSettings: _*)
}
