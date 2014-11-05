name := "cheminot.db"

scalaVersion := "2.11.4"

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.4.0-M1",
  "com.typesafe.play" %% "anorm" % "2.4.0-M1",
  "commons-codec" % "commons-codec" % "1.9",
  "org.xerial" % "sqlite-jdbc" % "3.8.6"
)
