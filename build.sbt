enablePlugins(GitVersioning, GitBranchPrompt)

lazy val buildSettings = Seq(
  organization := "org.cheminot",
  scalaVersion := "2.11.7",
  crossPaths := false
)

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-Xlint",
    "-Ywarn-dead-code",
    "-Ywarn-unused-import"
  ),
  scalacOptions in (Compile, console) ~= (_ filterNot (_ == "-Ywarn-unused-import"))
)

lazy val cheminotorgSettings = buildSettings ++ commonSettings

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(name := "cheminotdb").
  settings(cheminotorgSettings:_*).
  settings(libraryDependencies += "com.propensive" %% "rapture" % "2.0.0-SNAPSHOTS").
  settings(libraryDependencies += "com.propensive" %% "rapture-http-jetty" % "2.0.0-SNAPSHOTS").
  settings(libraryDependencies += "com.typesafe.play" %% "anorm" % "2.4.0-M2").
  settings(libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.2.2").
  settings(libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.8.6").
  settings(libraryDependencies += "org.scala-stm" %% "scala-stm" % "0.7").
  settings(com.github.retronym.SbtOneJar.oneJarSettings: _*)

// versioning
git.baseVersion := "0.1.0"
git.useGitDescribe := true
git.formattedShaVersion := git.gitHeadCommit.value map { sha => s"$sha".take(7) }

resolvers ++= Seq(
  Resolver.file("local", file(Path.userHome.absolutePath + "/.ivy2/local"))(Resolver.ivyStylePatterns),
  Resolver.sonatypeRepo("snapshots"),
  Resolver.typesafeRepo("releases")
)
