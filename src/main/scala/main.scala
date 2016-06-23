package org.cheminot.db

object Main {

  Class.forName("org.sqlite.JDBC")

  def main(args: Array[String]): Unit = {

    implicit val config = Config(args)

    val maybeMostRecentBuild = GtfsBundle.mostRecent(config.gtfsDir)

    val db = Upgrade.doIt(maybeMostRecentBuild)

    storage.Neo4j.write(config.dbDir, db)

    storage.Sqlite.create(config.dbDir, DB.subset(db, Nil))
  }
}
