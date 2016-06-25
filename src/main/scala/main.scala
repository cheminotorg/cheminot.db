package org.cheminot.db

object Main {

  Class.forName("org.sqlite.JDBC")

  def main(args: Array[String]): Unit = {

    Config.displayInfo()

    implicit val config = Config(args)

    val maybeMostRecentBuild = GtfsBundle.mostRecent(config.gtfsDir)

    val db = Upgrade.doIt(maybeMostRecentBuild)

    storage.Neo4j.write(config.dbDir, db)

    storage.Sqlite.createWithStops(config.dbDir, db)

    db.setAsCurrent(config.dbDir)
  }
}
