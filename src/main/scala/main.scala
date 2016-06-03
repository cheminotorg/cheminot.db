package m.cheminot

object Main {

  Class.forName("org.sqlite.JDBC")

  def main(args: Array[String]): Unit = {

    implicit val config = Config(args)

    val maybeMostRecentBuild = build.GtfsBundle.mostRecent(config.gtfsDir)

    val db = build.AutoUpdate.doIt(maybeMostRecentBuild) getOrElse build.DB.mount()

    build.storage.Sqlite.create(config.dbDir, build.DB.subset(db, Nil))

    if(config.daemon) {

      build.AutoUpdate.start(db.bundle)

      http.Daemon.start(db)

    }
  }
}
