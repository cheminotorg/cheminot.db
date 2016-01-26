package m.cheminot

object Main {

  Class.forName("org.sqlite.JDBC")

  def main(args: Array[String]): Unit = {

    implicit val config = Config(args)

    val maybeMostRecentBuild = build.GtfsBundle.mostRecent(config.gtfsDir)

    val db = build.AutoUpdate.doIt(maybeMostRecentBuild) getOrElse build.DB.mount()

    if(config.daemon) {

      build.AutoUpdate.start(db.bundle)

      http.Daemon.start(db)

    }
  }
}
