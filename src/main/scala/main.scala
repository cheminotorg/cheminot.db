package m.cheminot

object Main {

  Class.forName("org.sqlite.JDBC")

  def main(args: Array[String]): Unit = {

    implicit val config = Config(args)

    val db = build.AutoUpdate.doIt() getOrElse build.DB.empty //build.DB.setup()

    if(config.daemon) {

      build.AutoUpdate.start(db.bundle)

      http.Daemon.start(db)

    }
  }
}
