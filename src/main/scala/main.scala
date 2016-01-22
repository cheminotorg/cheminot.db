package m.cheminot

object Main {

  Class.forName("org.sqlite.JDBC")

  def main(args: Array[String]): Unit = {

    implicit val config = Config(args)

    //val db = build.DB.setup(config)
    val db = build.DB.empty

    if(config.daemon) {
      http.Daemon.start(db)
    }
  }
}
