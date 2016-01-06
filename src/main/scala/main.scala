package m.cheminot

import java.io.File

object Main {

  def main(args: Array[String]): Unit = {
    parser.parse(args, Config()) foreach { config =>

      val dbRootDir = config.dbdir getOrElse DB.defaultDbDir
      val gtfsRootDir = config.gtfsdir getOrElse GtfsBundle.defaultRoot

      dbRootDir.mkdirs
      gtfsRootDir.mkdirs

      if(config.autoupdate) {
        AutoUpdate.loop(config, gtfsRootDir, dbRootDir, () => GtfsBundle.mostRecent(config.gtfsdir))
      } else {
        (config.gtfsdir flatMap DB.fromDir) orElse DB.fromDefaultDir().map { db =>
          storage.Neo4j.writeGraph(dbRootDir, db)
        } getOrElse {
          println("Unable to find gtfs directory")
        }
      }
    }
  }

  val parser = new scopt.OptionParser[Config]("cheminotdb") {
    head("cheminotdb", "0.1.0")

    help("help") text("prints this usage text")

    opt[Unit]('a', "autoupdate") action { (_, config) =>
      config.copy(autoupdate = true)
    } text("Auto-update mode") children(
      opt[String]("twitter-consumer-key") action { (consumerKey, config) =>
        config.copy(twitterConsumerKey = Some(consumerKey))
      },
      opt[String]("twitter-consumer-secret") action { (consumerSecret, config) =>
        config.copy(twitterConsumerSecret = Some(consumerSecret))
      },
      opt[String]("twitter-access-key") action { (accessKey, config) =>
        config.copy(twitterAccessKey = Some(accessKey))
      },
      opt[String]("twitter-access-secret") action { (accessSecret, config) =>
        config.copy(twitterAccessSecret = Some(accessSecret))
      },
      opt[String]("twitter-pseudo") action { (twitterPseudo, config) =>
        config.copy(twitterPseudo = Some(twitterPseudo))
      } text("Specify twitter pseudo to notify")
    )

    opt[Unit]('g', "graph") action { (_, config) =>
      config.copy(graph = true)
    } text("Build graph file")

    opt[File]('d', "gtfs") action { (input, config) =>
      config.copy(gtfsdir = Some(input))
    } text("Specify gtfs root directory")

    opt[File]('e', "db") action { (input, config) =>
      config.copy(dbdir = Some(input))
    } text("Specify db root directory")
  }
}

case class Config(
  graph: Boolean = false,
  autoupdate: Boolean = false,
  gtfsdir: Option[File] = None,
  dbdir: Option[File] = None,
  twitterPseudo: Option[String] = None,
  twitterConsumerKey: Option[String] = None,
  twitterConsumerSecret: Option[String] = None,
  twitterAccessKey: Option[String] = None,
  twitterAccessSecret: Option[String] = None
) {
  def nothing = !(graph || autoupdate)
  val twitterOAuth =
    for {
      a <- twitterConsumerKey
      b <- twitterConsumerSecret
      c <- twitterAccessKey
      d <- twitterAccessSecret
    } yield misc.TwitterOAuth(a, b, c , d)
}
