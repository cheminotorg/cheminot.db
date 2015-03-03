package m.cheminot

import java.io.File

object Main {

  Class.forName("org.sqlite.JDBC");

  def main(args: Array[String]) {
    parser.parse(args, Config()) foreach { config =>

      val dbRootDir = config.dbdir getOrElse DB.defaultDbDir
      val gtfsRootDir = config.gtfsdir getOrElse Gtfs.defaultGtfsDir
      dbRootDir.mkdirs
      gtfsRootDir.mkdirs

      if(config.autoupdate) {
        AutoUpdate.loop(config, gtfsRootDir, dbRootDir, () => Gtfs.mostRecent(config.gtfsdir))
      } else {
        (for {
          db <- (config.gtfsdir flatMap DB.fromDir) orElse DB.fromDefault()
        } yield {
          if(config.nothing) {
            Persist.all(dbRootDir, db)
          } else {
            if(config.sqlite) {
              Persist.sqlite(dbRootDir, db.version, db.expiredAt, db.trips)
            }
            if(config.graph) {
              Persist.graph(dbRootDir, db.version, db.graph)
            }
            if(config.calendar) {
              Persist.calendarDates(dbRootDir, db.version, db.calendarDates)
            }
            if(config.ttstops) {
              Persist.ttstops(dbRootDir, db.version, db.ttstops)
            }
          }
        }) getOrElse {
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

    opt[Unit]('s', "sqlite") action { (_, config) =>
      config.copy(sqlite = true)
    } text("Build sqlite db")

    opt[Unit]('g', "graph") action { (_, config) =>
      config.copy(graph = true)
    } text("Build graph file")

    opt[Unit]('c', "calendar") action { (_, config) =>
      config.copy(calendar = true)
    } text("Build calendar dates file")

    opt[Unit]('t', "ttstops") action { (_, config) =>
      config.copy(ttstops = true)
    } text("Build TTreeStops")

    opt[File]('d', "gtfs") action { (input, config) =>
      config.copy(gtfsdir = Some(input))
    } text("Specify gtfs root directory")

    opt[File]('e', "db") action { (input, config) =>
      config.copy(dbdir = Some(input))
    } text("Specify db root directory")
  }
}

case class Config(
  sqlite: Boolean = false,
  graph: Boolean = false,
  calendar: Boolean = false,
  ttstops: Boolean = false,
  autoupdate: Boolean = false,
  gtfsdir: Option[File] = None,
  dbdir: Option[File] = None,
  twitterPseudo: Option[String] = None,
  twitterConsumerKey: Option[String] = None,
  twitterConsumerSecret: Option[String] = None,
  twitterAccessKey: Option[String] = None,
  twitterAccessSecret: Option[String] = None
) {
  def nothing = !(sqlite || graph || calendar || autoupdate)
  val twitterOAuth =
    for {
      a <- twitterConsumerKey
      b <- twitterConsumerSecret
      c <- twitterAccessKey
      d <- twitterAccessSecret
    } yield misc.TwitterOAuth(a, b, c , d)
}
