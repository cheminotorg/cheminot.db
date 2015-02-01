package m.cheminot

import java.io.File

object Main {

  Class.forName("org.sqlite.JDBC");

  def main(args: Array[String]) {
    parser.parse(args, Config()) foreach { config =>
      (for {
        db <- (config.directory flatMap DB.fromDir) orElse DB.fromDefault()
      } yield {
        if(config.nothing) {
          Persist.sqlite(db.version, db.trips)
          Persist.graph(db.version, db.graph)
          Persist.calendarDates(db.version, db.calendarDates)
          Persist.ttstops(db.version, db.ttstops)
        } else {
          if(config.sqlite) {
            Persist.sqlite(db.version, db.trips)
          }
          if(config.graph) {
            Persist.graph(db.version, db.graph)
          }
          if(config.calendar) {
            Persist.calendarDates(db.version, db.calendarDates)
          }
          if(config.ttstops) {
            Persist.ttstops(db.version, db.ttstops)
          }
        }
      }) getOrElse {
         Console.err.println("Unable to find gtfs directory")
      }
    }
  }

  val parser = new scopt.OptionParser[Config]("cheminotdb") {
    head("cheminotdb", "0.1.0")

    help("help") text("prints this usage text")

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

    opt[File]('d', "directory") action { (input, config) =>
      config.copy(directory = Some(input))
    } text("Specify gtfs directory")
  }
}

case class Config(
  sqlite: Boolean = false,
  graph: Boolean = false,
  calendar: Boolean = false,
  ttstops: Boolean = false,
  directory: Option[File] = None
) {
  def nothing = !(sqlite || graph || calendar)
}
