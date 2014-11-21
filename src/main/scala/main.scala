package m.cheminot

import java.io.File

object Main {

  Class.forName("org.sqlite.JDBC");

  def main(args: Array[String]) {
    parser.parse(args, Config()) foreach { config =>
      for {
        directory <- config.directory
        db <- DB.fromDir(directory) orElse DB.fromDefault()
      } yield {
        if(config.nothing) {
          Persist.sqlite(db.version, db.trips)
          Persist.graph(db.version, db.graph)
          Persist.calendarDates(db.version, db.calendarDates)
        } else {
          config.sqlite.foreach { _ =>
            Persist.sqlite(db.version, db.trips)
          }
          config.graph.foreach { _ =>
            Persist.graph(db.version, db.graph)
          }
          config.calendar.foreach { _ =>
            Persist.calendarDates(db.version, db.calendarDates)
          }
        }
      }
    }
  }

  val parser = new scopt.OptionParser[Config]("scopt") {
    head("cheminotdb", "0.1.0")

    help("help") text("prints this usage text")

    opt[Boolean]('s', "sqlite") action { (input, config) =>
      config.copy(sqlite = Some(input))
    } text("Generate sqlite db")

    opt[Boolean]('g', "graph") action { (input, config) =>
      config.copy(graph = Some(input))
    } text("Serialize graph")

    opt[Boolean]('c', "calendar") action { (input, config) =>
      config.copy(calendar = Some(input))
    } text("Serialize calendar")

    opt[File]('d', "directory") action { (input, config) =>
      config.copy(directory = Some(input))
    } text("Specify working directory")
  }
}

case class Config(
  sqlite: Option[Boolean] = None,
  graph: Option[Boolean] = None,
  calendar: Option[Boolean] = None,
  directory: Option[File] = None
) {
  def nothing = sqlite.isEmpty && graph.isEmpty && calendar.isEmpty
}
