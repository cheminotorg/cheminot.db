package m.cheminot.build

import org.joda.time.DateTime
import scala.util.control.Exception
import scala.concurrent.Future
import m.cheminot.Config
import rapture.fs._

case class Subset(
  id: String,
  graph: Map[StopId, Vertice],
  calendar: List[Calendar],
  calendarDates: List[CalendarDate]
)

case class DB(
  id: DbId,
  graph: Map[VerticeId, Vertice],
  trips: Map[TripId, Trip],
  calendarDates: List[CalendarDate],
  calendar: List[Calendar],
  bundle: GtfsBundle
)

object DB {

  def apply(gtfsBundle: GtfsBundle): DB = {
    val (graph: Map[VerticeId, Vertice], trips: Map[TripId, Trip]) =
      Builder.build(gtfsBundle)

    val calendar: List[Calendar] =
      gtfsBundle.data.calendar.map(Calendar.fromRecord)

    val calendarDates: List[CalendarDate] =
      gtfsBundle.data.calendarDates.map(CalendarDate.fromRecord).filter { calendarDate =>
        calendar.exists(_.serviceId == calendarDate.serviceId)
      }

    DB("world", graph, trips, calendarDates, calendar, gtfsBundle)
  }

  def defaultDbDir: FileUrl = File.currentDir / "db"

  def fromDir(directory: FileUrl): Option[DB] =
    GtfsBundle.mostRecent(root = Option(directory)).map(DB.apply)

  def fromDefaultDir(): Option[DB] =
    GtfsBundle.mostRecent().map(DB.apply)

  def subset(db: DB, verticeIds: Seq[String]): DB = {

    val trips = db.trips.filter {
      case (tripId, trip) =>
        trip.stopTimes.exists(stopTime => verticeIds.exists(_ == stopTime.stopId))
    }

    val calendarDates = db.calendarDates.filter { calendarDate =>
      trips.values.toList.exists(_.calendar.exists(c => c.serviceId == calendarDate.serviceId))
    }

    val calendar = db.calendar.filter { calendar =>
      trips.values.toList.exists(_.calendar.exists(c => c.serviceId == calendar.serviceId))
    }

    val id = verticeIds.sortBy(identity).mkString("#")

    db.copy(
      id = id,
      trips = trips,
      calendar = calendar,
      calendarDates = calendarDates
    )
  }

  def empty: DB = DB(
    id = "empty",
    graph = Map.empty,
    trips = Map.empty,
    calendarDates = Nil,
    calendar = Nil,
    bundle = GtfsBundle.empty
  )

  def setupEmbed(embedDb: DB)(implicit config: Config): FileUrl =
    storage.Sqlite.create(config.dbDir, embedDb)

  def setup()(implicit config: Config): DB = {
    DB.fromDir(config.gtfsDir).map { db =>
      storage.Neo4j.write(config.dbDir, db)
      db
    } getOrElse sys.error(s"Unable to found gtfs directory from ${config.gtfsDir}")
  }
}
