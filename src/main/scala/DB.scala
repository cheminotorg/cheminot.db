package m.cheminot

import java.io.File
import org.joda.time.DateTime
import scala.util.control.Exception
import scala.concurrent.Future
import models._

case class Subset(id: String, graph: Map[StopId, Vertice], calendar: List[Calendar], calendarDates: List[CalendarDate])

case class DB(id: String, version: Version, graph: Map[VerticeId, Vertice], trips: Map[TripId, Trip], stops: List[Vertice], calendarDates: List[CalendarDate], calendar: List[Calendar])

object DB {

  def apply(gtfsBundle: GtfsBundle): DB = {
    val version: Version = gtfsBundle.version

    val (graph: Map[VerticeId, Vertice], trips: Map[TripId, Trip]) = Builder.build(gtfsBundle)

    val stops: List[Vertice] = graph.values.toList

    val calendar: List[Calendar] =
      gtfsBundle.data.calendar.map(Calendar.fromRecord)

    val calendarDates: List[CalendarDate] =
      gtfsBundle.data.calendarDates.map(CalendarDate.fromRecord).filter { calendarDate =>
        calendar.exists(_.serviceId == calendarDate.serviceId)
      }

    DB("world", version, graph, trips, stops, calendarDates, calendar)
  }

  def defaultDbDir: File = new File("db")

  def fromDir(directory: File): Option[DB] =
    GtfsBundle.mostRecent(root = Option(directory)).map(DB.apply)

  def fromDefaultDir(): Option[DB] =
    GtfsBundle.mostRecent().map(DB.apply)

  def subset(id: String, db: DB, verticeIds: Seq[String]): DB = {

    val graph = verticeIds.foldLeft(Map.empty[VerticeId, Vertice]) {
      case (acc, verticeId) =>
        db.graph.get(verticeId).map { vertice =>
          acc + (verticeId -> vertice)
        } getOrElse acc
    }

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

    db.copy(
      id = id,
      graph = graph,
      trips = trips,
      calendar = calendar,
      calendarDates = calendarDates
    )
  }
}

case class Version(date: DateTime) {
  lazy val value: String = Version.formatter.print(date)
}

object Version {

  private def parse(name: String): Option[DateTime] = {
    Exception.allCatch[DateTime].opt {
      Version.formatter.parseDateTime(name)
    }
  }

  val formatter = org.joda.time.format.DateTimeFormat.forPattern("yyyyMMddHHmmss")

  def fromDir(dir: File): Option[Version] = {
    for {
      _ <- Option(dir).filter(_.isDirectory)
      date <- parse(dir.getName)
    } yield {
      Version(date)
    }
  }
}
