package m.cheminot

import java.io.File
import org.joda.time.DateTime
import scala.util.control.Exception
import scala.concurrent.Future
import models._

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
  meta: Meta
)

case class Meta(bundleId: BundleId, subsets: List[MetaSubset]) {
  lazy val id: String = bundleId.value
  lazy val bundleDate: DateTime = bundleId.date
}

case class MetaSubset(id: String, updatedDate: DateTime, startDate: DateTime, endDate: DateTime)

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

    val metaSubsets = gtfsBundle.data.subsetDirs.map { subsetDir =>
      MetaSubset(
        id = subsetDir.id,
        updatedDate = subsetDir.updatedDate,
        startDate = subsetDir.startDate,
        endDate = subsetDir.endDate
      )
    }

    val meta = Meta(gtfsBundle.bundleId, metaSubsets)

    DB("world", graph, trips, calendarDates, calendar, meta)
  }

  def defaultDbDir: File = new File("db")

  def fromDir(directory: File): Option[DB] =
    GtfsBundle.mostRecent(root = Option(directory)).map(DB.apply)

  def fromDefaultDir(): Option[DB] =
    GtfsBundle.mostRecent().map(DB.apply)

  def subset(id: String, db: DB, verticeIds: Seq[String]): DB = {

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
      id = s"${db.id}#${id}",
      trips = trips,
      calendar = calendar,
      calendarDates = calendarDates
    )
  }
}
