package m.cheminot

import java.io.File
import org.apache.commons.codec.binary.Base64
import org.joda.time.DateTime
import scala.util.control.Exception
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import misc._
import models._

case class DB(gtfs: GtfsDirectory) {
  lazy val version: Version = gtfs.version
  lazy val trips: List[Trip] = DB.buildTrips(gtfs)
  lazy val treeStops: TTreeNode[(String, String)] = DB.buildTreeStops(gtfs.stops)
  lazy val graph: List[Vertice] = DB.buildGraph(gtfs.stops, trips)
  lazy val calendarDates: List[CalendarDate] = gtfs.calendarDates.map(CalendarDate.fromRow)
  lazy val ttstops: TTreeNode[(String, String)] = DB.buildTreeStops(gtfs.stops)
  lazy val expiredAt: DateTime = calendarDates.sortBy(-_.date.getMillis).head.date
}

object DB {

  def fromDir(directory: File): Option[DB] =
    Gtfs.apply(directory) map DB.apply

  def fromDefault(): Option[DB] =
    Gtfs.mostRecent() map DB.apply

  private def buildGraph(stopsRows: CSVFile.Rows, trips: List[Trip]): List[Vertice] = {
    Console.out.println("Building graph...")
    stopsRows.par.map { s =>
      val stopId = s(0)
      val stopName = s(1).substring(8)
      val z = if(Stop.parisStops.contains(stopId)) {
        Stop.parisStops.filterNot(_ == stopId)
      } else List.empty[String]
      val (edges, stopTimes) = trips.foldLeft((z, List.empty[StopTime])) { (acc, trip) =>
        val (accEdges, accStopTimes) = acc
        val edges = trip.edgesOf(stopId)
        val stopTimes = trip.stopTimes.find(_.stopId == stopId).toList.map { st =>
          StopTime(st.tripId, st.arrival, st.departure, stopId, st.pos)
        }
        (edges ++: accEdges) -> (stopTimes ++: accStopTimes)
      }
      Vertice(stopId, stopName, edges.distinct, stopTimes.distinct)
    }.toList
  }

  private def buildTreeStops(stopsRows: CSVFile.Rows): TTreeNode[(String, String)] = {
    Console.out.println("Building TTreeStops...")
    TTreeNode(stopsRows.par.map { s =>
      val stopId = s(0)
      val stopName = s(1).substring(8)
      stopName.toLowerCase -> (stopId, stopName)
    }.toList)
  }

  private def buildTrips(gtfs: GtfsDirectory): List[Trip] = {
    Console.out.println("Building trips...")
    gtfs.trips.par.map { tripRow =>
      val routeId = tripRow(0)
      val serviceId = tripRow(1)
      val tripId = tripRow(2)
      val maybeService = gtfs.calendar.view.find(_.head == serviceId).map(Calendar.fromRow)
      val stopTimesForTrip = gtfs.stopTimes.collect {
        case stopTimeRow if(stopTimeRow.head == tripId) =>
          val stopId = stopTimeRow(3)
          StopTime.fromRow(stopTimeRow, stopId)
      }.toList
      Trip.fromRow(tripRow, routeId, maybeService, stopTimesForTrip)
    }.toList
  }
}

case class Version(value: String, date: DateTime)

object Version {

  private def encode(str: String): String = {
    new String(Base64.encodeBase64(str.getBytes), "UTF-8")
  }

  private def decode(str: String): String = {
    new String(Base64.decodeBase64(str.getBytes), "UTF-8")
  }

  private def parse(name: String): Option[DateTime] = {
    Exception.allCatch[DateTime].opt {
      Version.formatter.parseDateTime(name)
    }
  }

  val formatter = {
    org.joda.time.format.DateTimeFormat.forPattern("YYYY-MM-dd_HH-mm-ss")
  }

  def fromDir(dir: File): Option[Version] = {
    for {
      _ <- Option(dir).filter(_.isDirectory)
      date <- parse(dir.getName)
    } yield {
      Version(encode(dir.getName), date)
    }
  }
}
