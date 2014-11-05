package m.cheminot

import java.io.File
import org.apache.commons.codec.binary.Base64
import org.joda.time.DateTime
import scala.util.control.Exception
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import misc._
import models._

case class DB(
  version: String,
  trips: Seq[Trip],
  stops: Seq[Stop],
  treeStops: TTreeNode[(String, String)],
  graph: Seq[Vertice],
  exceptions: Seq[CalendarDate]
)

object DB {

  def buildFrom(directory: File): DB = {
    println(s"Let's build our database from: ${directory.getAbsolutePath}")
    val start = System.currentTimeMillis

    val gtfs = GtfsDirectory(CSVDirectory(directory).read())
    val stops = gtfs.stops.map(Stop.fromRow)
    val trips = flattenTrips(gtfs.trips, gtfs.calendar, gtfs.stopTimes, gtfs.stops)
    val version = Version.generateFromDir(directory).map(_.value).getOrElse {
      Cheminot.oops("Unable to generate version from directory")
    }

    val db = DB(
      version,
      trips,
      stops,
      buildTreeStops(gtfs.stops),
      buildGraph(gtfs.stops, trips),
      gtfs.calendarDates.map(CalendarDate.fromRow)
    )

    val end = System.currentTimeMillis
    println(s"Build DONE ! [${end - start} ms]")
    db
  }

  private def buildGraph(stopsRows: CSVFile.Rows, trips: List[Trip]): Seq[Vertice] = {
    stopsRows.par.map { s =>
      val stopId = s(0)
      val stopName = s(1).substring(8)
      val z = if(Stop.STOPS_PARIS.contains(stopId)) {
        Stop.STOPS_PARIS.filterNot(_ == stopId)
      } else {
        Seq.empty[String]
      }
      val (edges, stopTimes) = trips.foldLeft((z, Seq.empty[StopTime])) { (acc, trip) =>
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
    TTreeNode(stopsRows.par.map { s =>
      val stopId = s(0)
      val stopName = s(1).substring(8)
      stopName.toLowerCase -> (stopId, stopName)
    }.toList)
  }

  private def flattenTrips(tripsRows: CSVFile.Rows, calendarRows: CSVFile.Rows, stopTimesRows: CSVFile.Rows, stopsRows: CSVFile.Rows): List[Trip] = {
    tripsRows.par.map { tripRow =>
      val routeId = tripRow(0)
      val serviceId = tripRow(1)
      val tripId = tripRow(2)
      val maybeService = calendarRows.view.find(_.head == serviceId).map(Calendar.fromRow)
      val stopTimesForTrip = stopTimesRows.collect {
        case stopTimeRow if(stopTimeRow.head == tripId) =>
          val stopId = stopTimeRow(3)
          StopTime.fromRow(stopTimeRow, stopId)
      }.toList
      Trip.fromRow(tripRow, routeId, maybeService, stopTimesForTrip)
    }.toList
  }
}

case class Version(value: String) {
  lazy val asDateTime = {
    Exception.allCatch[DateTime].opt {
      Version.format.parseDateTime(Version.decode(value))
    }
  }
}

object Version {

  private def encode(str: String): String = {
    new String(Base64.encodeBase64(str.getBytes), "UTF-8")
  }

  private def decode(str: String): String = {
    new String(Base64.decodeBase64(str.getBytes), "UTF-8")
  }

  private def parse(name: String): Option[DateTime] = {
    Exception.allCatch[DateTime].opt {
      Version.format.parseDateTime(name)
    }
  }

  val format = {
    org.joda.time.format.DateTimeFormat.forPattern("YYYY-MM-dd_HH-mm-ss")
  }

  def generateFromDir(dir: File): Option[Version] = {
    for {
      _ <- Option(dir).filter(_.isDirectory)
      value <- parse(dir.getName).map(_ => encode(dir.getName))
    } yield {
      Version(value)
    }
  }
}
