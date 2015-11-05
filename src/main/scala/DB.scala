package m.cheminot

import java.io.File
import org.joda.time.DateTime
import scala.util.control.Exception
import scala.concurrent.Future
import misc._
import models._

case class Subset(id: String, graph: List[Vertice], calendar: List[Calendar], calendarDates: List[CalendarDate], ttstops: TTreeNode[(String, String)])

case class DB(gtfsBundle: GtfsBundle) {

  lazy val version = gtfsBundle.version

  lazy val terTrips = DB.buildTrips(gtfsBundle.ter)

  lazy val transTrips = DB.buildTrips(gtfsBundle.trans)

  lazy val interTrips = DB.buildTrips(gtfsBundle.inter)

  lazy val stops = (gtfsBundle.ter.stops ++: gtfsBundle.trans.stops ++: gtfsBundle.inter.stops).groupBy(_.stopId).values.toList.distinct.flatten

  lazy val ttstops = DB.buildTreeStops(stops)

  lazy val ter = Subset(
    "ter",
    DB.buildGraph(gtfsBundle.ter.stops, terTrips),
    gtfsBundle.ter.calendar.map(Calendar.fromRecord),
    gtfsBundle.ter.calendarDates.map(CalendarDate.fromRecord),
    DB.buildTreeStops(gtfsBundle.ter.stops)
  )

  lazy val inter = Subset(
    "inter",
    DB.buildGraph(gtfsBundle.inter.stops, interTrips),
    gtfsBundle.inter.calendar.map(Calendar.fromRecord),
    gtfsBundle.inter.calendarDates.map(CalendarDate.fromRecord),
    DB.buildTreeStops(gtfsBundle.inter.stops)
  )

  lazy val trans = Subset(
    "trans",
    DB.buildGraph(gtfsBundle.trans.stops, transTrips),
    gtfsBundle.trans.calendar.map(Calendar.fromRecord),
    gtfsBundle.trans.calendarDates.map(CalendarDate.fromRecord),
    DB.buildTreeStops(gtfsBundle.trans.stops)
  )
}

object DB {

  type StopId = String

  def defaultDbDir: File = new File("db")

  def fromDir(directory: File): Option[DB] =
    GtfsBundle.mostRecent(root = Some(directory)).map(DB.apply)

  def fromDefaultDir(): Option[DB] =
    GtfsBundle.mostRecent().map(DB.apply)

  private def mergeVertices(z: Vertice, verticeIds: List[String], vertices: Map[StopId, Vertice], replace: Boolean): Map[StopId, Vertice] = {
    val mergedVertice: Vertice = verticeIds.flatMap(vertices.get).foldLeft(z) { (acc, vertice) =>
      val stopTimes = (vertice.stopTimes ++: acc.stopTimes).map { stopTime =>
        stopTime.copy(stopId = vertice.id)
      }

      acc.copy(
        edges = (vertice.edges ++: acc.edges).distinct,
        stopTimes = stopTimes
      )
    }

    val merged = vertices + (mergedVertice.id -> mergedVertice)

    if (replace) verticeIds.foldLeft(merged)(_ - _) else merged
  }

  private def buildGraph(stopRecords: List[StopRecord], trips: List[Trip]): List[Vertice] =
    Measure.duration("Graph") {
      val vertices: Map[StopId, Vertice] = par(stopRecords) { stopRecord =>
        val zStopTimes = Subway.stopTimes.get(stopRecord.stopId).getOrElse(Nil)
        val zEdges = Stop.parisStops.filterNot(_ == stopRecord.stopId).toList
        val (edges, stopTimes) = trips.foldLeft((zEdges, zStopTimes)) { (acc, trip) =>
          val (accEdges, accStopTimes) = acc
          val edges = trip.edgesOf(stopRecord.stopId)
          val stopTimes = trip.stopTimes.find(_.stopId == stopRecord.stopId).toList.map { st =>
            StopTime(st.tripId, st.arrival, st.departure, stopRecord.stopId, st.pos)
          }
          (edges ++: accEdges) -> (stopTimes ++: accStopTimes)
        }
        stopRecord.stopId -> Vertice(stopRecord.stopId, stopRecord.stopName, stopRecord.stopLat, stopRecord.stopLong, edges.distinct, stopTimes.distinct)
      }.toMap

      List(
        (Vertice.PARIS, Stop.parisStops, false),
        (Vertice.PARIS_LYON, Stop.parisLyon, true),
        (Vertice.PARIS_NORD, Stop.parisNord, true)
      ).foldLeft(vertices) {
        case (acc, (vertice, verticeIds, replace)) =>
          mergeVertices(vertice, verticeIds, acc, replace)
      }.values.toList
    }

  private def buildTreeStops(stopRecords: List[StopRecord], mergedStops: List[String]): TTreeNode[(String, String)] = {
    Measure.duration("TTreeStops") {
      val entries = par(stopRecords.filterNot(s => mergedStops.exists(_ == s.stopId))) { stopRecord =>
        val saintStopNames = Normalizer.handleSaintWords(stopRecord.stopName)
        val compoundStopNames = if(saintStopNames.isEmpty) Normalizer.handleCompoundWords(stopRecord.stopName) else Nil
        (stopRecord.stopName +: saintStopNames ++: compoundStopNames).distinct.filterNot(_.isEmpty).map { s =>
          s.toLowerCase -> (stopRecord.stopId, stopRecord.stopName)
        }
      }.toList.flatten
      val paris = Vertice.PARIS.name.toLowerCase -> (Stop.STOP_PARIS, Vertice.PARIS.name)
      val parisLyon = Vertice.PARIS_LYON.name.toLowerCase -> (Stop.STOP_PARIS_LYON, Vertice.PARIS_LYON.name)
      val parisNord = Vertice.PARIS_NORD.name.toLowerCase -> (Stop.STOP_PARIS_NORD, Vertice.PARIS_NORD.name)
      TTreeNode(parisNord +: parisLyon +: paris +: entries)
    }
  }

  private def buildTrips(parsed: ParsedGtfsDirectory): List[Trip] =
    Measure.duration("Trips") {
      println(s"** Trips: ${parsed.trips.size}\n** StopTimes: ${parsed.stopTimes.size}\n** Calendar: ${parsed.calendar.size}")
      par(parsed.trips, debug = true) { tripRecord =>
        val maybeService = parsed.calendar.view.find(_.serviceId == tripRecord.serviceId).map(Calendar.fromRecord)
        val stopTimesForTrip = parsed.stopTimes.collect {
          case stopTimeRecord if(stopTimeRecord.tripId == tripRecord.tripId) =>
            StopTime.fromRecord(stopTimeRecord)
        }.toList

        Trip.fromRecord(tripRecord, tripRecord.routeId, maybeService, stopTimesForTrip)
      }.toList
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
