package m.cheminot

import java.io.File
import org.joda.time.DateTime
import java.util.concurrent.Executors
import scala.util.control.Exception
import scala.concurrent.{ Await, Future, ExecutionContext }
import scala.concurrent.duration._
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

  private val THREADS_PER_POOL = Option(System.getProperty("threads")).map(_.toInt).getOrElse(5)
  private implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(THREADS_PER_POOL))

  private def par[A, B](aaa: Seq[A])(f: (A) => B)(implicit ec: ExecutionContext): Seq[B] =
    Await.result(
      Future.sequence {
        aaa.grouped(THREADS_PER_POOL).toSeq.map { group =>
          Future(group.map(f))
        }
      }.map(_.flatten),
      1.hours
    )

  def defaultDbDir: File = new File("db")

  def fromDir(directory: File): Option[DB] =
    Gtfs.apply(directory) map DB.apply

  def fromDefault(): Option[DB] =
    Gtfs.mostRecent() map DB.apply

  private def buildGraph(stopsRows: CSVFile.Rows, trips: List[Trip]): List[Vertice] =
    Measure.duration("Graph") {
      par(stopsRows) { s =>
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

  private def buildTreeStops(stopsRows: CSVFile.Rows): TTreeNode[(String, String)] =
    Measure.duration("TTreeStops") {
      TTreeNode(par(stopsRows) { s =>
        val stopId = s(0)
        val stopName = s(1).substring(8)
        val compoundStopNames = stopName.split("""\.|-""")
        val saintStopNames = {
          val SaintReg = """^Saint([-|\s])(.*)$""".r
          val StReg = """^St([-|\s])(.*)$""".r
          stopName match {
            case SaintReg(sep, n) =>
              val st = List("St " + n, "St-" + n)
              if(sep == " ") ("Saint-" + n) +: st else ("Saint " + n) +: st
            case StReg(sep, n) =>
              val saint = List("Saint " + n, "Saint-" + n)
              if(sep == " ") ("St-" + n) +: saint else ("St " + n) +: saint
            case _ => Nil
          }
        }
        (stopName +: saintStopNames ++: compoundStopNames).distinct.filterNot(_.isEmpty).map { s =>
          s.toLowerCase -> (stopId, stopName)
        }
      }.toList.flatten)
    }

  private def buildTrips(gtfs: GtfsDirectory): List[Trip] =
    Measure.duration("Trips") {
      par(gtfs.trips) { tripRow =>
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
