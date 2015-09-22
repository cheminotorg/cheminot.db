package m.cheminot

import java.io.File
import org.joda.time.DateTime
import java.util.concurrent.Executors
import scala.util.control.Exception
import scala.concurrent.{ Await, Future, ExecutionContext }
import scala.concurrent.duration._
import misc._
import models._

case class DB(gtfsBundle: GtfsBundle) {
  lazy val version = gtfsBundle.version

  lazy val trips: List[Trip] = DB.buildTrips(gtfsBundle.ter, gtfsBundle.trans)

  lazy val graph: List[Vertice] =
    DB.buildGraph(gtfsBundle.ter.stops ++: gtfsBundle.trans.stops, trips)

  lazy val calendar: List[Calendar] =
    (gtfsBundle.ter.calendar ++: gtfsBundle.trans.calendar).map(Calendar.fromRow)

  lazy val calendarDates: List[CalendarDate] =
    (gtfsBundle.ter.calendarDates ++: gtfsBundle.trans.calendarDates).map(CalendarDate.fromRow)

  lazy val ttstops: TTreeNode[(String, String)] = {
    DB.buildTreeStops(gtfsBundle.ter.stops ++: gtfsBundle.trans.stops)
  }
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
    GtfsBundle.mostRecent(root = Some(directory)).map(DB.apply)

  def fromDefaultDir(): Option[DB] =
    GtfsBundle.mostRecent().map(DB.apply)

  private def buildGraph(stopsRows: CSVFile.Rows, trips: List[Trip]): List[Vertice] =
    Measure.duration("Graph") {
      var paris = Vertice(Stop.STOP_PARIS, "Paris", 48.858859, 2.3470599, Nil, Nil)
      val vertices = par(stopsRows) { s =>
        val stopId = s(0)
        val stopName = s(1)
        val lat = s(3).toDouble
        val lng = s(4).toDouble
        val zStopTimes = Subway.stopTimes.get(stopId).getOrElse(Nil)
        val zEdges = Stop.parisStops.filterNot(_ == stopId).toList
        val (edges, stopTimes) = trips.foldLeft((zEdges, zStopTimes)) { (acc, trip) =>
          val (accEdges, accStopTimes) = acc
          val edges = trip.edgesOf(stopId)
          val stopTimes = trip.stopTimes.find(_.stopId == stopId).toList.map { st =>
            StopTime(st.tripId, st.arrival, st.departure, stopId, st.pos)
          }
          (edges ++: accEdges) -> (stopTimes ++: accStopTimes)
        }
        if(Stop.parisStops.contains(stopId)) {
          paris = paris.copy(edges = (edges ++: paris.edges).distinct, stopTimes = (stopTimes ++: paris.stopTimes).distinct)
        }
        Vertice(stopId, stopName, lat, lng, edges.distinct, stopTimes.distinct)
      }.toList
      paris +: vertices
    }

  private def buildTreeStops(stopsRows: CSVFile.Rows): TTreeNode[(String, String)] = {
    def handleCompoundWords(stopName: String): List[String] = {
      val spaceIndex = Option(stopName.indexOf(" ")).filterNot(_ == -1)
      val dashIndex = Option(stopName.indexOf("-")).filterNot(_ == -1)
      val (sep, newsep) = {
        val isSpace = spaceIndex.getOrElse(stopName.size) < dashIndex.getOrElse(stopName.size)
        if (isSpace) " " -> "-" else "-" -> " "
      }
      val splitStopName = stopName.split(sep).toList
      if(!spaceIndex.isEmpty && !dashIndex.isEmpty) {
        val x = stopName.split(newsep).toList match {
          case h :: t => h.replaceAll(sep, newsep) + newsep + t.mkString(newsep)
          case _ => sys.error("Unexpected case in handleCompoundWords")
        }
        x +: splitStopName
      } else {
        stopName.replaceAll(sep, newsep) +: splitStopName
      }
    }

    def handleSaintWords(stopName: String): List[String] = {
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

    Measure.duration("TTreeStops") {
      TTreeNode(("paris" -> (Stop.STOP_PARIS, "Paris")) +: par(stopsRows) { s =>
        val stopId = s(0)
        val stopName = s(1)
        val saintStopNames = handleSaintWords(stopName)
        val compoundStopNames = if(saintStopNames.isEmpty) handleCompoundWords(stopName) else Nil
        (stopName +: saintStopNames ++: compoundStopNames).distinct.filterNot(_.isEmpty).map { s =>
          s.toLowerCase -> (stopId, stopName)
        }
      }.toList.flatten)
    }
  }

  private def buildTrips(ter: GtfsDirectory, trans: GtfsDirectory): List[Trip] =
    Measure.duration("Trips") {
      par(ter.trips ++: trans.trips) { tripRow =>
        val routeId = tripRow(0)
        val serviceId = tripRow(1)
        val tripId = tripRow(2)
        val maybeService = (ter.calendar ++: trans.calendar).view.find(_.head == serviceId).map(Calendar.fromRow)
        val stopTimesForTrip = (ter.stopTimes ++: trans.stopTimes).collect {
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
