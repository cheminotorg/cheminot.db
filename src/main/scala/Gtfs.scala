package m.cheminot

import java.io.File
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import misc._
import models._
import scala.collection.mutable.{ Map => MMap }

object Gtfs {

  def parseDateTime(str: String): DateTime = {
    val formatter = DateTimeFormat.forPattern("yyyyMMdd").withZoneUTC
    DateTime.parse(str, formatter)
  }

  def parseTime(str: String): DateTime = {
    val TimeR = """(\d{2}):(\d{2}):(\d{2})""".r
    str match {
      case TimeR(hours, minutes, seconds) =>
        val now = DateTime.now
        val h = hours.toInt
        if(hours.toInt > 23) {
          now.plusDays(1).withTime(h - 24, minutes.toInt, seconds.toInt, 0)
        } else {
          now.withTime(h, minutes.toInt, seconds.toInt, 0)
        }
    }
  }
}

case class GtfsBundle(version: Version, ter: GtfsDirectory, trans: GtfsDirectory)

object GtfsBundle {

  def defaultRoot: File = new File("gtfs")

  def fromDir(directory: File): Option[GtfsBundle] =
    for {
      version <- Version.fromDir(directory)
      terDir <- GtfsDirectory.check(new File(directory.getAbsolutePath + "/ter"))
      transDir <- GtfsDirectory.check(new File(directory.getAbsolutePath + "/trans"))
    } yield {
      val (gtfsTer, terConnections) = GtfsDirectory.ter(terDir)
      val (gtfsTrans, transConnections) = GtfsDirectory.trans(transDir, terConnections)
      GtfsBundle(version, gtfsTer, gtfsTrans)
    }

  def mostRecent(root: Option[File] = None): Option[GtfsBundle] =
    root.getOrElse(defaultRoot).listFiles.toList.filter (_.isDirectory)
      .filter(d => fromDir(d).isDefined)
      .flatMap(dir => Version.fromDir(dir) map (dir -> _))
      .sortBy { case (_, version) => -version.date.getMillis }
      .headOption flatMap { case (dir, _) => fromDir(dir) }
}

object GtfsDirectory {

  type TerConnections = MMap[String, String] // common id, ter stopId

  type TransConnections = MMap[String, String] // trans stopId, common id

  val TerStopId = """StopPoint:OCETrain TER-(.*).""".r

  val TransStopId = """StopPoint:DUA(.*)""".r

  def ter(dir: File): (GtfsDirectory, TerConnections) = {

    val terConnections: TerConnections = MMap()

    val gtfsDir = GtfsDirectory(dir)

    val gtfs = new GtfsDirectory(dir) {
      override lazy val stopTimes =
        gtfsDir.stopTimes.drop(1).filter(_.size == 9)

      override lazy val trips =
        gtfsDir.trips.drop(1).dropRight(1).filter(_.size == 7)

      override lazy val stops =
        gtfsDir.stops.par.collect {
          case List(id, name, desc, lat, lng, zone, url, location, parent) if(id.startsWith("StopPoint:OCETrain TER-")) =>
            id match {
              case TerStopId(normalizedId) =>
                terConnections += (normalizedId -> id)
              case _ =>
                println("Unable to normalize ter id")
            }
            List(id, name.substring(8), desc, lat, lng, zone, url, location, parent)
        }.toList

      override lazy val calendar =
        gtfsDir.calendar.drop(1).dropRight(1).filter(_.size == 10)

      override lazy val calendarDates =
        gtfsDir.calendarDates.drop(1).filter(_.size == 3)
    }

    gtfs -> terConnections
  }

  def trans(dir: File, terConnections: TerConnections): (GtfsDirectory, TransConnections) = {

    val transDir = GtfsDirectory(dir)

    val transConnections: TransConnections = MMap()

    val gtfs = new GtfsDirectory(dir) {

      override lazy val stopTimes = {
        transDir.stopTimes.drop(1).par.collect {
          case List(tripId, arrivalTime, departureTime, stopId, stopSeq, stopHeadSign, pickupType, dropOffType) =>
            val updatedStopId = (for {
              commonId <- {
                stopId match {
                  case TransStopId(normalizedId) =>
                    Some(normalizedId)
                  case _ =>
                    println("Unable to normalize trans id")
                    None
                }
              }
              terId <- terConnections.get(commonId)
            } yield terId) getOrElse stopId
            List(tripId, arrivalTime, departureTime, updatedStopId, stopSeq, stopHeadSign, pickupType, dropOffType)
        }.toList
      }

      override lazy val trips =
        transDir.trips.drop(1).dropRight(1).filter(_.size == 6)

      lazy val stops = {
        transDir.stops.par.collect {
          case List(id, name, desc, lat, lng, zone, url, location, parent) if(id.startsWith("StopPoint:DUA")) =>
            id match {
              case TransStopId(normalizedId) if(terConnections.get(normalizedId).isEmpty) =>
                List(id, name.toLowerCase.capitalize, desc, lat, lng, zone, url, location, parent)

              case TransStopId(normalizedId) =>
                transConnections += (id -> normalizedId)
                List.empty

              case _ =>
                println("Unable to normalize trans id")
                List.empty
            }
        }.toList
      }

      override lazy val calendar =
        transDir.calendar.drop(1).dropRight(1).filter(_.size == 10)

      override lazy val calendarDates =
        transDir.calendarDates.drop(1).filter(_.size == 3)
    }

    gtfs -> transConnections
  }

  val gtfsFiles = Seq(
    "agency.txt",
    "calendar.txt",
    "calendar_dates.txt",
    "routes.txt",
    "stop_times.txt",
    "stops.txt",
    "transfers.txt",
    "trips.txt"
  )

  def check(directory: File): Option[File] = {
    Option(directory).filter { dir =>
      dir.exists && dir.isDirectory
    } filter { _ =>
      gtfsFiles.forall { name =>
        directory.listFiles.toList.exists(_.getName == name)
      }
    }
  }
}

case class GtfsDirectory(directory: File) {

  lazy val fileNames = Seq("stop_times.txt", "trips.txt", "stops.txt", "calendar.txt", "calendar_dates.txt")

  lazy val gtfs: Map[String, CSVFile.Rows] = {
    println(s"Reading gtfs from ${directory.getAbsolutePath}...")
    CSVDirectory(directory).read(n => fileNames.exists(_ == n))
  }

  private def oops(message: String) = throw new RuntimeException(message)

  lazy val stopTimes: CSVFile.Rows =
    gtfs.get("stop_times.txt").getOrElse(oops("Invalid gtfs format: stop_times.txt not found!"))

  lazy val trips: CSVFile.Rows =
    gtfs.get("trips.txt").getOrElse(oops("Invalid gtfs format: trips.txt not found!"))

  lazy val stops: CSVFile.Rows =
    gtfs.get("stops.txt").getOrElse(oops("Invalid gtfs format: stops.txt not found!"))

  lazy val calendar: CSVFile.Rows =
    gtfs.get("calendar.txt").getOrElse(oops("Invalid gtfs format: calendar.txt not found!"))

  lazy val calendarDates: CSVFile.Rows =
    gtfs.get("calendar_dates.txt").getOrElse(oops("Invalid gtfs format: calendar_dates.txt not found!"))
}
