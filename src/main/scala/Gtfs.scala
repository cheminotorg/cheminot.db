package m.cheminot

import java.io.File
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import misc._
import models._

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

case class GtfsBundle(version: Version, ter: GtfsDirectory)

object GtfsBundle {

  def defaultRoot: File = new File("gtfs")

  def fromDir(directory: File): Option[GtfsBundle] =
    for {
      version <- Version.fromDir(directory)
      terDir <- GtfsDirectory.check(new File(directory.getAbsolutePath + "/ter"))
      subwayDir <- GtfsDirectory.check(new File(directory.getAbsolutePath + "/subway"))
    } yield {
      GtfsBundle(version, GtfsDirectory.ter(terDir))
    }

  def mostRecent(root: Option[File] = None): Option[GtfsBundle] =
    root.getOrElse(defaultRoot).listFiles.toList.filter (_.isDirectory)
      .filter(d => fromDir(d).isDefined)
      .flatMap(dir => Version.fromDir(dir) map (dir -> _))
      .sortBy { case (_, version) => -version.date.getMillis }
      .headOption flatMap { case (dir, _) => fromDir(dir) }
}

object GtfsDirectory {

  def ter(dir: File): GtfsDirectory = {
    val gtfsDir = GtfsDirectory(dir)

    new GtfsDirectory(dir) {
      override lazy val stopTimes =
        gtfsDir.stopTimes.drop(1).filter(_.size == 9)

      override lazy val trips =
        gtfsDir.trips.drop(1).dropRight(1).filter(_.size == 7)

      override lazy val stops =
        gtfsDir.stops.par.collect {
          case List(id, name, desc, lat, lng, zone, url, location, parent) if(id.startsWith("StopPoint:OCETrain TER-")) =>
            List(id, name.substring(8), desc, lat, lng, zone, url, location, parent)
        }.toList

      override lazy val calendar =
        gtfsDir.calendar.drop(1).dropRight(1).filter(_.size == 10)

      override lazy val calendarDates =
        gtfsDir.calendarDates.drop(1).filter(_.size == 3)
    }
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

  lazy val gtfs: Map[String, CSVFile.Rows] = {
    println(s"Reading gtfs from ${directory.getAbsolutePath}...")
    CSVDirectory(directory).read()
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
