package m.cheminot

import java.io.File
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import misc._
import models._

object Gtfs {

  private lazy val defaultGtfsDir = new File("gtfs")

  def apply(directory: File): Option[GtfsDirectory] =
    check(directory).map { version =>
      Console.out.println(s"Reading gtfs from ${directory}...")
      val csvDir = CSVDirectory(directory).read()
      GtfsDirectory(version, csvDir)
    }

  def apply(): Option[GtfsDirectory] =
    defaultGtfsDir.listFiles.toList.filter (_.isDirectory)
      .filter(check(_).isDefined)
      .flatMap(dir => Version.fromDir(dir) map (dir -> _))
      .sortBy { case (_, version) => -version.date.getMillis }
      .headOption flatMap { case (dir, _) => Gtfs(dir) }

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

  def check(directory: File): Option[Version] = {
    Option(directory).filter { dir =>
      dir.exists && dir.isDirectory
    } flatMap Version.fromDir filter { _ =>
      gtfsFiles.forall { name =>
        directory.listFiles.toList.exists(_.getName == name)
      }
    }
  }

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

case class GtfsDirectory(version: Version, gtfs: Map[String, CSVFile.Rows]) {

  private def oops(message: String) = throw new RuntimeException(message)

  lazy val stopTimes = {
    val rows = gtfs.get("stop_times.txt").getOrElse(oops("Invalid gtfs format: stop_times.txt not found!"))
    rows.drop(1).filter(_.size == 9)
  }

  lazy val trips = {
    val rows = gtfs.get("trips.txt").getOrElse(oops("Invalid gtfs format: trips.txt not found!"))
    rows.drop(1).dropRight(1).filter(_.size == 7)
  }

  lazy val stops = {
    val rows = gtfs.get("stops.txt").getOrElse(oops("Invalid gtfs format: stops.txt not found!"))
    rows.drop(1).par.filter(row => row(0).startsWith("StopPoint:OCETrain") && row.size == 9).toList
  }

  lazy val calendar = {
    val rows = gtfs.get("calendar.txt").getOrElse(oops("Invalid gtfs format: calendar.txt not found!"))
    rows.drop(1).dropRight(1).filter(_.size == 10)
  }

  lazy val calendarDates = {
    val rows = gtfs.get("calendar_dates.txt").getOrElse(oops("Invalid gtfs format: calendar_dates.txt not found!"))
    rows.drop(1).filter(_.size == 3)
  }
}
