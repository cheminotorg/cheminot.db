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

case class GtfsBundle(version: Version, ter: ParsedGtfsDirectory, trans: ParsedGtfsDirectory)

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

  def ter(dir: File): (ParsedGtfsDirectory, TerConnections) = {

    val terConnections: TerConnections = MMap()

    val stopTimes = GtfsDirReader.stopTimes(dir) {
      case x => x
    }

    val trips = GtfsDirReader.trips(dir) {
      case x => x
    }

    val stops = GtfsDirReader.stops(dir) {
      case x => x
    }

    val calendar = GtfsDirReader.calendar(dir) {
      case x => x
    }

    val calendarDates = GtfsDirReader.calendarDates(dir) {
      case x => x
    }

    val parsed = ParsedGtfsDirectory(stopTimes, trips, stops, calendar, calendarDates)

    parsed -> terConnections
  }

  def trans(dir: File, terConnections: TerConnections): (ParsedGtfsDirectory, TransConnections) = {

    val transConnections: TransConnections = MMap()

    val stopTimes = GtfsDirReader.stopTimes(dir) {
      case x => x
    }

    val trips = GtfsDirReader.trips(dir) {
      case x => x
    }

    val stops = GtfsDirReader.stops(dir) {
      case x => x
    }

    val calendar = GtfsDirReader.calendar(dir) {
      case x => x
    }

    val calendarDates = GtfsDirReader.calendarDates(dir) {
      case x => x
    }

    val parsed = ParsedGtfsDirectory(stopTimes, trips, stops, calendar, calendarDates)

    parsed -> transConnections
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

case class ParsedGtfsDirectory(
  stopTimes: CSVFile.Records,
  trips: CSVFile.Records,
  stops: CSVFile.Records,
  calendar: CSVFile.Records,
  calendarDates: CSVFile.Records
)


object GtfsDirReader {

  private def file(root: File, name: String): File = {
    new File(root.getAbsolutePath() + "/" + name)
  }

  def stopTimes(gtfsDir: File)(collect: PartialFunction[CSVFile.Record, CSVFile.Record]): CSVFile.Records =
    CSVFile(file(gtfsDir, "stop_times.txt")).read(collect)

  def trips(gtfsDir: File)(collect: PartialFunction[CSVFile.Record, CSVFile.Record]): CSVFile.Records =
    CSVFile(file(gtfsDir, "trips.txt")).read(collect)

  def stops(gtfsDir: File)(collect: PartialFunction[CSVFile.Record, CSVFile.Record]): CSVFile.Records =
    CSVFile(file(gtfsDir, "stops.txt")).read(collect)

  def calendar(gtfsDir: File)(collect: PartialFunction[CSVFile.Record, CSVFile.Record]): CSVFile.Records =
    CSVFile(file(gtfsDir, "calendar.txt")).read(collect)

  def calendarDates(gtfsDir: File)(collect: PartialFunction[CSVFile.Record, CSVFile.Record]): CSVFile.Records =
    CSVFile(file(gtfsDir, "calendar_dates.txt")).read(collect)
}
