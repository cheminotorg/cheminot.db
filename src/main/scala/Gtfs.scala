package m.cheminot

import java.io.File
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import misc._
import models._
import scala.collection.mutable.{ Map => MMap }

object Normalizer {

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
}

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

  def parseBoolean(str: String): Boolean = {
    str match {
      case "0" => false
      case "1" => true
      case _ => sys.error("Unable to parse boolean from: " + str)
    }
  }
}

case class GtfsBundle(version: Version, ter: ParsedGtfsDirectory, trans: ParsedGtfsDirectory)

object GtfsBundle {

  def defaultRoot: File = new File("gtfs")

  private def open(directory: File): Option[(Version, File, File)] = {
    for {
      version <- Version.fromDir(directory)
      terDir <- GtfsDirectory.check(new File(directory.getAbsolutePath + "/ter"))
      transDir <- GtfsDirectory.check(new File(directory.getAbsolutePath + "/trans"))
    } yield (version, terDir, transDir)
  }

  private def fromDir(directory: File): Option[GtfsBundle] =
    open(directory) map {
      case (version, terDir, transDir) =>
        val (gtfsTer, terConnections) = GtfsDirectory.ter(terDir)
        GtfsBundle(version, gtfsTer, GtfsDirectory.trans(transDir, terConnections))
    }

  def mostRecent(root: Option[File] = None): Option[GtfsBundle] =
    root.getOrElse(defaultRoot).listFiles.toList.filter (_.isDirectory)
      .filter(d => open(d).isDefined)
      .flatMap(dir => Version.fromDir(dir) map (dir -> _))
      .sortBy { case (_, version) => -version.date.getMillis }
      .headOption flatMap { case (dir, _) => fromDir(dir) }
}

object GtfsDirectory {

  import Gtfs._

  type TerConnections = Map[String, String] // common id, ter stopId

  val TerStopId = """StopPoint:OCETrain TER-(.*).""".r

  val TransStopId = """StopPoint:DUA(.*)""".r

  def ter(dir: File): (ParsedGtfsDirectory, TerConnections) = {
    println("[GTFS] Reading ter")

    var terConnections: TerConnections = Map()

    val stopTimes = GtfsDirReader.stopTimes(dir) {
      case record@List(tripId, arrival, departure, stopId, stopSeq, stopHeadSign, pickUpType, dropOffType, _) =>
        StopTimeRecord(tripId, parseTime(arrival), parseTime(departure), stopId, stopSeq.toInt, stopHeadSign, pickUpType, dropOffType)
    }

    val trips = GtfsDirReader.trips(dir) {
      case record@List(routeId, serviceId, tripId, tripHeadSign, directionId, blockId, shapeId) =>
        TripRecord(routeId, serviceId, tripId, tripHeadSign, directionId, blockId)
    }

    val stops = GtfsDirReader.stops(dir) {
      case record@List(stopId, stopName, stopDesc, stopLat, stopLong, zoneId, stopUrl, locationType, parentStation) if(stopId.startsWith("StopPoint:OCETrain TER-")) =>
        stopId match {
          case TerStopId(normalizedId) =>
            terConnections = terConnections + (normalizedId -> stopId)
          case _ =>
            println(s"** Reading stops: unable to normalize ter id for: $stopId")
        }
        StopRecord(stopId, stopName.substring(8), stopDesc, stopLat.toDouble, stopLong.toDouble, zoneId, stopUrl, locationType, parentStation)
    }

    val calendar = GtfsDirReader.calendar(dir) {
      case record@List(serviceId, monday, tuesday, wednesday, thursday, friday, saturday, sunday, startDate, endDate) =>
        CalendarRecord(
          serviceId,
          parseBoolean(monday),
          parseBoolean(tuesday),
          parseBoolean(wednesday),
          parseBoolean(thursday),
          parseBoolean(friday),
          parseBoolean(saturday),
          parseBoolean(sunday),
          parseDateTime(startDate),
          parseDateTime(endDate)
        )
    }

    val calendarDates = GtfsDirReader.calendarDates(dir) {
      case record@List(serviceId, date, exceptionType) =>
        CalendarDateRecord(serviceId, parseDateTime(date), exceptionType.toInt)
    }

    val parsed = ParsedGtfsDirectory(stopTimes, trips, stops, calendar, calendarDates)

    parsed -> terConnections
  }

  def trans(dir: File, terConnections: TerConnections): ParsedGtfsDirectory = {
    println("[GTFS] Reading trans")

    val stopTimes = GtfsDirReader.stopTimes(dir) {
      case record@List(tripId, arrival, departure, stopId, stopSeq, stopHeadSign, pickUpType, dropOffType) =>
        val maybeUpdatedStopId = for {
          commonStopId <- stopId match {
            case TransStopId(normalizedId) =>
              Some(normalizedId)
            case _ =>
              println(s"** Reading stopTimes : unable to normalize trans id for: $stopId")
              None
          }
          terStopId <- terConnections.get(commonStopId)
        } yield terStopId

        StopTimeRecord(tripId, parseTime(arrival), parseTime(departure), maybeUpdatedStopId.getOrElse(stopId), stopSeq.toInt, stopHeadSign, pickUpType, dropOffType)
    }

    val trips = GtfsDirReader.trips(dir) {
      case record@List(routeId, serviceId, tripId, tripHeadSign, directionId, blockId) =>
        TripRecord(routeId, serviceId, tripId, tripHeadSign, directionId, blockId)
    }

    val stops = GtfsDirReader.stops(dir) {
      case record@List(stopId, stopName, stopDesc, stopLat, stopLong, zoneId, stopUrl, locationType, parentStation) if(stopId.startsWith("StopPoint:DUA")) =>
        stopId match {
          case TransStopId(normalizedId) if(terConnections.get(normalizedId).isEmpty) =>
            StopRecord(stopId, stopName.toLowerCase.capitalize, stopDesc, stopLat.toDouble, stopLong.toDouble, zoneId, stopUrl, locationType, parentStation)
          case _ =>
            sys.error(s"** Reading stops: unable to normalize trans id for: $stopId")
        }
    }

    val calendar = GtfsDirReader.calendar(dir) {
      case record@List(serviceId, monday, tuesday, wednesday, thursday, friday, saturday, sunday, startDate, endDate) =>
        CalendarRecord(
          serviceId,
          parseBoolean(monday),
          parseBoolean(tuesday),
          parseBoolean(wednesday),
          parseBoolean(thursday),
          parseBoolean(friday),
          parseBoolean(saturday),
          parseBoolean(sunday),
          parseDateTime(startDate),
          parseDateTime(endDate)
        )
    }

    val calendarDates = GtfsDirReader.calendarDates(dir) {
      case record@List(serviceId, date, exceptionType) =>
        CalendarDateRecord(serviceId, parseDateTime(date), exceptionType.toInt)
    }

    ParsedGtfsDirectory(stopTimes, trips, stops, calendar, calendarDates)
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
  stopTimes: List[StopTimeRecord],
  trips: List[TripRecord],
  stops: List[StopRecord],
  calendar: List[CalendarRecord],
  calendarDates: List[CalendarDateRecord]
)

object ParsedGtfsDirectory {

  def empty = ParsedGtfsDirectory(
    Nil, Nil, Nil, Nil, Nil
  )
}


object GtfsDirReader {

  private def file(root: File, name: String): File = {
    new File(root.getAbsolutePath() + "/" + name)
  }

  def stopTimes(gtfsDir: File)(collect: CSVFile.CollectFunct[StopTimeRecord]): List[StopTimeRecord] =
    CSVFile(file(gtfsDir, "stop_times.txt")).read(collect)

  def trips(gtfsDir: File)(collect: CSVFile.CollectFunct[TripRecord]): List[TripRecord] =
    CSVFile(file(gtfsDir, "trips.txt")).read(collect)

  def stops(gtfsDir: File)(collect: CSVFile.CollectFunct[StopRecord]): List[StopRecord] =
    CSVFile(file(gtfsDir, "stops.txt")).read(collect)

  def calendar(gtfsDir: File)(collect: CSVFile.CollectFunct[CalendarRecord]): List[CalendarRecord] =
    CSVFile(file(gtfsDir, "calendar.txt")).read(collect)

  def calendarDates(gtfsDir: File)(collect: CSVFile.CollectFunct[CalendarDateRecord]): List[CalendarDateRecord] =
    CSVFile(file(gtfsDir, "calendar_dates.txt")).read(collect)
}

case class StopTimeRecord(
  tripId: String,
  arrival: DateTime,
  departure: DateTime,
  stopId: String,
  stopSeq: Int,
  stopHeadSign: String,
  pickUpType: String,
  dropOffType: String
)

case class TripRecord(
  routeId: String,
  serviceId: String,
  tripId: String,
  tripHeadSign: String,
  directionId: String,
  blockId: String
)

case class StopRecord(
  stopId: String,
  stopName: String,
  stopDesc: String,
  stopLat: Double,
  stopLong: Double,
  zone: String,
  stopUrl: String,
  locationType: String,
  parentStation: String
)

case class CalendarRecord(
  serviceId: String,
  monday: Boolean,
  tuesday: Boolean,
  wednesday: Boolean,
  thursday: Boolean,
  friday: Boolean,
  saturday: Boolean,
  sunday: Boolean,
  startDate: DateTime,
  endDate: DateTime
)

case class CalendarDateRecord(
  serviceId: String,
  date: DateTime,
  exceptionType: Int
)
