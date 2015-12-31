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

  def stopName(name: String): String =
    name.toLowerCase.split("\\s").map(_.capitalize).mkString(" ")
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

case class GtfsBundle(version: Version, data: ParsedGtfsDirectory)

object GtfsBundle {

  def defaultRoot: File = new File("gtfs")

  private def open(directory: File): Option[(Version, File, File, File)] = {
    for {
      version <- Version.fromDir(directory)
      terDir <- GtfsDirectory.check(new File(directory.getAbsolutePath + "/ter"))
      transDir <- GtfsDirectory.check(new File(directory.getAbsolutePath + "/trans"))
      interDir <- GtfsDirectory.check(new File(directory.getAbsolutePath + "/inter"))
    } yield (version, terDir, transDir, interDir)
  }

  private def fromDir(directory: File): Option[GtfsBundle] =
    open(directory) map {
      case (version, terDir, transDir, interDir) =>
        val gtfsTer = GtfsDirectory.ter(terDir)
        val gtfsTrans = GtfsDirectory.trans(transDir)
        val gtfsInter = GtfsDirectory.inter(interDir)
        GtfsBundle(version, gtfsTer merge gtfsTrans merge gtfsInter)
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

  type NodeId = String

  type TerStopId = String

  type TransStopId = String

  type InterStopId = String

  val TerStopId = """StopPoint:OCETrain TER-(.*).""".r

  val TerParentStopId = """StopArea:OCE(.*).""".r

  val TransStopId = """StopPoint:DUA(.*)""".r

  val TransParentStopId = """StopArea:DUA(.*)""".r

  val InterStopId = """StopPoint:OCECorail Intercité-(.*).""".r

  val InterParentStopId = """StopArea:OCE(.*).""".r

  def ter(dir: File): ParsedGtfsDirectory = {
    println(s"[GTFS] Reading ter from ${dir.getAbsolutePath}")

    val terServiceId = (id: String) => s"ter#${id}"

    val stopTimes = GtfsDirReader.stopTimes(dir) {
      case record@List(tripId, arrival, departure, stopId, stopSeq, stopHeadSign, pickUpType, dropOffType, _) =>
        stopId match {
          case TerStopId(nodeId) =>
            StopTimeRecord(tripId, parseTime(arrival), parseTime(departure), nodeId, stopSeq.toInt, stopHeadSign, pickUpType, dropOffType)
          case _ =>
            throw new CSVReadFile.Verbose(s"** Reading stops: unable to normalize ter id for: $stopId")
        }
    }

    val trips = GtfsDirReader.trips(dir) {
      case record@List(routeId, serviceId, tripId, tripHeadSign, directionId, blockId, shapeId) =>
        TripRecord(routeId, terServiceId(serviceId), tripId, tripHeadSign, directionId, blockId)
    }

    val stops = GtfsDirReader.stops(dir) {
      case record@List(stopId, stopName, stopDesc, stopLat, stopLong, zoneId, stopUrl, locationType, parentStation) if(stopId.startsWith("StopPoint:OCETrain TER-")) =>
        (stopId, parentStation) match {
          case (TerStopId(nodeId), TerParentStopId(parentNodeId)) =>
            val parent = parentNodeId match {
              case id if Stop.isParisLyon(id) => Stop.STOP_PARIS_LYON
              case id if Stop.isParisNord(id) => Stop.STOP_PARIS_NORD
              case _ => parentNodeId
            }
            StopRecord(nodeId, stopName.substring(8), stopDesc, stopLat.toDouble, stopLong.toDouble, zoneId, stopUrl, locationType, parent)
          case _ =>
            throw new CSVReadFile.Verbose(s"** Reading stops: unable to normalize ter id for: $stopId")
        }
    }

    val calendar = GtfsDirReader.calendar(dir) {
      case record@List(serviceId, monday, tuesday, wednesday, thursday, friday, saturday, sunday, startDate, endDate) =>
        CalendarRecord(
          terServiceId(serviceId),
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
        CalendarDateRecord(terServiceId(serviceId), parseDateTime(date), exceptionType.toInt)
    }

    ParsedGtfsDirectory(stopTimes, trips, stops, calendar, calendarDates)
  }

  def trans(dir: File): ParsedGtfsDirectory = {
    println(s"[GTFS] Reading trans from ${dir.getAbsolutePath}")

    val transServiceId = (id: String) => s"trans#${id}"

    val stopTimes = GtfsDirReader.stopTimes(dir) {
      case record@List(tripId, arrival, departure, stopId, stopSeq, stopHeadSign, pickUpType, dropOffType) =>
        stopId match {
          case TransStopId(nodeId) =>
            StopTimeRecord(tripId, parseTime(arrival), parseTime(departure), nodeId, stopSeq.toInt, stopHeadSign, pickUpType, dropOffType)
          case _ =>
            throw new CSVReadFile.Verbose(s"** Reading stops: unable to normalize trans id for: $stopId")
        }
    }

    val trips = GtfsDirReader.trips(dir) {
      case record@List(routeId, serviceId, tripId, tripHeadSign, directionId, blockId) =>
        TripRecord(routeId, transServiceId(serviceId), tripId, tripHeadSign, directionId, blockId)
    }

    val stops = GtfsDirReader.stops(dir) {
      case record@List(stopId, stopName, stopDesc, stopLat, stopLong, zoneId, stopUrl, locationType, parentStation) if(stopId.startsWith("StopPoint:DUA")) =>
        (stopId, parentStation) match {
          case (TransStopId(nodeId), TransParentStopId(parentNodeId)) =>
            val parent = parentNodeId match {
              case id if Stop.isParisLyon(id) => Stop.STOP_PARIS_LYON
              case id if Stop.isParisNord(id) => Stop.STOP_PARIS_NORD
              case _ => parentNodeId
            }
            StopRecord(nodeId, Normalizer.stopName(stopName), stopDesc, stopLat.toDouble, stopLong.toDouble, zoneId, stopUrl, locationType, parent)
          case _ =>
            throw new CSVReadFile.Verbose(s"** Reading stops: unable to normalize trans id for: $stopId")
        }
    }

    val calendar = GtfsDirReader.calendar(dir) {
      case record@List(serviceId, monday, tuesday, wednesday, thursday, friday, saturday, sunday, startDate, endDate) =>
        CalendarRecord(
          transServiceId(serviceId),
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
        CalendarDateRecord(transServiceId(serviceId), parseDateTime(date), exceptionType.toInt)
    }

    ParsedGtfsDirectory(stopTimes, trips, stops, calendar, calendarDates)
  }

  def inter(dir: File): ParsedGtfsDirectory = {
    println(s"[GTFS] Reading inter from ${dir.getAbsolutePath}]")

    val interServiceId = (id: String) => s"inter#${id}"

    val stopTimes = GtfsDirReader.stopTimes(dir) {
      case record@List(tripId, arrival, departure, stopId, stopSeq, stopHeadSign, pickUpType, dropOffType, _) =>
        stopId match {
          case InterStopId(nodeId) =>
            StopTimeRecord(tripId, parseTime(arrival), parseTime(departure), nodeId, stopSeq.toInt, stopHeadSign, pickUpType, dropOffType)
          case _ =>
            throw new CSVReadFile.Verbose(s"** Reading stops: unable to normalize inter id for: $stopId")
        }
    }

    val trips = GtfsDirReader.trips(dir) {
      case record@List(routeId, serviceId, tripId, tripHeadSign, directionId, blockId, shapeId) =>
        TripRecord(routeId, interServiceId(serviceId), tripId, tripHeadSign, directionId, blockId)
    }

    val stops = GtfsDirReader.stops(dir) {
      case record@List(stopId, stopName, stopDesc, stopLat, stopLong, zoneId, stopUrl, locationType, parentStation) if(stopId.startsWith("StopPoint:OCECorail")) =>
        (stopId, parentStation) match {
          case (InterStopId(nodeId), InterParentStopId(parentNodeId)) =>
            val parent = parentNodeId match {
              case id if Stop.isParisLyon(id) => Stop.STOP_PARIS_LYON
              case id if Stop.isParisNord(id) => Stop.STOP_PARIS_NORD
              case _ => parentNodeId
            }
            StopRecord(nodeId, stopName.substring(8), stopDesc, stopLat.toDouble, stopLong.toDouble, zoneId, stopUrl, locationType, parent)
          case _ =>
            throw new CSVReadFile.Verbose(s"** Reading stops: unable to normalize inter id for: $stopId")
        }
    }

    val calendar = GtfsDirReader.calendar(dir) {
      case record@List(serviceId, monday, tuesday, wednesday, thursday, friday, saturday, sunday, startDate, endDate) =>
        CalendarRecord(
          interServiceId(serviceId),
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
        CalendarDateRecord(interServiceId(serviceId), parseDateTime(date), exceptionType.toInt)
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
) {
  def merge(p: ParsedGtfsDirectory): ParsedGtfsDirectory = {
    p.copy(
      p.stopTimes ++: stopTimes,
      p.trips ++: trips,
      p.stops ++: stops,
      p.calendar ++: calendar,
      p.calendarDates ++: calendarDates
    )
  }
}

object ParsedGtfsDirectory {

  def empty = ParsedGtfsDirectory(
    Nil, Nil, Nil, Nil, Nil
  )
}


object GtfsDirReader {

  private def file(root: File, name: String): File = {
    new File(root.getAbsolutePath() + "/" + name)
  }

  def stopTimes(gtfsDir: File)(collect: CSVReadFile.CollectFunct[StopTimeRecord]): List[StopTimeRecord] =
    CSVReadFile(file(gtfsDir, "stop_times.txt")).read(collect)

  def trips(gtfsDir: File)(collect: CSVReadFile.CollectFunct[TripRecord]): List[TripRecord] =
    CSVReadFile(file(gtfsDir, "trips.txt")).read(collect)

  def stops(gtfsDir: File)(collect: CSVReadFile.CollectFunct[StopRecord]): List[StopRecord] =
    CSVReadFile(file(gtfsDir, "stops.txt")).read(collect)

  def calendar(gtfsDir: File)(collect: CSVReadFile.CollectFunct[CalendarRecord]): List[CalendarRecord] =
    CSVReadFile(file(gtfsDir, "calendar.txt")).read(collect)

  def calendarDates(gtfsDir: File)(collect: CSVReadFile.CollectFunct[CalendarDateRecord]): List[CalendarDateRecord] =
    CSVReadFile(file(gtfsDir, "calendar_dates.txt")).read(collect)
}
