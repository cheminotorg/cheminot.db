package m.cheminot.build

import scala.concurrent.Future
import org.joda.time.DateTime
import scala.collection.mutable.{ Map => MMap }
import rapture.fs._
import m.cheminot.misc
import m.cheminot.misc.{ CSVReadFile, CSVWriteFile }

object Normalizer {

  def stopName(name: String): String =
    name.toLowerCase.split("\\s").map(_.capitalize).mkString(" ")
}

object Gtfs {

  def parseDateTime(str: String): DateTime = {
    val formatter = misc.DateTime.forPattern("yyyyMMdd")
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

case class SubsetDir(
  dir: FileUrl,
  id: String,
  name: String,
  updatedDate: Option[DateTime],
  startDate: Option[DateTime],
  endDate: Option[DateTime]
)

object SubsetDir {

  val R = """^(.+)-(.+)-(.+)-(.+)-(.+)$""".r

  val formatter = misc.DateTime.forPattern("yyyyMMdd")

  object AsDateTime {
    def unapply(d: String): Option[Option[DateTime]] = {
      Option(scala.util.Try(formatter.parseDateTime(d)).toOption)
    }
  }

  def ter(rootDir: FileUrl): Option[SubsetDir] =
    SubsetDir.fromDir(rootDir, "ter")

  def trans(rootDir: FileUrl): Option[SubsetDir] =
    SubsetDir.fromDir(rootDir, "trans")

  def inter(rootDir: FileUrl): Option[SubsetDir] =
    SubsetDir.fromDir(rootDir, "inter")

  private def fromDir(rootDir: FileUrl, id: String): Option[SubsetDir] =
    rootDir.children.map(_.filename).collect {
      case name@R(subsetId, recordId, AsDateTime(updatedDate), AsDateTime(startDate), AsDateTime(endDate)) if subsetId == id =>
        val dir = rootDir / name
        SubsetDir(dir, recordId, subsetId, updatedDate, startDate, endDate)
    }.headOption.filter(s => GtfsDirectory.check(s.dir).isDefined)

  def empty: SubsetDir =
    SubsetDir(File / "fake", "xxx", "fake", None, None, None)
}

class GtfsBundle(_id: BundleId, _data: => ParsedGtfsDirectory) {
  lazy val id = _id
  lazy val data = _data
}

object GtfsBundle {

  def apply(id: BundleId, data: => ParsedGtfsDirectory) =
    new GtfsBundle(id, data)

  def empty: GtfsBundle =
    GtfsBundle(BundleId.next, ParsedGtfsDirectory.empty)

  def defaultRoot: FileUrl= File.currentDir / "gtfs"

  private def open(rootDir: FileUrl): Option[(BundleId, SubsetDir, SubsetDir, SubsetDir)] = {
    for {
      bundleId <- BundleId.fromDir(rootDir)
      terDir <- SubsetDir.ter(rootDir)
      transDir <- SubsetDir.trans(rootDir)
      interDir <- SubsetDir.inter(rootDir)
    } yield (bundleId, terDir, transDir, interDir)
  }

  private def fromDir(directory: FileUrl): Option[GtfsBundle] =
    open(directory) map {
      case (bundleId, terDir, transDir, interDir) =>
        lazy val gtfsTer = GtfsDirectory.ter(terDir)
        lazy val gtfsTrans = GtfsDirectory.trans(transDir)
        lazy val gtfsInter = GtfsDirectory.inter(interDir)
        GtfsBundle(bundleId, gtfsTer merge gtfsTrans merge gtfsInter)
    }

  def mostRecent(root: FileUrl): Option[GtfsBundle] =
    mostRecent(Option(root))

  def mostRecent(root: Option[FileUrl] = None): Option[GtfsBundle] =
    root.getOrElse(defaultRoot).children.toList.filter (_.isDirectory)
      .filter(d => open(d).isDefined)
      .flatMap(dir => BundleId.fromDir(dir) map (dir -> _))
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

  def ter(subset: SubsetDir): ParsedGtfsDirectory = {
    println(s"[GTFS] Reading ter from ${subset.dir}")

    val terServiceId = (id: String) => s"ter#${id}"

    val stopTimes = GtfsDirReader.stopTimes(subset.dir) {
      case record@List(tripId, arrival, departure, stopId, stopSeq, stopHeadSign, pickUpType, dropOffType, _) =>
        stopId match {
          case TerStopId(nodeId) =>
            StopTimeRecord(tripId, parseTime(arrival), parseTime(departure), nodeId, stopSeq.toInt, stopHeadSign, pickUpType, dropOffType)
          case _ =>
            throw new CSVReadFile.Verbose(s"** Reading stops: unable to normalize ter id for: $stopId")
        }
    }

    val trips = GtfsDirReader.trips(subset.dir) {
      case record@List(routeId, serviceId, tripId, tripHeadSign, directionId, blockId, shapeId) =>
        TripRecord(routeId, terServiceId(serviceId), tripId, tripHeadSign, directionId, blockId)
    }

    val stops = GtfsDirReader.stops(subset.dir) {
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

    val calendar = GtfsDirReader.calendar(subset.dir) {
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

    val calendarDates = GtfsDirReader.calendarDates(subset.dir) {
      case record@List(serviceId, date, exceptionType) =>
        CalendarDateRecord(terServiceId(serviceId), parseDateTime(date), exceptionType.toInt)
    }

    ParsedGtfsDirectory(subset, stopTimes, trips, stops, calendar, calendarDates)
  }

  def trans(subset: SubsetDir): ParsedGtfsDirectory = {
    println(s"[GTFS] Reading trans from ${subset.dir}")

    val transServiceId = (id: String) => s"trans#${id}"

    val stopTimes = GtfsDirReader.stopTimes(subset.dir) {
      case record@List(tripId, arrival, departure, stopId, stopSeq, stopHeadSign, pickUpType, dropOffType) =>
        stopId match {
          case TransStopId(nodeId) =>
            StopTimeRecord(tripId, parseTime(arrival), parseTime(departure), nodeId, stopSeq.toInt, stopHeadSign, pickUpType, dropOffType)
          case _ =>
            throw new CSVReadFile.Verbose(s"** Reading stops: unable to normalize trans id for: $stopId")
        }
    }

    val trips = GtfsDirReader.trips(subset.dir) {
      case record@List(routeId, serviceId, tripId, tripHeadSign, directionId, blockId) =>
        TripRecord(routeId, transServiceId(serviceId), tripId, tripHeadSign, directionId, blockId)
    }

    val stops = GtfsDirReader.stops(subset.dir) {
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

    val calendar = GtfsDirReader.calendar(subset.dir) {
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

    val calendarDates = GtfsDirReader.calendarDates(subset.dir) {
      case record@List(serviceId, date, exceptionType) =>
        CalendarDateRecord(transServiceId(serviceId), parseDateTime(date), exceptionType.toInt)
    }

    ParsedGtfsDirectory(subset, stopTimes, trips, stops, calendar, calendarDates)
  }

  def inter(subset: SubsetDir): ParsedGtfsDirectory = {
    println(s"[GTFS] Reading inter from ${subset.dir}]")

    val interServiceId = (id: String) => s"inter#${id}"

    val stopTimes = GtfsDirReader.stopTimes(subset.dir) {
      case record@List(tripId, arrival, departure, stopId, stopSeq, stopHeadSign, pickUpType, dropOffType, _) =>
        stopId match {
          case InterStopId(nodeId) =>
            StopTimeRecord(tripId, parseTime(arrival), parseTime(departure), nodeId, stopSeq.toInt, stopHeadSign, pickUpType, dropOffType)
          case _ =>
            throw new CSVReadFile.Verbose(s"** Reading stops: unable to normalize inter id for: $stopId")
        }
    }

    val trips = GtfsDirReader.trips(subset.dir) {
      case record@List(routeId, serviceId, tripId, tripHeadSign, directionId, blockId, shapeId) =>
        TripRecord(routeId, interServiceId(serviceId), tripId, tripHeadSign, directionId, blockId)
    }

    val stops = GtfsDirReader.stops(subset.dir) {
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

    val calendar = GtfsDirReader.calendar(subset.dir) {
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

    val calendarDates = GtfsDirReader.calendarDates(subset.dir) {
      case record@List(serviceId, date, exceptionType) =>
        CalendarDateRecord(interServiceId(serviceId), parseDateTime(date), exceptionType.toInt)
    }

    ParsedGtfsDirectory(subset, stopTimes, trips, stops, calendar, calendarDates)
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

  def check(directory: FileUrl): Option[FileUrl] = {
    Option(directory).filter { dir =>
      dir.exists && dir.isDirectory
    } filter { _ =>
      gtfsFiles.forall { name =>
        directory.children.exists(_.filename == name)
      }
    }
  }
}

case class ParsedGtfsDirectory(
  subsetDirs: List[SubsetDir],
  stopTimes: List[StopTimeRecord],
  trips: List[TripRecord],
  stops: List[StopRecord],
  calendar: List[CalendarRecord],
  calendarDates: List[CalendarDateRecord]
) {
  def merge(p: ParsedGtfsDirectory): ParsedGtfsDirectory = {
    p.copy(
      p.subsetDirs ++: subsetDirs,
      p.stopTimes ++: stopTimes,
      p.trips ++: trips,
      p.stops ++: stops,
      p.calendar ++: calendar,
      p.calendarDates ++: calendarDates
    )
  }
}

object ParsedGtfsDirectory {

  def apply(
    subsetDir: SubsetDir,
    stopTimes: List[StopTimeRecord],
    trips: List[TripRecord],
    stops: List[StopRecord],
    calendar: List[CalendarRecord],
    calendarDates: List[CalendarDateRecord]
  ): ParsedGtfsDirectory = {
    ParsedGtfsDirectory(subsetDir :: Nil, stopTimes, trips, stops, calendar, calendarDates)
  }

  def empty: ParsedGtfsDirectory =
    ParsedGtfsDirectory(
      subsetDir = SubsetDir.empty,
      stopTimes = Nil,
      trips = Nil,
      stops = Nil,
      calendar = Nil,
      calendarDates = Nil
    )
}

case class BundleId(date: DateTime) {
  lazy val value: String = BundleId.formatter.print(date)
}

object BundleId {

  private def parse(name: String): Option[DateTime] =
    scala.util.Try(formatter.parseDateTime(name)).toOption

  val formatter = misc.DateTime.forPattern("yyyyMMddHHmmss")

  def fromDir(dir: FileUrl): Option[BundleId] = {
    for {
      _ <- Option(dir).filter(_.isDirectory)
      date <- parse(dir.filename)
    } yield {
      BundleId(date)
    }
  }

  def next: BundleId =
    BundleId(DateTime.now)
}


object GtfsDirReader {

  private def file(root: FileUrl, name: String): java.io.File =
    (root / name).javaFile

  def stopTimes(gtfsDir: FileUrl)(collect: CSVReadFile.CollectFunct[StopTimeRecord]): List[StopTimeRecord] =
    CSVReadFile(file(gtfsDir, "stop_times.txt")).read(collect)

  def trips(gtfsDir: FileUrl)(collect: CSVReadFile.CollectFunct[TripRecord]): List[TripRecord] =
    CSVReadFile(file(gtfsDir, "trips.txt")).read(collect)

  def stops(gtfsDir: FileUrl)(collect: CSVReadFile.CollectFunct[StopRecord]): List[StopRecord] =
    CSVReadFile(file(gtfsDir, "stops.txt")).read(collect)

  def calendar(gtfsDir: FileUrl)(collect: CSVReadFile.CollectFunct[CalendarRecord]): List[CalendarRecord] =
    CSVReadFile(file(gtfsDir, "calendar.txt")).read(collect)

  def calendarDates(gtfsDir: FileUrl)(collect: CSVReadFile.CollectFunct[CalendarDateRecord]): List[CalendarDateRecord] =
    CSVReadFile(file(gtfsDir, "calendar_dates.txt")).read(collect)
}
