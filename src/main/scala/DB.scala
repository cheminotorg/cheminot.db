package m.cheminot

import java.io.File
import org.joda.time.DateTime
import scala.util.control.Exception
import scala.concurrent.Future
import misc._
import models._

case class Subset(id: String, graph: Map[StopId, Vertice], calendar: List[Calendar], calendarDates: List[CalendarDate], ttstops: TTreeNode[(String, String)])

case class DB(gtfsBundle: GtfsBundle) {

  lazy val version: Version = gtfsBundle.version

  private lazy val terTrips: List[Trip] = DB.buildTrips(gtfsBundle.ter)

  lazy val fixedTerTrips: List[Trip] = DB.fixTrips(terTrips, refs)

  private lazy val transTrips: List[Trip] = DB.buildTrips(gtfsBundle.trans)

  lazy val fixedTransTrips: List[Trip] = DB.fixTrips(transTrips, refs)

  private lazy val interTrips: List[Trip] = DB.buildTrips(gtfsBundle.inter)

  lazy val fixedInterTrips: List[Trip] = DB.fixTrips(interTrips, refs)

  private lazy val trips: List[Trip] = terTrips ++: transTrips ++: interTrips

  lazy val fixedTrips: List[Trip] = fixedTerTrips ++: fixedTransTrips ++: fixedInterTrips

  lazy val stops: List[StopRecord] = (gtfsBundle.ter.stops ++: gtfsBundle.trans.stops ++: gtfsBundle.inter.stops).distinct

  lazy val ttstops: TTreeNode[(String, String)] = DB.buildTreeStops(stops, graph)

  lazy val (terGraph: Map[StopId, Vertice], _) = DB.buildGraph(gtfsBundle.ter.stops, terTrips)

  lazy val (transGraph: Map[StopId, Vertice], _) = DB.buildGraph(gtfsBundle.trans.stops, transTrips)

  lazy val (interGraph: Map[StopId, Vertice], _) = DB.buildGraph(gtfsBundle.inter.stops, interTrips)

  lazy val (graph: Map[StopId, Vertice], refs: Map[StopId, StopId]) = DB.buildGraph(stops, trips)

  lazy val ter = Subset(
    "ter",
    terGraph,
    gtfsBundle.ter.calendar.map(Calendar.fromRecord),
    gtfsBundle.ter.calendarDates.map(CalendarDate.fromRecord),
    DB.buildTreeStops(gtfsBundle.ter.stops, terGraph)
  )

  lazy val inter = Subset(
    "inter",
    interGraph,
    gtfsBundle.inter.calendar.map(Calendar.fromRecord),
    gtfsBundle.inter.calendarDates.map(CalendarDate.fromRecord),
    DB.buildTreeStops(gtfsBundle.inter.stops, interGraph)
  )

  lazy val trans = Subset(
    "trans",
    transGraph,
    gtfsBundle.trans.calendar.map(Calendar.fromRecord),
    gtfsBundle.trans.calendarDates.map(CalendarDate.fromRecord),
    DB.buildTreeStops(gtfsBundle.trans.stops, transGraph)
  )
}

object DB {

  def defaultDbDir: File = new File("db")

  def fromDir(directory: File): Option[DB] =
    GtfsBundle.mostRecent(root = Some(directory)).map(DB.apply)

  def fromDefaultDir(): Option[DB] =
    GtfsBundle.mostRecent().map(DB.apply)

  private def mergeVertices(z: Vertice, verticeIds: List[String], vertices: Map[StopId, Vertice], replace: Boolean): Map[StopId, Vertice] = {
    val mergedVertice = verticeIds.flatMap(vertices.get).foldLeft(z) { (acc, vertice) =>
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

  private def stopRecord2Vertice(stopRecord: StopRecord, trips: List[Trip]): Vertice = {
    val zEdges = if(Stop.isParis(stopRecord.stopId)) Stop.parisStops.filterNot(_ == stopRecord.stopId).toList else Seq.empty[StopId]
    val zStopTimes = if(Stop.isParis(stopRecord.stopId)) Subway.stopTimes.get(stopRecord.stopId).getOrElse(Nil) else Seq.empty[StopTime]
    val (edges, stopTimes) = trips.foldLeft((zEdges, zStopTimes)) { (acc, trip) =>
      val (accEdges, accStopTimes) = acc
      val edges = trip.edgesOf(stopRecord.stopId)
      val stopTimes = trip.stopTimes.find(_.stopId == stopRecord.stopId).toList.map { st =>
        StopTime(st.tripId, st.arrival, st.departure, stopRecord.stopId, st.pos)
      }
      (edges ++: accEdges) -> (stopTimes ++: accStopTimes)
    }
    Vertice(stopRecord.stopId, stopRecord.stopName, stopRecord.stopLat, stopRecord.stopLong, edges.distinct, stopTimes.distinct)
  }

  private def fixVertice(vertice: Vertice)(f: StopId => Option[StopId]): Vertice = {
    vertice.copy(
      stopTimes = vertice.stopTimes.map { stopTime =>
        f(stopTime.stopId) map (stopId => stopTime.copy(stopId = stopId)) getOrElse stopTime
      }.distinct,

      edges = vertice.edges.map { edge =>
        f(edge) getOrElse edge
      }.distinct
    )
  }

  private def fixTrip(trip: Trip)(f: StopId => Option[StopId]): Trip = {
    trip.copy(
      stopTimes = trip.stopTimes.map { stopTime =>
        f(stopTime.stopId) map (stopId => stopTime.copy(stopId = stopId)) getOrElse stopTime
      }.distinct
    )
  }

  private def fixTrips(trips: List[Trip], refs: Map[StopId, StopId]): List[Trip] = {
    trips.map(trip => fixTrip(trip)(refs.get))
  }

  private def buildGraph(stopRecords: List[StopRecord], trips: List[Trip]): (Map[StopId, Vertice], Map[StopId, StopId]) = {
    import scala.collection.JavaConverters._

    Measure.duration("Graph") {
      val refs = new java.util.concurrent.ConcurrentHashMap[String, String]()
      val groupedByParent: Map[StopId, Seq[StopRecord]] = stopRecords.groupBy(_.parentStation)
      val graph: Map[StopId, Vertice] = par(groupedByParent.toSeq) {
        case (parentStationId, child :: children) =>
          val vertice = stopRecord2Vertice(child, trips).copy(id = parentStationId)
          val merged = children.map { stopRecord =>
            stopRecord2Vertice(stopRecord, trips)
          }.foldLeft(vertice) { (acc, v) =>
            acc.copy(
              stopTimes = (v.stopTimes ++: acc.stopTimes).distinct,
              edges = (v.edges ++: acc.edges).distinct
            )
          }
          merged.id -> fixVertice(merged) { stopId =>
            Option(refs.get(stopId)) orElse {
              groupedByParent.collectFirst {
                case (parentId, stops) if stops.exists(_.stopId == stopId) =>
                  refs.put(stopId, parentId)
                  parentId
              }
            }
          }
        case x => sys.error("Unable to build graph: " + x)
      }.toMap

      mergeVertices(Vertice.PARIS, Stop.parisStops, graph, replace = false) -> refs.asScala.toMap
    }
  }

  private def buildTreeStops(stopRecords: List[StopRecord], graph: Map[StopId, Vertice]): TTreeNode[(String, String)] = {
    Measure.duration("TTreeStops") {
      val entries = par(stopRecords.filter(s => graph.get(s.stopId).isDefined)) { stopRecord =>
        val saintStopNames = Normalizer.handleSaintWords(stopRecord.stopName)
        val compoundStopNames = if(saintStopNames.isEmpty) Normalizer.handleCompoundWords(stopRecord.stopName) else Nil
        (stopRecord.stopName +: saintStopNames ++: compoundStopNames).distinct.filterNot(_.isEmpty).map { s =>
          s.toLowerCase -> (stopRecord.stopId, stopRecord.stopName)
        }
      }.toList.flatten
      val paris = Vertice.PARIS.name.toLowerCase -> (Stop.STOP_PARIS, Vertice.PARIS.name)
      TTreeNode(paris +: entries)
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
