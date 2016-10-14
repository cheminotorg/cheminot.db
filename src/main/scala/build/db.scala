package org.cheminot.db

import rapture.fs._
import java.nio.file.Files
import rapture.uri._
import org.cheminot.misc

case class Subset(
  id: String,
  graph: Map[StopId, Vertice],
  calendar: List[Calendar],
  calendarDates: List[CalendarDate]
)

case class DB(
  id: DbId,
  graph: Map[VerticeId, Vertice],
  trips: Map[TripId, Trip],
  calendarDates: List[CalendarDate],
  calendar: List[Calendar],
  bundle: GtfsBundle
) {
  def outDir(rootDir: FsUrl) =
    rootDir / bundle.id.value / id

  def setAsCurrent(rootDir: FsUrl): Unit = {
    val currentLink = rootDir / "current"
    currentLink.javaFile.delete
    val dbDir = outDir(rootDir)
    Files.createSymbolicLink(currentLink.javaFile.toPath, dbDir.javaFile.toPath);
  }
}

object DB {

  def apply(gtfsBundle: GtfsBundle): DB = {
    val (graph: Map[VerticeId, Vertice], trips: Map[TripId, Trip]) =
      Builder.build(gtfsBundle)

    val tripsByServiceId = trips.toSeq.map {
      case (_, trip) =>
        trip.calendar.serviceId -> trip
    }.toMap

    val calendar: List[Calendar] =
      gtfsBundle.data.calendar.map(Calendar.fromRecord).filter { c =>
        tripsByServiceId.get(c.serviceId).isDefined
      } :+ Calendar.on

    val calendarDates: List[CalendarDate] =
      gtfsBundle.data.calendarDates.map(CalendarDate.fromRecord)

    DB("world", graph, trips, calendarDates, calendar, gtfsBundle)
  }

  def defaultDbDir: FsUrl = misc.File.currentDir / "db"

  def fromDir(directory: FsUrl): Option[DB] =
    GtfsBundle.fromDir(directory).map(apply)

  def fromDirOrFail(directory: FsUrl): DB =
    fromDir(directory).getOrElse {
      sys.error(s"Unable to build db from dir $directory")
    }

  def subset(db: DB, verticeIds: Seq[String]): DB = {
    val trips = db.trips.filter {
      case (tripId, trip) =>
        trip.stopTimes.exists(stopTime => verticeIds.exists(_ == stopTime.stopId))
    }

    val calendarDates = db.calendarDates.filter { calendarDate =>
      trips.values.toList.exists(_.calendar.serviceId == calendarDate.serviceId)
    }

    val calendar = db.calendar.filter { calendar =>
      trips.values.toList.exists(_.calendar.serviceId == calendar.serviceId)
    }

    val id = verticeIds.sortBy(identity).mkString("#")

    db.copy(
      id = id,
      trips = trips,
      calendar = calendar,
      calendarDates = calendarDates
    )
  }

  def empty: DB = DB(
    id = "empty",
    graph = Map.empty,
    trips = Map.empty,
    calendarDates = Nil,
    calendar = Nil,
    bundle = GtfsBundle.empty
  )

  def buildEmbed(embedDb: DB)(implicit config: Config): FsUrl =
    storage.Sqlite.create(config.dbDir, embedDb)
}
