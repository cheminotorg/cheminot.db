package m.cheminot

import java.io.File
import java.sql.{ Connection, DriverManager, PreparedStatement }
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.Json
import anorm._
import misc._
import models._

object Sqlite {

  implicit object bytesToStatement extends ToStatement[Array[Byte]] {
    def set(s: PreparedStatement, i: Int, array: Array[Byte]): Unit =
      s.setBytes(i, array)
  }

  def withConnection[A](dbPath: String)(block: Connection => A): A = {
    val connection = DriverManager.getConnection("jdbc:sqlite:" + dbPath)
    val a = block(connection)
    connection.close()
    a
  }

  def init()(implicit connection: Connection) {
    SQL("PRAGMA synchronous = OFF").executeUpdate
  }

  def createMetaTable()(implicit connection: Connection) {
    SQL("CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT)").executeUpdate
  }

  def createTripsTable()(implicit connection: Connection) {
    SQL("CREATE TABLE trips (id TEXT PRIMARY KEY, calendar BLOB, direction TEXT, stopIds TEXT, type TEXT)").executeUpdate
    SQL("CREATE TABLE trips_stops (tripId TEXT, stopId TEXT, FOREIGN KEY (tripId) REFERENCES trips(id))").executeUpdate
  }

  def insertTrips(groupedTrips: (Symbol, Seq[Trip])*)(implicit connection: Connection) {
    SQL("BEGIN TRANSACTION").executeUpdate

    groupedTrips.foreach {
      case (typ, trips) =>

        val insertTripQuery = SQL("INSERT INTO trips (id , calendar, direction, stopIds, type) VALUES({id}, {calendar}, {direction}, {stopIds}, {type})")
        trips.foreach { trip =>
          try {
            insertTripQuery.on(
              'id -> trip.id,
              'calendar -> trip.calendar.map(c => Calendar.serialize(c).toByteArray).getOrElse(Array()),
              'direction -> trip.direction,
              'stopIds -> Trip.serializeStopIds(trip).toByteArray,
              'type -> typ.name
            ).executeUpdate
          } catch {
            case _: Exception => println("######## ", trip.id)
          }
        }

        val insertTripsStopsQuery = SQL("INSERT INTO trips_stops (tripId , stopId) VALUES({tripId}, {stopId})")
        trips.foreach { trip =>
          trip.stopTimes.foreach { stopTime =>
            insertTripsStopsQuery.on(
              'tripId -> trip.id,
              'stopId -> stopTime.stopId
            ).executeUpdate
          }
        }
    }

    SQL("END TRANSACTION").executeUpdate

    SQL("CREATE INDEX trips_stops_stop_index ON trips_stops (stopId)").executeUpdate
  }

  def initMeta(version: Version) (implicit connection: Connection) {
    val formatter = DateTimeFormat.forPattern("dd/MM/yyy").withZoneUTC
    SQL("INSERT INTO meta (key, value) VALUES({key}, {value})").on(
      'key -> "version",
      'value -> version.value
    ).executeUpdate

    SQL("INSERT INTO meta (key, value) VALUES({key}, {value})").on(
      'key -> "createdAt",
      'value -> formatter.print(version.date)
    ).executeUpdate
  }
}
