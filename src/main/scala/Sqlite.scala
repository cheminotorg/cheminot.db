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

  def createMetaTable()(implicit connection: Connection) {
    SQL("CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT)").executeUpdate
  }

  def createTripsTable()(implicit connection: Connection) {
    SQL("CREATE TABLE trips (id TEXT PRIMARY KEY, calendar BLOB, direction TEXT, stopIds TEXT)").executeUpdate
    SQL("CREATE TABLE trips_stops (tripId TEXT, stopId TEXT, FOREIGN KEY (tripId) REFERENCES trips(id))").executeUpdate
  }

  def insertTrips(trips: Seq[Trip])(implicit connection: Connection) {
    trips.foreach { trip =>
      SQL("INSERT INTO trips (id , calendar, direction, stopIds) VALUES({id}, {calendar}, {direction}, {stopIds})").on(
        'id -> trip.id,
        'calendar -> trip.calendar.map(c => Calendar.serialize(c).toByteArray).getOrElse(Array()),
        'direction -> trip.direction,
        'stopIds -> Trip.serializeStopIds(trip).toByteArray
      ).executeUpdate
    }

    trips.foreach { trip =>
      trip.stopTimes.foreach { stopTime =>
        SQL("INSERT INTO trips_stops (tripId , stopId) VALUES({tripId}, {stopId})").on(
          'tripId -> trip.id,
          'stopId -> stopTime.stopId
        ).executeUpdate
      }
    }
  }

  def initMeta(version: Version, expiredAt: DateTime)(implicit connection: Connection) {
    val formatter = DateTimeFormat.forPattern("dd/MM/yyy").withZoneUTC
    SQL("INSERT INTO meta (key, value) VALUES({key}, {value})").on(
      'key -> "version",
      'value -> version.value
    ).executeUpdate

    SQL("INSERT INTO meta (key, value) VALUES({key}, {value})").on(
      'key -> "aborted",
      'value -> false
    ).executeUpdate

    SQL("INSERT INTO meta (key, value) VALUES({key}, {value})").on(
      'key -> "createdAt",
      'value -> formatter.print(version.date)
    ).executeUpdate

    SQL("INSERT INTO meta (key, value) VALUES({key}, {value})").on(
      'key -> "expiredAt",
      'value -> DateTimeFormat.forPattern("dd/MM/yyy").withZoneUTC.print(expiredAt)
    ).executeUpdate
  }
}
