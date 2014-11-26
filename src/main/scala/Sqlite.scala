package m.cheminot

import java.io.File
import java.sql.{ Connection, DriverManager, PreparedStatement }
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
    SQL("CREATE TABLE trips (id TEXT PRIMARY KEY, calendar BLOB, direction TEXT)").executeUpdate
  }

  def insertTrips(trips: Seq[Trip])(implicit connection: Connection) {
    trips.foreach { trip =>
      SQL("INSERT INTO trips (id , calendar, direction) VALUES({id}, {calendar}, {direction})").on(
        'id -> trip.id,
        'calendar -> trip.calendar.map(c => Calendar.serialize(c).toByteArray),
        'direction -> trip.direction
      ).executeUpdate
    }
  }

  def setVersion(version: Version)(implicit connection: Connection) {
    SQL("INSERT INTO meta (key, value) VALUES({key}, {value})").on(
      'key -> "version",
      'value -> version.value
    ).executeUpdate
  }
}
