package m.cheminot

import java.io.File
import java.sql.{ Connection, DriverManager }
import play.api.libs.json.Json
import anorm._
import misc._
import models._

object Sqlite {

  def withConnection[A](dbPath: String)(block: Connection => A): A = {
    val connection = DriverManager.getConnection("jdbc:sqlite:" + dbPath)
    val a = block(connection)
    connection.close()
    a
  }

  def createMetaTable()(implicit connection: Connection) {
    SQL("CREATE TABLE meta (key unique, value)").executeUpdate
  }

  def createTripsTable()(implicit connection: Connection) {
    SQL("CREATE TABLE trips (id unique, service, direction, stopTimes)").executeUpdate
  }

  def insertTrips(trips: Seq[Trip])(implicit connection: Connection) {
    trips.foreach { trip =>
      SQL("INSERT INTO trips (id , service, direction, stopTimes) VALUES({id}, {service}, {direction}, {stopTimes})").on(
        'id -> trip.id,
        'service -> Json.stringify(Json.toJson(trip.service)),
        'direction -> trip.direction,
        'stopTimes -> Json.stringify(Json.toJson(trip.stopTimes))
      ).executeUpdate
    }
  }

  def setVersion(version: String)(implicit connection: Connection) {
    SQL("INSERT INTO meta (key, value) VALUES({key}, {value})").on(
      'key -> "version",
      'value -> version
    ).executeUpdate
  }
}
