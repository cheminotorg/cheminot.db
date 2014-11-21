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

  def createGraphTable()(implicit connection: Connection) {
    SQL("CREATE TABLE graph (id unique, name, edges, stopTimes)").executeUpdate
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

  def insertGraph(graph: Seq[Vertice])(implicit connection: Connection) {
    graph.foreach { case Vertice(id, name, edges, stopTimes) =>
      SQL("INSERT INTO graph (id, name, edges, stopTimes) VALUES({id}, {name}, {edges}, {stopTimes})").on(
        'id -> id,
        'name -> name,
        'edges -> Json.stringify(Json.toJson(edges)),
        'stopTimes -> Json.stringify(Json.toJson(stopTimes))
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
