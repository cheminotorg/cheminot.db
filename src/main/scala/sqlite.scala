package m.cheminot

import java.io.File
import java.sql.{ Connection, DriverManager }
import anorm._
import play.api.libs.json._
import misc._
import models._

object Sqlite {

  private def get(version: String): File = {
    new File(Cheminot.dbDirectory + "/" + version + "/cheminot.db")
  }

  private def withConnection[A](dbPath: String)(block: Connection => A): A = {
    val connection = DriverManager.getConnection("jdbc:sqlite:" + dbPath)
    val a = block(connection)
    connection.close()
    a
  }

  def create(db: DB): File = {
    println(s"Let's create sqlite for db ${db.version} !")
    val dbFile = get(db.version)
    new File(dbFile.getParent()).mkdirs
    withConnection(dbFile.getAbsolutePath) { implicit connection =>
      createCacheTable()
      createTripsTable()
      createGraphTable()
      createStopsTable()
      insertDateExceptions(db.exceptions)
      insertGraph(db.graph)
      insertTrips(db.trips)
      insertStops(db.stops)
      setVersion(db.version)
    }
    println(s"Ok, sqlite ${db.version} created !")
    dbFile
  }

  private def createCacheTable()(implicit connection: Connection) {
    SQL("CREATE TABLE cache (key unique, value)").executeUpdate
  }

  private def createTripsTable()(implicit connection: Connection) {
    SQL("CREATE TABLE trips (id unique, service, direction, stopTimes)").executeUpdate
  }

  private def createGraphTable()(implicit connection: Connection) {
    SQL("CREATE TABLE graph (id unique, name, edges, stopTimes)").executeUpdate
  }

  private def createStopsTable()(implicit connection: Connection) {
    SQL("CREATE TABLE stops (id unique, name, lat, lng)").executeUpdate
  }

  private def insertDateExceptions(dateExceptions: Seq[CalendarDate])(implicit connection: Connection) {
    val json = dateExceptions.groupBy(_.serviceId).foldRight(Json.obj()) {
      case ((serviceId, exceptions), acc) =>
        acc ++ Json.obj(serviceId -> Json.toJson(exceptions))
    }

    SQL("INSERT INTO cache (key, value) VALUES({key}, {value})").on(
      'key -> "exceptions",
      'value -> Json.stringify(json)
    ).executeUpdate
  }

  private def insertGraph(graph: Seq[Vertice])(implicit connection: Connection) {
    graph.foreach { case Vertice(id, name, edges, stopTimes) =>
      SQL("INSERT INTO graph (id, name, edges, stopTimes) VALUES({id}, {name}, {edges}, {stopTimes})").on(
        'id -> id,
        'name -> name,
        'edges -> Json.stringify(Json.toJson(edges)),
        'stopTimes -> Json.stringify(Json.toJson(stopTimes))
      ).executeUpdate
    }
  }

  private def insertTrips(trips: Seq[Trip])(implicit connection: Connection) {
    trips.foreach { trip =>
      SQL("INSERT INTO trips (id , service, direction, stopTimes) VALUES({id}, {service}, {direction}, {stopTimes})").on(
        'id -> trip.id,
        'service -> Json.stringify(Json.toJson(trip.service)),
        'direction -> trip.direction,
        'stopTimes -> Json.stringify(Json.toJson(trip.stopTimes))
      ).executeUpdate
    }
  }

  private def insertStops(stops: Seq[Stop])(implicit connection: Connection) {
    stops.foreach { trip =>
      SQL("INSERT INTO stops (id , name, lat, lng) VALUES({id}, {name}, {lat}, {lng})").on(
        'id -> trip.id,
        'name -> trip.name,
        'lat -> trip.lat,
        'lng -> trip.lng
      ).executeUpdate
    }
  }

  private def setVersion(version: String)(implicit connection: Connection) {
    SQL("INSERT INTO cache (key, value) VALUES({key}, {value})").on(
      'key -> "version",
      'value -> version
    ).executeUpdate
  }
}
