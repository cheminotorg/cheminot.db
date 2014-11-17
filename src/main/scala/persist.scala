package m.cheminot

import java.io.File
import org.apache.commons.io.FileUtils
import play.api.libs.json.Json
import models._

object Persist {

  private def directory(version: String) = {
    val dir = new File(Cheminot.dbDirectory + "/" + version)
    dir.mkdirs
    (file: String) => {
      val f = new File(dir.getPath + "/" + file)
      f.delete()
      f
    }
  }

  def tripsToSqlite(db: DB): File = {
    val dbFile = directory(db.version)("cheminot.db")
    Sqlite.withConnection(dbFile.getAbsolutePath) { implicit connection =>
      Sqlite.createMetaTable()
      Sqlite.createTripsTable()
      Sqlite.insertTrips(db.trips)
      Sqlite.setVersion(db.version)
      dbFile
    }
  }

  def graphToFile(db: DB): File = {
    val graphFile = directory(db.version)("graph.json")
    graphFile.delete()
    val content = Json.stringify(Json.toJson(db.graph))
    FileUtils.write(graphFile, content)
    graphFile
  }

  def dateExceptionsToFile(db: DB): File = {
    val dateExceptionsFile = directory(db.version)("date_exceptions.json")
    dateExceptionsFile.delete()
    val json = db.exceptions.groupBy(_.serviceId).foldRight(Json.obj()) {
      case ((serviceId, exceptions), acc) =>
        acc ++ Json.obj(serviceId -> Json.toJson(exceptions))
    }
    FileUtils.write(dateExceptionsFile, Json.stringify(json))
    dateExceptionsFile
  }
}
