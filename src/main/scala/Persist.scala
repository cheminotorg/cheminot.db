package m.cheminot

import java.io.File
import play.api.libs.json.Json
import models._

object Persist {

  private def directory(version: Version) = {
    val dir = new File(Cheminot.dbDirectory + "/" + version.value)
    dir.mkdirs
    (file: String) => {
      val f = new File(dir.getPath + "/" + file)
      f.delete()
      f
    }
  }

  def sqlite(version: Version, trips: List[Trip]): File = {
    val file = directory(version)("cheminot.db")
    Sqlite.withConnection(file.getAbsolutePath) { implicit connection =>
      Sqlite.createMetaTable()
      Sqlite.createTripsTable()
      Sqlite.insertTrips(trips)
      Sqlite.setVersion(version)
      file
    }
  }

  def graph(version: Version, graph: List[Vertice]): File = {
    val file = directory(version)("graph")
    val output = new java.io.FileOutputStream(file)
    Vertice.serializeSeq(graph).writeTo(output)
    file
  }

  def calendarDates(version: Version, calendarDates: List[CalendarDate]): File = {
    val file = directory(version)("calendar")
    val output = new java.io.FileOutputStream(file)
    CalendarDate.serializeSeq(calendarDates).writeTo(output)
    file
  }
}
