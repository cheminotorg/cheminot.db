package m.cheminot

import java.io.File
import org.apache.commons.io.FileUtils
import play.api.libs.json.Json
import org.joda.time.DateTime
import models._

object Persist {

  private def directory(dbDir: File, version: Version) = {
    val dir = new File(dbDir.getAbsolutePath + "/" + version.value)
    dir.mkdirs
    (file: String) => {
      val f = new File(dir.getPath + "/" + file)
      f.delete()
      f
    }
  }

  def all(dbDir: File, db: DB) {
    Persist.sqlite(dbDir, db.version, db.expiredAt, db.trips)
    Persist.graph(dbDir, db.version, db.graph)
    Persist.calendarDates(dbDir, db.version, db.calendarDates)
    Persist.ttstops(dbDir, db.version, db.ttstops)
  }

  def sqlite(dbDir: File, version: Version, expiredAt: DateTime, trips: List[Trip]): File = {
    val file = directory(dbDir, version)(s"cheminot-${version.value}.db")
    Console.out.println("Storing trips to " + file)
    Sqlite.withConnection(file.getAbsolutePath) { implicit connection =>
      Sqlite.createMetaTable()
      Sqlite.createTripsTable()
      Sqlite.insertTrips(trips)
      Sqlite.initMeta(version, expiredAt)
      file
    }
  }

  def graph(dbDir: File, version: Version, graph: List[Vertice]): File = {
    val file = directory(dbDir, version)(s"graph-${version.value}")
    Console.out.println("Storing graph to " + file)
    val output = new java.io.FileOutputStream(file)
    Vertice.serializeGraph(graph).writeTo(output)
    file
  }

  def calendarDates(dbDir: File, version: Version, calendarDates: List[CalendarDate]): File = {
    val file = directory(dbDir, version)(s"calendardates-${version.value}")
    Console.out.println("Storing calendar dates to " + file)
    val output = new java.io.FileOutputStream(file)
    CalendarDate.serializeCalendarDates(calendarDates).writeTo(output)
    file
  }

  def ttstops(dbDir: File, version: Version, ttstops: misc.TTreeNode[(String, String)]): File = {
    val file = directory(dbDir, version)("stops_ttree.json")
    Console.out.println("Storing ternary tree stops to " + file)
    val content = Json.stringify(Json.toJson(ttstops))
    FileUtils.write(file, content, "utf-8")
    file
  }
}
