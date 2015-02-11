package m.cheminot

import java.io.File
import org.apache.commons.io.FileUtils
import play.api.libs.json.Json
import org.joda.time.DateTime
import models._

object Persist {

  private def directory(version: Version) = {
    val dir = new File("db/" + version.value)
    dir.mkdirs
    (file: String) => {
      val f = new File(dir.getPath + "/" + file)
      f.delete()
      f
    }
  }

  def all(db: DB) {
    Persist.sqlite(db.version, db.expiredAt, db.trips)
    Persist.graph(db.version, db.graph)
    Persist.calendarDates(db.version, db.calendarDates)
    Persist.ttstops(db.version, db.ttstops)
  }

  def sqlite(version: Version, expiredAt: DateTime, trips: List[Trip]): File = {
    val file = directory(version)("cheminot.db")
    Console.out.println("Storing trips to " + file)
    Sqlite.withConnection(file.getAbsolutePath) { implicit connection =>
      Sqlite.createMetaTable()
      Sqlite.createTripsTable()
      Sqlite.insertTrips(trips)
      Sqlite.initMeta(version, expiredAt)
      file
    }
  }

  def graph(version: Version, graph: List[Vertice]): File = {
    val file = directory(version)("graph")
    Console.out.println("Storing graph to " + file)
    val output = new java.io.FileOutputStream(file)
    Vertice.serializeGraph(graph).writeTo(output)
    file
  }

  def calendarDates(version: Version, calendarDates: List[CalendarDate]): File = {
    val file = directory(version)("calendar_dates")
    Console.out.println("Storing calendar dates to " + file)
    val output = new java.io.FileOutputStream(file)
    CalendarDate.serializeCalendarDates(calendarDates).writeTo(output)
    file
  }

  def ttstops(version: Version, ttstops: misc.TTreeNode[(String, String)]): File = {
    val file = directory(version)("stops_ttree.json")
    Console.out.println("Storing ternary tree stops to " + file)
    val content = Json.stringify(Json.toJson(ttstops))
    FileUtils.write(file, content, "utf-8")
    file
  }
}
