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
    Persist.sqlite(dbDir, db.version, db.trips)
    Persist.graph(dbDir, db.version, db.graph)
    Persist.calendarDates(dbDir, db)
    Persist.ttstops(dbDir, db)
  }

  def sqlite(dbDir: File, version: Version, trips: List[Trip]): File = {
    val file = directory(dbDir, version)(s"cheminot-${version.value}.db")
    println("Storing trips to " + file)
    Sqlite.withConnection(file.getAbsolutePath) { implicit connection =>
      Sqlite.init();
      Sqlite.createMetaTable()
      Sqlite.createTripsTable()
      Sqlite.insertTrips(trips)
      Sqlite.initMeta(version)
      connection.close()
      println("done!")
      file
    }
  }

  def graph(dbDir: File, version: Version, graph: List[Vertice]): File = {
    val file = directory(dbDir, version)(s"graph-${version.value}")
    println("Storing graph to " + file)
    val output = new java.io.FileOutputStream(file)
    Vertice.serializeGraph(graph).writeTo(output)
    println("done!")
    file
  }

  def calendarDates(dbDir: File, db: DB): File = {
    calendarDates(dbDir, db.ter.id, db.version, db.ter.calendarDates, db.ter.calendar)
    calendarDates(dbDir, db.trans.id, db.version, db.trans.calendarDates, db.trans.calendar)
  }

  private def calendarDates(dbDir: File, id: String, version: Version, calendarDates: List[CalendarDate], calendar: Seq[Calendar]): File = {
    val file = directory(dbDir, version)(s"${id}-calendardates-${version.value}")
    println("Storing calendar dates to " + file)
    val output = new java.io.FileOutputStream(file)
    CalendarDate.serializeCalendarDates(calendarDates, calendar).writeTo(output)
    println("done!")
    file
  }

  def ttstops(dbDir: File, db: DB): File = {
    ttstops(dbDir, db.ter.id, db.version, db.ter.ttstops)
    ttstops(dbDir, db.trans.id, db.version, db.trans.ttstops)
  }

  private def ttstops(dbDir: File, id: String, version: Version, ttstops: misc.TTreeNode[(String, String)]): File = {
    val file = directory(dbDir, version)("${id}-stops_ttree.json")
    println("Storing ternary tree stops to " + file)
    val content = Json.stringify(Json.toJson(ttstops))
    FileUtils.write(file, content, "utf-8")
    println("done!")
    file
  }
}
