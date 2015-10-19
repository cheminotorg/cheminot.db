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
    Persist.sqlite(dbDir, db)
    Persist.graph(dbDir, db)
    Persist.calendarDates(dbDir, db)
    Persist.ttstops(dbDir, db)
  }

  def sqlite(dbDir: File, db: DB): File = {
    val file = directory(dbDir, db.version)(s"cheminot-${db.version.value}.db")
    println("Storing trips to " + file)
    Sqlite.withConnection(file.getAbsolutePath) { implicit connection =>
      Sqlite.init();
      Sqlite.createMetaTable()
      Sqlite.createTripsTable()
      Sqlite.insertTrips('TER -> db.terTrips, 'TRANS -> db.transTrips, 'INTER -> db.interTrips)
      Sqlite.initMeta(db.version)
      connection.close()
      println("done!")
      file
    }
  }

  def graph(dbDir: File, db: DB): File = {
    graph(dbDir, db.ter.id, db.version, db.ter.graph)
    graph(dbDir, db.trans.id, db.version, db.trans.graph)
    graph(dbDir, db.inter.id, db.version, db.inter.graph)
  }

  private def graph(dbDir: File, id: String, version: Version, graph: List[Vertice]): File = {
    val file = directory(dbDir, version)(s"${id}-graph-${version.value}")
    println("Storing graph to " + file)
    val output = new java.io.FileOutputStream(file)
    Vertice.serializeGraph(graph).writeTo(output)
    println("done!")
    file
  }

  def calendarDates(dbDir: File, db: DB): File = {
    calendarDates(dbDir, db.ter.id, db.version, db.ter.calendarDates, db.ter.calendar)
    calendarDates(dbDir, db.trans.id, db.version, db.trans.calendarDates, db.trans.calendar)
    calendarDates(dbDir, db.inter.id, db.version, db.inter.calendarDates, db.inter.calendar)
  }

  private def calendarDates(dbDir: File, id: String, version: Version, calendarDates: List[CalendarDate], calendar: Seq[Calendar]): File = {
    val file = directory(dbDir, version)(s"${id}-calendardates-${version.value}")
    println("Storing calendar dates to " + file)
    val output = new java.io.FileOutputStream(file)
    CalendarDate.serializeCalendarDates(calendarDates, calendar).writeTo(output)
    println("done!")
    file
  }

  def ttstops(dbDir: File, db: DB) {
    ttstops(dbDir, db.version, db.ttstops)
  }

  private def ttstops(dbDir: File, version: Version, ttstops: misc.TTreeNode[(String, String)]): File = {
    val file = directory(dbDir, version)("stops_ttree.json")
    println("Storing ternary tree stops to " + file)
    val content = Json.stringify(Json.toJson(ttstops))
    FileUtils.write(file, content, "utf-8")
    println("done!")
    file
  }
}
