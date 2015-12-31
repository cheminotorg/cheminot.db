package m.cheminot

import java.io.File
import org.apache.commons.io.FileUtils
import play.api.libs.json.Json
import org.joda.time.DateTime
import models._
import storage._

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
    Persist.graph(dbDir, db)
  }

  def graph(dbDir: File, db: DB): Unit = {
    Neo4j.insertGraph(dbDir, db)
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
