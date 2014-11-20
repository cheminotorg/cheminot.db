package m.cheminot

import scopt._

object Main {

  Class.forName("org.sqlite.JDBC");

  def main(args: Array[String]) {
    args match {
      case Array("trips", path) =>
      case Array("graph", path) =>
      case Array("calendar", path) =>
      case _ =>
    }
    // val db = DB.buildFrom(new java.io.File(Cheminot.gtfsDirectory + "/2014-10-03_09-29-00"))
    // Persist.tripsToSqlite(db)
    // Persist.graphToFile(db)
    // Persist.dateExceptionsToFile(db)
  }
}
