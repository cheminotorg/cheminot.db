package m.cheminot

import play.api.libs.json._

object Main {

  Class.forName("org.sqlite.JDBC");

  def main(args: Array[String]) {
    val db = DB.buildFrom(new java.io.File(Cheminot.gtfsDirectory + "/2014-10-03_09-29-00"))
    Sqlite.create(db)
  }
}
