package m.cheminot

import java.io.File
import rapture.core._
import rapture.cli._

case class Config(dbDir: File, gtfsDir: File, daemon: Boolean, port: Int)

object Config {

  implicit val fileExtractor: New.Param.Extractor[File] = new New.Param.Extractor[File] {
    def extract(values: Vector[String]): Option[File] = values match {
      case Vector(v) => Option(new File(v)).filter(_.exists)
      case _ => None
    }
  }

  implicit val booleanExtractor: New.Param.Extractor[Boolean] = new New.Param.Extractor[Boolean] {
    def extract(values: Vector[String]): Option[Boolean] = values match {
      case _:Vector[String] => Option(true)
      case _ => None
    }
  }

  import modes.returnOption._

  val DbDir = New.Param[File]('o', 'out)
  val GtfsDir = New.Param[File]('g', 'gtfs)
  val Daemon = New.Param[Boolean]('d', 'daemon)
  val Port = New.Param[Int]('p', 'port)

  def apply(args: Array[String]): Config = {
    val params = New.ParamMap(args:_*)
    Config(
      dbDir = DbDir.parse(params) getOrElse build.DB.defaultDbDir,
      gtfsDir = GtfsDir.parse(params) getOrElse build.GtfsBundle.defaultRoot,
      daemon = Daemon.parse(params) getOrElse false,
      port = Port.parse(params) getOrElse 8080
    )
  }
}
