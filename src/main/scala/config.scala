package org.cheminot.db

import rapture.core._
import rapture.cli._
import rapture.fs._

case class Config(dbDir: FsUrl, gtfsDir: FsUrl, daemon: Boolean)

object Config {

  implicit val fileExtractor: New.Param.Extractor[FsUrl] = new New.Param.Extractor[FsUrl] {
    def extract(values: Vector[String]): Option[FsUrl] = values match {
      case Vector(v) =>
        scala.util.Try(File.parse(v)).toOption.filter(_.exists)
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

  val DbDir = New.Param[FsUrl]('o', 'out)
  val GtfsDir = New.Param[FsUrl]('g', 'gtfs)
  val Daemon = New.Param[Boolean]('n', 'daemon)

  def apply(args: Array[String]): Config = {
    val params = New.ParamMap(args:_*)
    Config(
      dbDir = DbDir.parse(params) getOrElse DB.defaultDbDir,
      gtfsDir = GtfsDir.parse(params) getOrElse GtfsBundle.defaultRoot,
      daemon = Daemon.parse(params) getOrElse false
    )
  }
}
