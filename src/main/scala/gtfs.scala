package m.cheminot

import java.io.File
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import org.joda.time.DateTime
import misc._
import models._

object Gtfs {

  val archiveFileNames = Seq(
    "agency.txt",
    "calendar.txt",
    "calendar_dates.txt",
    "routes.txt",
    "stop_times.txt",
    "stops.txt",
    "transfers.txt",
    "trips.txt"
  )

  def parseName(str: String): Option[DateTime] = {
      scala.util.control.Exception.allCatch[DateTime] opt (DateTime.parse(str, Version.format))
  }

  val gtfsDirectoryOnly = new PartialFunction[File, (File, DateTime)] {
    def apply(directory: File) = {
      directory -> DateTime.parse(directory.getName, Version.format)
    }

    def isDefinedAt(directory: File) = {
      directory.exists &&
      directory.isDirectory &&
      directory.canWrite &&
      scala.util.control.Exception.allCatch[DateTime].opt {
        DateTime.parse(directory.getName, Version.format)
      }.isDefined &&
      archiveFileNames.forall { fileName =>
        directory.listFiles.toList.find(_.getName == fileName).isDefined
      }
    }
  }

  def createDirectory(name: String): File = {
    val directory = new File(Cheminot.gtfsDirectory + "/" + name)
    directory.mkdirs
    directory
  }
}

case class GtfsDirectory(gtfs: Map[String, CSVFile.Rows]) {
  lazy val stopTimes = {
    val rows = gtfs.get("stop_times.txt").getOrElse(Cheminot.oops("Invalid gtfs format: stop_times.txt not found!"))
    rows.drop(1).filter(_.size == 9)
  }

  lazy val trips = {
    val rows = gtfs.get("trips.txt").getOrElse(Cheminot.oops("Invalid gtfs format: trips.txt not found!"))
    rows.drop(1).dropRight(1).filter(_.size == 7)
  }

  lazy val stops = {
    val rows = gtfs.get("stops.txt").getOrElse(Cheminot.oops("Invalid gtfs format: stops.txt not found!"))
    rows.drop(1).par.filter(row => row(0).startsWith("StopPoint:OCETrain") && row.size == 9).toList
  }

  lazy val calendar = {
    val rows = gtfs.get("calendar.txt").getOrElse(Cheminot.oops("Invalid gtfs format: calendar.txt not found!"))
    rows.drop(1).dropRight(1).filter(_.size == 10)
  }

  lazy val calendarDates = {
    val rows = gtfs.get("calendar_dates.txt").getOrElse(Cheminot.oops("Invalid gtfs format: calendar_dates.txt not found!"))
    rows.drop(1).filter(_.size == 3)
  }
}
