package m.cheminot.storage

import java.io.File
import m.cheminot._
import m.cheminot.misc.CSVWriteFile
import m.cheminot.models.{ Stop, StopTime }

object Neo4j {

  type Row = List[String]

  private def write(dir: File, name: String, headers: Row, data: List[Row]): Unit = {
    val headerFile = new File(s"""${dir}/${name}_headers.csv""")
    val dataFile = new File(s"""${dir}/${name}1.csv""")
    println(s"Writing to ${dataFile}")
    CSVWriteFile(dataFile).write(headers +: data)
  }

  def writeIndexes(outdir: File, db: DB): Unit = {
    import scala.collection.JavaConverters._

    val lines = List(
      "CREATE INDEX ON :Meta(metaid);",
      "CREATE INDEX ON :MetaSubset(metasubsetid);",
      "CREATE INDEX ON :Station(stationid);",
      "CREATE INDEX ON :Station(parentid);",
      "CREATE INDEX ON :Stop(stationid);",
      "CREATE INDEX ON :Stop(stopid);",
      "CREATE INDEX ON :Stop(parentid);",
      "CREATE INDEX ON :Calendar(serviceid);",
      "CREATE INDEX ON :Calendar(monday);",
      "CREATE INDEX ON :Calendar(tuesday);",
      "CREATE INDEX ON :Calendar(wednesday);",
      "CREATE INDEX ON :Calendar(thursday);",
      "CREATE INDEX ON :Calendar(friday);",
      "CREATE INDEX ON :Calendar(saturday);",
      "CREATE INDEX ON :Calendar(sunday);",
      "CREATE INDEX ON :Calendar(startdate);",
      "CREATE INDEX ON :Calendar(enddate);",
      "CREATE INDEX ON :CalendarDate(date);",
      "CREATE INDEX ON :Trip(serviceid);"
    )
    val file = new File(s"""${outdir}/indexes.cypher""")
    org.apache.commons.io.FileUtils.writeLines(file, lines.asJava)
  }

  object Nodes {

    def writeMeta(outdir: File, db: DB): Unit = {
      val headers = List("metaid:ID(Meta)", "bundledate:int", ":LABEL")
      val data = List(List(db.meta.id, formatDateTime(db.meta.bundleDate), "Meta"))
      write(outdir, name = "meta", headers = headers, data = data)
    }

    def writeMetaSubsets(outdir: File, db: DB): Unit = {
      val headers = List("metasubsetid:ID(MetaSubset)", "updateddate:int", "startdate:int", "enddate:int", ":LABEL")
      val data = db.meta.subsets.map { subset =>
        List(subset.id, formatDate(subset.updatedDate), formatDate(subset.startDate), formatDate(subset.endDate), "MetaSubset")
      }
      write(outdir, name = "metasubsets", headers = headers, data = data)
    }

    def writeStations(outdir: File, db: DB): Unit = {
      val headers = List("stationid:ID(Station)", "name:string", "parentid:string", "lat:double", "lng:double", ":LABEL")
      val data = db.graph.values.toList.map { stop =>
        val parentId = if(Stop.isParis(stop.id)) Stop.STOP_PARIS else ""
        List(stop.id, stop.name, parentId, stop.lat.toString, stop.lng.toString, "Station")
      }
      write(outdir, name = "stations", headers = headers, data = data)
    }

    def writeStops(outdir: File, db: DB): Unit = {
      val headers = List("stopid:ID(Stop)", "stationid:string", "parentid:string", ":LABEL")
      val data = db.trips.values.toList.flatMap { trip =>
        trip.stopTimes.map { stopTime =>
          val parentId = if(Stop.isParis(stopTime.stopId)) Stop.STOP_PARIS else ""
          List(stopTime.id, stopTime.stopId, parentId, "Stop")
        }
      }
      write(outdir, name = "stops", headers = headers, data = data)
    }

    def writeTrips(outdir: File, db: DB): Unit = {
      val headers = List("tripid:ID(Trip)", "serviceid:string", ":LABEL")
      val data = db.trips.values.toList.map { trip =>
        List(trip.id, trip.calendar.map(_.serviceId).getOrElse(""), "Trip")
      }
      write(outdir, name = "trips", headers = headers, data = data)
    }

    def writeCalendar(outdir: File, db: DB): Unit = {
      val headers = List(
        "serviceid:ID(Calendar)",
        "monday:boolean",
        "tuesday:boolean",
        "wednesday:boolean",
        "thursday:boolean",
        "friday:boolean",
        "saturday:boolean",
        "sunday:boolean",
        "startdate:int",
        "enddate:int",
        ":LABEL"
      )

      val data = db.calendar.map { calendar =>
        List(
          calendar.serviceId,
          calendar.monday.toString,
          calendar.tuesday.toString,
          calendar.wednesday.toString,
          calendar.thursday.toString,
          calendar.friday.toString,
          calendar.saturday.toString,
          calendar.sunday.toString,
          formatDate(calendar.startDate),
          formatDate(calendar.endDate),
          "Calendar"
        )
      }

      write(outdir, name = "calendar", headers = headers, data = data)
    }

    def writeCalendarDates(outdir: File, db: DB): Unit = {
      val headers = List("calendardateid:ID(CalendarDate)", "serviceid:string", "date:int", ":LABEL")
      val data = db.calendarDates.map { calendarDate =>
        List(calendarDate.id, calendarDate.serviceId, formatDate(calendarDate.date), "CalendarDate")
      }
      write(outdir, name = "calendardates", headers = headers, data = data)
    }
  }

  object Relationships {

    def writeStop2Station(outdir: File, db: DB): Unit = {
      val headers = List(":START_ID(Stop)", ":END_ID(Station)", ":TYPE")
      val data = db.trips.values.toList.flatMap { trip =>
        trip.stopTimes.flatMap { stopTime =>
          db.trips.get(stopTime.tripId).orElse {
            println(">> Not a valid trip " + stopTime.tripId)
            None
          }.map { trip =>
            List(stopTime.id, stopTime.stopId, "AT")
          }
        }
      }
      write(outdir, name = "stop2station", headers = headers, data = data)
    }

    def writeTrip2Stop(outdir: File, db: DB): Unit = {
      val headers = List(":START_ID(Trip)", ":END_ID(Stop)", "arrival:int", ":TYPE")
      val data = db.trips.values.toList.flatMap { trip =>
        trip.stopTimes.headOption.toList.map { stopTime =>
          List(trip.id, stopTime.id, formatTime(stopTime.arrival), "GOES_TO")
        }
      }
      write(outdir, name = "trip2stop", headers = headers, data = data)
    }

    def writeTrip2Calendar(outdir: File, db: DB): Unit = {
      val headers = List(":START_ID(Trip)", ":END_ID(Calendar)", ":TYPE")
      val data = db.trips.values.toList.flatMap { trip =>
        trip.calendar.map { calendar =>
          List(trip.id, calendar.serviceId, "SCHEDULED_AT")
        }
      }
      write(outdir, name = "trip2calendar", headers = headers, data = data)
    }

    def writeCalendar2CalendarDates(outdir: File, db: DB): Unit = {
      val headers = List(":START_ID(Calendar)", ":END_ID(CalendarDate)", ":TYPE")
      val data = db.calendarDates.map { calendarDate =>
        val `type` = if(calendarDate.exceptionType == 1) "ON" else "OFF"
        List(calendarDate.serviceId, calendarDate.id, `type`)
      }
      write(outdir, name = "calendar2calendardates", headers = headers, data = data)
    }

    def writeStop2Stop(outdir: File, db: DB): Unit = {
      val headers = List(":START_ID(Stop)", "departure:int", "arrival:int", ":END_ID(Stop)", ":TYPE")
      val data = db.trips.values.toList.flatMap { trip =>
        trip.stopTimes.flatMap { stopTime =>
          for {
            trip <- db.trips.get(stopTime.tripId).orElse {
              println(">> Not a valid trip " + stopTime.tripId)
              None
            }
            next <- trip.stopTimes.lift(stopTime.pos + 1)
            serviceId <- trip.calendar.map(_.serviceId)
          } yield {
            val departure = formatTime(stopTime.departure)
            val arrival = formatTime(next.arrival)
            List(stopTime.id, departure, arrival, next.id, "GOES_TO")
          }
        }
      }
      write(outdir, name = "stop2stop", headers = headers, data = data)
    }

    def writeMeta2MetaSubsets(outdir: File, db: DB): Unit = {
      val headers = List(":START_ID(Meta)", ":END_ID(MetaSubset)", ":TYPE")
      val data = db.meta.subsets.map { subset =>
        List(db.meta.id, subset.id, "HAS")
      }
      write(outdir, name = "meta2metasubsets", headers = headers, data = data)
    }
  }

  def write(dbDir: File, db: DB): Unit = {
    val outdir = new File(dbDir.getAbsolutePath + "/" + db.meta.id + "/" + db.id)
    outdir.mkdirs

    Nodes.writeStations(outdir, db);
    Nodes.writeTrips(outdir, db)
    Nodes.writeStops(outdir, db)
    Nodes.writeCalendar(outdir, db)
    Nodes.writeCalendarDates(outdir, db)
    Nodes.writeMeta(outdir, db)
    Nodes.writeMetaSubsets(outdir, db)
    Relationships.writeMeta2MetaSubsets(outdir, db)
    Relationships.writeStop2Station(outdir, db)
    Relationships.writeStop2Stop(outdir, db)
    Relationships.writeTrip2Stop(outdir, db)
    Relationships.writeTrip2Calendar(outdir, db)
    Relationships.writeCalendar2CalendarDates(outdir, db)
    writeIndexes(outdir, db);
  }
}
