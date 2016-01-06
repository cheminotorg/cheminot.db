package m.cheminot.storage

import java.io.File
import m.cheminot.{ DB, Version }
import m.cheminot.misc.CSVWriteFile
import m.cheminot.models._

object Neo4j {

  private def write(dir: File, name: String, headers: List[String], data: List[List[String]]): Unit = {
    val headerFile = new File(s"""${dir}/${name}_headers.csv""")
    val dataFile = new File(s"""${dir}/${name}1.csv""")
    println(s"Writing to ${dataFile}")
    CSVWriteFile(dataFile).write(headers +: data)
  }

  private def stopId(trip: Trip, stopTime: StopTime): String =
    s"${trip.id}#${stopTime.stopId}"

  private def calendarDateId(calendarDate: CalendarDate): String = {
    val date = calendarDate.date.withTimeAtStartOfDay().getMillis()
    s"${calendarDate.serviceId}#${date}#${calendarDate.exceptionType}"
  }

  def writeIndexes(outdir: File, db: DB): Unit = {
    import scala.collection.JavaConverters._

    val lines = List(
      "CREATE INDEX ON :Station(stationid);",
      "CREATE INDEX ON :Stop(stationid);",
      "CREATE INDEX ON :Stop(stopid);",
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
    val file = new File(s"""${outdir}/${db.version.value}/indexes.cypher""")
    org.apache.commons.io.FileUtils.writeLines(file, lines.asJava)
  }

  object Nodes {

    def writeStations(outdir: File, db: DB): Unit = {
      val headers = List("stationid:ID(Station)", "name:string", "lat:double", "lng:double", ":LABEL")
      val data = db.graph.values.toList.map { vertice =>
        List(vertice.id, vertice.name, vertice.lat.toString, vertice.lng.toString, "Station")
      }
      write(outdir, name = "stations", headers = headers, data = data)
    }

    def writeStops(outdir: File, db: DB): Unit = {
      val headers = List("stopid:ID(Stop)", "stationid:string", ":LABEL")
      val data = db.trips.values.toList.flatMap { trip =>
        trip.stopTimes.map { stopTime =>
          List(stopId(trip, stopTime), stopTime.stopId, "Stop")
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
      val formatDateTime = (date: org.joda.time.DateTime) => {
        (date.withTimeAtStartOfDay().getMillis() / 1000).toString
      }
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
          formatDateTime(calendar.startDate),
          formatDateTime(calendar.endDate),
          "Calendar"
        )
      }
      write(outdir, name = "calendar", headers = headers, data = data)
    }

    def writeCalendarDates(outdir: File, db: DB): Unit = {
      val formatDateTime = (date: org.joda.time.DateTime) => {
        (date.withTimeAtStartOfDay().getMillis() / 1000).toString
      }
      val headers = List("calendardateid:ID(CalendarDate)", "serviceid:string", "date:int", ":LABEL")
      val data = db.calendarDates.map { calendarDate =>
        List(calendarDateId(calendarDate), calendarDate.serviceId, formatDateTime(calendarDate.date), "CalendarDate")
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
            List(stopId(trip, stopTime), stopTime.stopId, "AT")
          }
        }
      }
      write(outdir, name = "stop2station", headers = headers, data = data)
    }

    def writeTrip2Stop(outdir: File, db: DB): Unit = {
      val headers = List(":START_ID(Trip)", ":END_ID(Stop)", ":TYPE")
      val data = db.trips.values.toList.flatMap { trip =>
        trip.stopTimes.map { stopTime =>
          List(trip.id, stopId(trip, stopTime), "GOES_TO")
        }
      }
      write(outdir, name = "trip2stops", headers = headers, data = data)
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
        List(calendarDate.serviceId, calendarDateId(calendarDate), `type`)
      }
      write(outdir, name = "calendar2calendardates", headers = headers, data = data)
    }

    def writeStop2Stop(outdir: File, db: DB): Unit = {
      val formatTime = (time: org.joda.time.DateTime) => {
        val formatter = org.joda.time.format.DateTimeFormat.forPattern("HHmm")
        formatter.print(time)
      }
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
            departure <- stopTime.departure.map(formatTime)
            arrival <- next.arrival.map(formatTime)
          } yield {
            List(stopId(trip, stopTime), departure, arrival, stopId(trip, next), "GOES_TO")
          }
        }
      }
      write(outdir, name = "ways", headers = headers, data = data)
    }
  }

  def writeGraph(dbDir: File, db: DB): Unit = {
    val outdir = new File(dbDir.getAbsolutePath + "/" + db.version.value)
    Nodes.writeStations(outdir, db);
    Nodes.writeTrips(outdir, db)
    Nodes.writeStops(outdir, db)
    Nodes.writeCalendar(outdir, db)
    Nodes.writeCalendarDates(outdir, db)
    Relationships.writeStop2Stop(outdir, db)
    Relationships.writeTrip2Stop(outdir, db)
    Relationships.writeTrip2Calendar(outdir, db)
    Relationships.writeCalendar2CalendarDates(outdir, db)
    Relationships.writeStop2Stop(outdir, db)
    writeIndexes(dbDir, db);
  }
}
