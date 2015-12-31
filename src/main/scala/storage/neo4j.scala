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

  private def calendarDateId(calendarDate: CalendarDate): String =
    s"${calendarDate.serviceId}#${calendarDate.date.withTimeAtStartOfDay().getMillis()}#${calendarDate.exceptionType}"

  def indexes(outdir: File, db: DB): Unit = {
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

    def stations(outdir: File, db: DB): Unit = {
      val headers = List("stationid:ID(Station)", "name:string", "lat:double", "lng:double", ":LABEL")
      val data = db.graph.values.toList.map { vertice =>
        List(vertice.id, vertice.name, vertice.lat.toString, vertice.lng.toString, "Station")
      }
      write(outdir, name = "stations", headers = headers, data = data)
    }

    def stops(outdir: File, db: DB): Unit = {
      val headers = List("stopid:ID(Stop)", "stationid:string", ":LABEL")
      val data = db.trips.values.toList.flatMap { trip =>
        trip.stopTimes.map { stopTime =>
          List(stopId(trip, stopTime), stopTime.stopId, "Stop")
        }
      }
      write(outdir, name = "stops", headers = headers, data = data)
    }

    def trips(outdir: File, db: DB): Unit = {
      val headers = List("tripid:ID(Trip)", "serviceid:string", ":LABEL")
      val data = db.trips.values.toList.map { trip =>
        List(trip.id, trip.calendar.map(_.serviceId).getOrElse(""), "Trip")
      }
      write(outdir, name = "trips", headers = headers, data = data)
    }

    def calendar(outdir: File, db: DB): Unit = {
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

    def calendarDates(outdir: File, db: DB): Unit = {
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

    def trip2Stops(outdir: File, db: DB): Unit = {
      val headers = List(":START_ID(Trip)", ":END_ID(Stop)", ":TYPE")
      val data = db.trips.values.toList.flatMap { trip =>
        trip.stopTimes.map { stopTime =>
          List(trip.id, stopId(trip, stopTime), "GOES_TO")
        }
      }
      write(outdir, name = "trip2stops", headers = headers, data = data)
    }

    def trip2Calendar(outdir: File, db: DB): Unit = {
      val headers = List(":START_ID(Trip)", ":END_ID(Calendar)", ":TYPE")
      val data = db.trips.values.toList.flatMap { trip =>
        trip.calendar.map { calendar =>
          List(trip.id, calendar.serviceId, "SCHEDULED_AT")
        }
      }
      write(outdir, name = "trip2calendar", headers = headers, data = data)
    }

    def calendar2CalendarDates(outdir: File, db: DB): Unit = {
      val headers = List(":START_ID(Calendar)", ":END_ID(CalendarDate)", ":TYPE")
      val data = db.calendarDates.map { calendarDate =>
        val `type` = if(calendarDate.exceptionType == 1) "ON" else "OFF"
        List(calendarDate.serviceId, calendarDateId(calendarDate), `type`)
      }
      write(outdir, name = "calendar2calendardates", headers = headers, data = data)
    }

    def tripWays(outdir: File, db: DB): Unit = {
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

  def insertGraph(dbDir: File, db: DB): Unit = {
    val outdir = new File(dbDir.getAbsolutePath + "/" + db.version.value)
    Nodes.stations(outdir, db);
    Nodes.trips(outdir, db)
    Nodes.stops(outdir, db)
    Nodes.calendar(outdir, db)
    Nodes.calendarDates(outdir, db)
    Relationships.tripWays(outdir, db)
    Relationships.trip2Stops(outdir, db)
    Relationships.trip2Calendar(outdir, db)
    Relationships.calendar2CalendarDates(outdir, db)
    indexes(dbDir, db);
  }
}
