package org.cheminot.db.storage

import java.sql.{ Connection, DriverManager }
import anorm._
import rapture.fs._
import rapture.uri._
import org.cheminot.db._

object Sqlite {

  def withConnection[A](dbFile: FsUrl)(block: Connection => A): A = {
    val dbpath = "jdbc:sqlite:" + dbFile.javaFile.getAbsolutePath
    Logger.info(dbpath)
    val connection = DriverManager.getConnection(dbpath)
    val a = block(connection)
    connection.close()
    a
  }

  def init()(implicit connection: Connection): Unit = {
    SQL("PRAGMA synchronous = OFF").executeUpdate
  }

  def createMetaTable()(implicit connection: Connection): Unit = {
    SQL("CREATE TABLE meta (id TEXT PRIMARY KEY, bundledate NUMERIC)").executeUpdate
  }

  def insertMeta(gtfsBundle: GtfsBundle) (implicit connection: Connection): Unit = {
    SQL("INSERT INTO meta (id, bundledate) VALUES({id}, {bundledate})").on(
      'id -> gtfsBundle.id.value,
      'bundledate -> formatDateTime(gtfsBundle.id.date).toLong
    ).executeUpdate
  }

  def createMetaSubsetsTable()(implicit connection: Connection): Unit = {
    SQL("""
      CREATE TABLE metasubset (
        id TEXT PRIMARY KEY,
        metaid TEXT,
        timestamp NUMERIC,
        FOREIGN KEY(metaid) REFERENCES meta(id)
      )""").executeUpdate
  }

  def insertMetaSubsets(gtfsBundle: GtfsBundle) (implicit connection: Connection): Unit = {
    gtfsBundle.subsetDirs.foreach { subsetDir =>
      SQL("""INSERT INTO metasubset (id, metaid, timestamp)
             VALUES({id}, {metaid}, {timestamp})""")
      .on(
        'id -> subsetDir.id,
        'metaid -> gtfsBundle.id.value,
        'timestamp -> formatDateTime(subsetDir.timestamp).toLong
      ).executeUpdate
    }
  }

  def createTripsTable()(implicit connection: Connection): Unit = {
    SQL("CREATE TABLE trip (id TEXT PRIMARY KEY, serviceId TEXT)").executeUpdate
    SQL("CREATE INDEX trip_serviceid ON trip(serviceId)").executeUpdate
  }

  def insertTrips(trips: Map[TripId, Trip])(implicit connection: Connection): Unit = {
    SQL("BEGIN TRANSACTION").executeUpdate

    trips.values.toList.foreach { trip =>
      val query = SQL("INSERT INTO trip (id, serviceid) VALUES({id}, {serviceid})")
      try {
        query.on('id -> trip.id, 'serviceid -> trip.calendar.serviceId).executeUpdate
      } catch {
        case e: Exception =>
          Logger.error(s"Unable to insert trip ${trip.id}: ${e.getMessage}")
      }
    }

    SQL("END TRANSACTION").executeUpdate
  }

  def createStationsFtsTable()(implicit connection: Connection): Unit = {
    SQL("CREATE VIRTUAL TABLE stationfts USING fts4 (id, name)").executeUpdate
  }

  def insertStationsFts(graph: Map[VerticeId, Vertice])(implicit connection: Connection): Unit = {
    SQL("BEGIN TRANSACTION").executeUpdate

    graph.values.toList.foreach { vertice =>
      val query = SQL("INSERT INTO stationfts (id, name) VALUES({id}, {name})")
      try {
        query.on('id -> vertice.id, 'name -> vertice.name).executeUpdate
      } catch {
        case e: Exception =>
          Logger.error(s"Unable to insert stationfts ${vertice.id}: ${e.getMessage}")
      }
    }

    SQL("END TRANSACTION").executeUpdate
  }

  def createStationsTable()(implicit connection: Connection): Unit =  {
    SQL("CREATE TABLE station (id TEXT PRIMARY KEY, name TEXT, parentid TEXT, lat REAL, lng REAL)").executeUpdate
    SQL("CREATE INDEX station_name ON station(name)").executeUpdate
    SQL("CREATE INDEX station_parentid ON station(parentid)").executeUpdate
    SQL("CREATE INDEX station_lat ON station(lat)").executeUpdate
    SQL("CREATE INDEX station_lng ON station(lng)").executeUpdate
  }

  def insertStations(graph: Map[VerticeId, Vertice])(implicit connection: Connection): Unit = {
    SQL("BEGIN TRANSACTION").executeUpdate

    graph.values.toList.foreach { vertice =>
      val query = SQL("INSERT INTO station (id, name, parentid, lat, lng) VALUES({id}, {name}, {parentid}, {lat}, {lng})")
      val parentId = if(Stop.isParis(vertice.id)) Stop.STOP_PARIS else ""
      try {
        query.on('id -> vertice.id, 'name -> vertice.name, 'parentid -> parentId, 'lat -> vertice.lat, 'lng -> vertice.lng).executeUpdate
      } catch {
        case e: Exception =>
          Logger.error(s"Unable to insert station ${vertice.id}: ${e.getMessage}")
      }
    }

    SQL("END TRANSACTION").executeUpdate
  }

  def createStopsTable()(implicit connection: Connection): Unit =  {
    SQL("""
      CREATE TABLE stop (
        id TEXT PRIMARY KEY,
        stationid TEXT,
        parentid TEXT,
        tripid TEXT,
        pos INTEGER,
        FOREIGN KEY(stationid) REFERENCES station(id),
        FOREIGN KEY(tripid) REFERENCES trip(id)
      )""").executeUpdate
    SQL("CREATE INDEX stop_pos ON stop(pos)").executeUpdate
    SQL("CREATE INDEX stop_parentid ON stop(parentid)").executeUpdate
  }

  def insertStops(trips: Map[TripId, Trip])(implicit connection: Connection): Unit = {
    SQL("BEGIN TRANSACTION").executeUpdate
    trips.values.toList.foreach { trip =>
      trip.stopTimes.foreach { stopTime =>
        val parentId = if(Stop.isParis(stopTime.stopId)) Stop.STOP_PARIS else ""
        val query = SQL("INSERT INTO stop (id, stationid, parentid, tripid, pos) VALUES({id}, {stationid}, {parentid}, {tripid}, {pos})")
        try {
          val arrival = formatTime(stopTime.arrival).toInt
          val departure = formatTime(stopTime.departure).toInt
          query.on('id -> stopTime.id, 'stationid -> stopTime.stopId , 'arrival -> arrival, 'departure -> departure, 'parentid -> parentId, 'tripid -> trip.id, 'pos -> stopTime.pos).executeUpdate
        } catch {
          case e: Exception =>
            Logger.error(s"Unable to insert stop ${trip.id}: ${e.getMessage}")
        }
      }
    }

    SQL("END TRANSACTION").executeUpdate
  }

  def createCalendarDatesTable()(implicit connection: Connection): Unit =  {
    SQL("CREATE TABLE calendardate (id TEXT PRIMARY_KEY, serviceid TEXT, date NUMERIC, type INTEGER, FOREIGN KEY(serviceid) REFERENCES calendar(serviceid))").executeUpdate
    SQL("CREATE INDEX calendardate_date ON calendardate(date)").executeUpdate
    SQL("CREATE INDEX calendardate_type ON calendardate(type)").executeUpdate
  }

  def insertCalendarDates(calendarDates: List[CalendarDate])(implicit connection: Connection): Unit = {
    SQL("BEGIN TRANSACTION").executeUpdate

    calendarDates.foreach { calendarDate =>
      val query = SQL("INSERT INTO calendardate (serviceid, date, type) VALUES({serviceid}, {date}, {type})")
      try {
        query.on('id -> calendarDate.id, 'serviceid -> calendarDate.serviceId, 'date -> formatDate(calendarDate.date), 'type -> calendarDate.exceptionType).executeUpdate
      } catch {
        case e: Exception =>
          Logger.error(s"Unable to insert calendardate ${calendarDate.serviceId}: ${e.getMessage}")
      }
    }

    SQL("END TRANSACTION").executeUpdate
  }

  def createCalendarTable()(implicit connection: Connection): Unit =  {
    SQL("""
      CREATE TABLE calendar (
        serviceid TEXT,
        monday NUMERIC,
        tuesday NUMERIC,
        wednesday NUMERIC,
        thursday NUMERIC,
        friday NUMERIC,
        saturday NUMERIC,
        sunday NUMERIC,
        startdate NUMERIC,
        enddate NUMERIC,
        FOREIGN KEY(serviceid) REFERENCES trip(serviceid)
      )
    """).executeUpdate

    SQL("CREATE INDEX calendar_monday ON calendar(monday)").executeUpdate
    SQL("CREATE INDEX calendar_tuesday ON calendar(tuesday)").executeUpdate
    SQL("CREATE INDEX calendar_wednesday ON calendar(wednesday)").executeUpdate
    SQL("CREATE INDEX calendar_thursday ON calendar(thursday)").executeUpdate
    SQL("CREATE INDEX calendar_friday ON calendar(friday)").executeUpdate
    SQL("CREATE INDEX calendar_saturday ON calendar(saturday)").executeUpdate
    SQL("CREATE INDEX calendar_sunday ON calendar(sunday)").executeUpdate
    SQL("CREATE INDEX calendar_startdate ON calendar(startdate)").executeUpdate
    SQL("CREATE INDEX calendar_enddate ON calendar(enddate)").executeUpdate
  }

  def insertCalendar(calendar: Seq[Calendar])(implicit connection: Connection): Unit = {
    SQL("BEGIN TRANSACTION").executeUpdate

    calendar.foreach { calendar =>
      val query = SQL("""
       INSERT INTO calendar (serviceid, monday, tuesday, wednesday, thursday, friday, saturday, sunday, startdate, enddate)
              VALUES({serviceid}, {monday}, {tuesday}, {wednesday}, {thursday}, {friday}, {saturday}, {sunday}, {startdate}, {enddate})
       """)
      try {
        query.on(
          'serviceid -> calendar.serviceId,
          'monday -> calendar.monday,
          'tuesday -> calendar.tuesday,
          'wednesday -> calendar.wednesday,
          'thursday -> calendar.thursday,
          'friday -> calendar.friday,
          'saturday -> calendar.saturday,
          'sunday -> calendar.sunday,
          'startdate -> formatDate(calendar.startDate),
          'enddate -> formatDate(calendar.endDate)
        ).executeUpdate
      } catch {
        case e: Exception =>
          Logger.error(s"Unable to insert calendar ${calendar.serviceId}: ${e.getMessage}")
      }
    }

    SQL("END TRANSACTION").executeUpdate
  }

  def createWithStops(dbDir: FsUrl, db: DB): FsUrl =
    create(dbDir, DB.subset(db, Nil))

  def create(dbDir: FsUrl, db: DB): FsUrl = {
    val outFile = db.outDir(dbDir) / "sqlite" / "cheminot.db"
    outFile.parent.mkdir(makeParents = true)

    withConnection(outFile) { implicit connection =>
      Sqlite.init()

      Logger.info("Meta table")
      Sqlite.createMetaTable()
      Sqlite.insertMeta(db.bundle)

      Logger.info("Meta subsets table")
      Sqlite.createMetaSubsetsTable()
      Sqlite.insertMetaSubsets(db.bundle)

      Logger.info("Trips table")
      Sqlite.createTripsTable()
      Sqlite.insertTrips(db.trips)

      Logger.info("Stations table")
      Sqlite.createStationsTable()
      Sqlite.insertStations(db.graph)

      Logger.info("Stations fts table")
      Sqlite.createStationsFtsTable()
      Sqlite.insertStationsFts(db.graph)

      Logger.info("Stops table")
      Sqlite.createStopsTable()
      Sqlite.insertStops(db.trips)

      Logger.info("CalendarDates table")
      Sqlite.createCalendarDatesTable()
      Sqlite.insertCalendarDates(db.calendarDates)

      Logger.info("Calendar table")
      Sqlite.createCalendarTable()
      Sqlite.insertCalendar(db.calendar)

      Logger.info("DONE")

      outFile
    }
  }
}
