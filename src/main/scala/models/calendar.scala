package m.cheminot.models

import org.joda.time.DateTime
import play.api.libs.json._
import m.cheminot.Gtfs

case class Calendar(
  serviceId: String,
  monday: String,
  tuesday: String,
  wednesday: String,
  thursday: String,
  friday: String,
  saturday: String,
  sunday: String,
  startDate: DateTime,
  endDate: DateTime
)

object Calendar {

  def fromRow(data: List[String]): Calendar = {
    Calendar(
      data.head,
      data(1),
      data(2),
      data(3),
      data(4),
      data(5),
      data(6),
      data(7),
      Gtfs.parseDateTime(data(8)),
      Gtfs.parseDateTime(data(9))
    )
  }

  implicit val dateTimeWriter = play.api.libs.json.Writes.jodaDateWrites("dd/MM/YYYY")
  implicit val writer: Writes[Calendar] = Json.writes[Calendar]
}

case class CalendarDate(
  serviceId: String,
  date: DateTime,
  exceptionType: Int
)

object CalendarDate {

  import m.cheminot.data.CheminotBuf

  def fromRow(data: List[String]): CalendarDate = {
    CalendarDate(
      data.head,
      Gtfs.parseDateTime(data(1)),
      data(2).toInt
    )
  }

  private def formatDate(date: DateTime): String = {
    val formatter = org.joda.time.format.DateTimeFormat.forPattern("dd/MM/YYYY")
    formatter.print(date)
  }

  def serialize(calendarDate: CalendarDate): CheminotBuf.CalendarDate = {
    val builder = CheminotBuf.CalendarDate.newBuilder()
    builder.setServiceId(calendarDate.serviceId)
           .setDate(formatDate(calendarDate.date))
           .setExceptionType(calendarDate.exceptionType)

    builder.build()
  }

  def serializeCalendarExceptions(exceptions: List[CalendarDate]): CheminotBuf.CalendarExceptions = {
    val builder = CheminotBuf.CalendarExceptions.newBuilder()
    exceptions.foreach { calendarDate =>
      builder.addCalendarDates(serialize(calendarDate))
    }
    builder.build()
  }

  def serializeCalendarDates(calendarDates: List[CalendarDate]): CheminotBuf.CalendarDates = {
    val builder = CheminotBuf.CalendarDates.newBuilder()
    calendarDates.groupBy(_.serviceId).foreach {
      case (serviceId, dates) =>
        builder.getMutableExceptionsByServiceId().put(serviceId, serializeCalendarExceptions(dates))
    }
    builder.build()
  }
}

//   def dateExceptionsToFile(db: DB): File = {
//     val dateExceptionsFile = directory(db.version)("date_exceptions.json")
//     dateExceptionsFile.delete()
//     val json = db.exceptions.groupBy(_.serviceId).foldRight(Json.obj()) {
//       case ((serviceId, exceptions), acc) =>
//         acc ++ Json.obj(serviceId -> Json.toJson(exceptions))
//     }
//     FileUtils.write(dateExceptionsFile, Json.stringify(json))
//     dateExceptionsFile
//   }
// }
