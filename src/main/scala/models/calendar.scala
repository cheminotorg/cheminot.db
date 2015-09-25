package m.cheminot.models

import org.joda.time.DateTime
import m.cheminot.Gtfs
import m.cheminot.{ CalendarRecord, CalendarDateRecord }

case class Calendar(
  serviceId: String,
  monday: Boolean,
  tuesday: Boolean,
  wednesday: Boolean,
  thursday: Boolean,
  friday: Boolean,
  saturday: Boolean,
  sunday: Boolean,
  startDate: DateTime,
  endDate: DateTime
)

object Calendar {

  import m.cheminot.data.CheminotBuf

  def fromRecord(record: CalendarRecord): Calendar = {
    Calendar(
      record.serviceId,
      record.monday,
      record.tuesday,
      record.wednesday,
      record.thursday,
      record.friday,
      record.saturday,
      record.sunday,
      record.startDate,
      record.endDate
    )
  }

  private def formatDate(date: DateTime): String = {
    val formatter = org.joda.time.format.DateTimeFormat.forPattern("dd/MM/YYYY")
    formatter.print(date)
  }

  private def formatBoolean(bool: Boolean): String =
    if(bool) "1" else "0"

  def serialize(calendar: Calendar): CheminotBuf.Calendar = {
    val builder = CheminotBuf.Calendar.newBuilder()
    builder
      .setServiceId(calendar.serviceId)
      .setMonday(formatBoolean(calendar.monday))
      .setTuesday(formatBoolean(calendar.tuesday))
      .setWednesday(formatBoolean(calendar.wednesday))
      .setThursday(formatBoolean(calendar.thursday))
      .setFriday(formatBoolean(calendar.friday))
      .setSaturday(formatBoolean(calendar.saturday))
      .setSunday(formatBoolean(calendar.sunday))
      .setStartDate(formatDate(calendar.startDate))
      .setEndDate(formatDate(calendar.endDate))
    builder.build()
  }
}

case class CalendarDate(
  serviceId: String,
  date: DateTime,
  exceptionType: Int
)

object CalendarDate {

  import m.cheminot.data.CheminotBuf

  def fromRecord(record: CalendarDateRecord): CalendarDate = {
    CalendarDate(
      record.serviceId,
      record.date,
      record.exceptionType
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

  def serializeCalendarDates(calendarDates: List[CalendarDate], calendar: Seq[Calendar]): CheminotBuf.CalendarDates = {
    val builder = CheminotBuf.CalendarDates.newBuilder()
    calendarDates.groupBy(_.serviceId).foreach {
      case (serviceId, dates) =>
        builder.getMutableExceptionsByServiceId().put(serviceId, serializeCalendarExceptions(dates))
    }
    val unknown = calendar.map(_.serviceId).diff(calendarDates.map(_.serviceId))
    unknown.foreach { serviceId =>
      builder.getMutableExceptionsByServiceId().put(serviceId, serializeCalendarExceptions(Nil))
    }
    builder.build()
  }
}
