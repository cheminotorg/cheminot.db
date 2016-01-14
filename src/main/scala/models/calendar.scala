package m.cheminot.models

import org.joda.time.DateTime

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
}

case class CalendarDate(
  serviceId: String,
  date: DateTime,
  exceptionType: Int
) {
  lazy val id = {
    val timestamp = date.withTimeAtStartOfDay().getMillis()
    s"${serviceId}#${timestamp}#${exceptionType}"
  }
}

object CalendarDate {

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
}
