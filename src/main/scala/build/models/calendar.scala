package org.cheminot.db

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

  lazy val off = {
    val now = DateTime.now
    Calendar(
      serviceId = "off",
      monday = false,
      tuesday = false,
      wednesday = false,
      thursday = false,
      friday = false,
      saturday = false,
      sunday = false,
      startDate = now.minusYears(1),
      endDate = now.plusYears(1)
    )
  }

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
}
