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
) {

  override def equals(o: Any): Boolean =
    o match {
      case c: Calendar =>
        monday == c.monday &&
        tuesday == c.tuesday &&
        wednesday == c.wednesday &&
        thursday == c.thursday &&
        friday == c.friday &&
        saturday == c.saturday &&
        sunday == c.sunday &&
        startDate.equals(c.startDate) &&
        endDate.equals(c.endDate)

      case _ => false
    }

  override def hashCode =
    List(
      monday.toString,
      tuesday.toString,
      wednesday.toString,
      thursday.toString,
      friday.toString,
      saturday.toString,
      sunday.toString,
      startDate.toString,
      endDate.toString
    ).mkString.hashCode
}

object Calendar {

  lazy val on: Calendar = { 
    val now = DateTime.now
    Calendar(
      serviceId = "off",
      monday = true,
      tuesday = true,
      wednesday = true,
      thursday = true,
      friday = true,
      saturday = true,
      sunday = true,
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
