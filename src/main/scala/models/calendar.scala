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

  implicit val reader: Reads[Calendar] = Json.reads[Calendar]
  implicit val dateTimeWriter = play.api.libs.json.Writes.jodaDateWrites("dd/MM/YYYY")
  implicit val writer: Writes[Calendar] = Json.writes[Calendar]
}

case class CalendarDate(
  serviceId: String,
  date: DateTime,
  exceptionType: Int
)

object CalendarDate {
  def fromRow(data: List[String]): CalendarDate = {
    CalendarDate(
      data.head,
      Gtfs.parseDateTime(data(1)),
      data(2).toInt
    )
  }

  def serialize(calendar: List[CalendarDate]): Array[Byte] = {
    ???
  }

  implicit val reader: Reads[CalendarDate] = Json.reads[CalendarDate]
  implicit val writer: Writes[CalendarDate] = Json.writes[CalendarDate]
}
