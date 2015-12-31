package m.cheminot.models

import org.joda.time.DateTime
import m.cheminot.Gtfs

case class StopTime(
  tripId: String,
  arrival: Option[DateTime],
  departure: Option[DateTime],
  stopId: String,
  pos: Int
)

object StopTime {

  def fromRecord(record: StopTimeRecord): StopTime = {
    StopTime(
      record.tripId,
      Option(record.arrival),
      Option(record.departure),
      record.stopId,
      record.stopSeq
    )
  }

  def formatTime(time: DateTime): String = {
    val formatter = org.joda.time.format.DateTimeFormat.forPattern("HH:mm")
    formatter.print(time)
  }
}
