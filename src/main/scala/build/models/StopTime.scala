package m.cheminot.build

import org.joda.time.DateTime

case class StopTime(
  tripId: String,
  arrival: DateTime,
  departure: DateTime,
  stopId: String,
  pos: Int
) {
  lazy val id = StopTime.id(tripId, stopId)
}

object StopTime {

  def id(tripId: TripId, stopId: StopId): String =
    s"${tripId}#${stopId}"

  def fromRecord(record: StopTimeRecord): StopTime = {
    StopTime(
      record.tripId,
      record.arrival,
      record.departure,
      record.stopId,
      record.stopSeq
    )
  }

  def formatTime(time: DateTime): String = {
    val formatter = org.joda.time.format.DateTimeFormat.forPattern("HH:mm")
    formatter.print(time)
  }
}
