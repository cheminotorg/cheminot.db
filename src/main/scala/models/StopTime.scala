package m.cheminot.models

import org.joda.time.DateTime
import m.cheminot.Gtfs
import m.cheminot.StopTimeRecord

case class StopTime(
  tripId: String,
  arrival: Option[DateTime],
  departure: Option[DateTime],
  stopId: String,
  pos: Int
)

object StopTime {

  import m.cheminot.data.CheminotBuf

  def fromRecord(record: StopTimeRecord): StopTime = {
    StopTime(
      record.tripId,
      Some(record.arrival),
      Some(record.departure),
      record.stopId,
      record.stopSeq
    )
  }

  def formatTime(time: DateTime): String = {
    val formatter = org.joda.time.format.DateTimeFormat.forPattern("HH:mm")
    formatter.print(time)
  }

  def serialize(stopTime: StopTime): CheminotBuf.StopTime = {
    val builder = CheminotBuf.StopTime.newBuilder()

    builder.setTripId(stopTime.tripId)
      .setStopId(stopTime.stopId)
      .setPos(stopTime.pos)

    stopTime.arrival.foreach { arrival =>
      builder.setArrival(formatTime(arrival))
    }

    stopTime.departure.foreach { departure =>
      builder.setDeparture(formatTime(departure))
    }

    builder.build()
  }
}
