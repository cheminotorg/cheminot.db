package m.cheminot.models

import org.joda.time.DateTime
import m.cheminot.Gtfs

case class StopTime(
  tripId: String,
  arrival: DateTime,
  departure: DateTime,
  stopId: String,
  pos: Int
)

object StopTime {

  import m.cheminot.data.CheminotBuf

  def fromRow(data: List[String], stopId: String): StopTime = {
    StopTime(
      data(0),
      Gtfs.parseTime(data(1)),
      Gtfs.parseTime(data(2)),
      stopId,
      data(4).toInt
    )
  }

  def formatTime(time: DateTime): String = {
    val formatter = org.joda.time.format.DateTimeFormat.forPattern("HH:mm")
    formatter.print(time)
  }

  def serialize(stopTime: StopTime): CheminotBuf.StopTime = {
    val builder = CheminotBuf.StopTime.newBuilder()
    builder.setTripId(stopTime.tripId)
      .setArrival(formatTime(stopTime.arrival))
      .setDeparture(formatTime(stopTime.departure))
      .setStopId(stopTime.stopId)
      .setPos(stopTime.pos)
    builder.build()
  }
}
