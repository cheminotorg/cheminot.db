package m.cheminot.models

import org.joda.time.DateTime
import play.api.libs.json._
import m.cheminot.Gtfs

case class StopTime(
  tripId: String,
  arrival: DateTime,
  departure: DateTime,
  stopId: String,
  pos: Int
)

object StopTime {

  def fromRow(data: List[String], stopId: String): StopTime = {
    StopTime(
      data(0),
      Gtfs.parseTime(data(1)),
      Gtfs.parseTime(data(2)),
      stopId,
      data(4).toInt
    )
  }

  implicit val reader = Json.reads[StopTime]
  implicit val readerSeq: Reads[Seq[StopTime]] = Reads.seq(reader)
  implicit val dateTimeWriter = play.api.libs.json.Writes.jodaDateWrites("HH:mm")
  implicit val writer = Json.writes[StopTime]
}
