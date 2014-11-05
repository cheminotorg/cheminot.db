package m.cheminot.models

import org.joda.time.DateTime
import play.api.libs.json._

case class StopTime(
  tripId: String,
  arrival: DateTime,
  departure: DateTime,
  stopId: String,
  pos: Int
)

object StopTime extends FormatReader {

  def fromRow(data: List[String], stopId: String): StopTime = {
    StopTime(
      data(0),
      asTime(data(1)),
      asTime(data(2)),
      stopId,
      data(4).toInt
    )
  }

  implicit val reader = Json.reads[StopTime]
  implicit val readerSeq: Reads[Seq[StopTime]] = Reads.seq(reader)
  implicit val dateTimeWriter = play.api.libs.json.Writes.jodaDateWrites("HH:mm")
  implicit val writer = Json.writes[StopTime]
}
