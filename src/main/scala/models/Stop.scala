package m.cheminot.models

import play.api.libs.json._

case class Stop(id: String, name: String, lat: Double, lng: Double)

object Stop {

  val STOP_PARIS_MONTP_VAUG = "StopArea:OCE87391102"
  val STOP_PARIS_MONTP = "StopArea:OCE87391003"
  val STOP_PARIS_BERCY = "StopArea:OCE87686667"
  val STOP_PARIS_LYON = "StopArea:OCE87686006"
  val STOP_PARIS_EST = "StopArea:OCE87113001"
  val STOP_PARIS_NORD = "StopArea:OCE87271007"
  val STOP_PARIS_LAZARE = "StopArea:OCE87384008"
  val STOP_PARIS_AUSTERLITZ = "StopArea:OCE87547000"

  val STOPS_PARIS = Seq(STOP_PARIS_MONTP_VAUG, STOP_PARIS_MONTP, STOP_PARIS_BERCY, STOP_PARIS_LYON, STOP_PARIS_EST, STOP_PARIS_NORD, STOP_PARIS_LAZARE, STOP_PARIS_AUSTERLITZ)

  def fromRow(data: List[String]): Stop = {
    Stop(
      data(0),
      data(1),
      data(3).toDouble,
      data(4).toDouble
    )
  }

  implicit val reader: Reads[Stop] = Json.reads[Stop]
  implicit val writer: Writes[Stop] = Json.writes[Stop]
}
