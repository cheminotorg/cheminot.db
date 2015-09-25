package m.cheminot.models

import org.joda.time.DateTime
import m.cheminot.StopRecord

case class Stop(id: String, name: String, lat: Double, lng: Double)

object Stop {

  val STOP_PARIS_MONTP_VAUG = "StopPoint:OCETrain TER-87391102"
  val STOP_PARIS_MONTP = "StopPoint:OCETrain TER-87391003"
  val STOP_PARIS_BERCY = "StopPoint:OCETrain TER-87686667"
  val STOP_PARIS_LYON = "StopPoint:OCETrain TER-87686006"
  val STOP_PARIS_EST = "StopPoint:OCETrain TER-87113001"
  val STOP_PARIS_NORD = "StopPoint:OCETrain TER-87271007"
  val STOP_PARIS_LAZARE = "StopPoint:OCETrain TER-87384008"
  val STOP_PARIS_AUSTERLITZ = "StopPoint:OCETrain TER-87547000"
  val STOP_PARIS = "StopPoint:OCETrain TER-PARISXXX"

  val parisStops = List(
    STOP_PARIS_MONTP_VAUG,
    STOP_PARIS_MONTP,
    STOP_PARIS_BERCY,
    STOP_PARIS_LYON,
    STOP_PARIS_EST,
    STOP_PARIS_NORD,
    STOP_PARIS_LAZARE,
    STOP_PARIS_AUSTERLITZ
  )

  def fromRecord(record: StopRecord): Stop = {
    Stop(
      record.stopId,
      record.stopName,
      record.stopLat,
      record.stopLong
    )
  }
}
