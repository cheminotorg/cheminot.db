package m.cheminot.models

import org.joda.time.DateTime
import m.cheminot.StopRecord

case class Stop(id: String, name: String, lat: Double, lng: Double)

object Stop {

  val STOP_PARIS_MONTP_VAUG = "8739110"
  val STOP_PARIS_MONTP = "8739100"
  val STOP_PARIS_BERCY = "8768666"
  val STOP_PARIS_LYON = "8768600"
  val STOP_PARIS_EST = "8711300"
  val STOP_PARIS_NORD = "8727100"
  val STOP_PARIS_LAZARE = "8738400"
  val STOP_PARIS_AUSTERLITZ = "8754700"
  val STOP_PARIS = "PARISXX"

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

  def isParis(stopId: String) =
    parisStops.exists(_ == stopId)

  def fromRecord(record: StopRecord): Stop = {
    Stop(
      record.stopId,
      record.stopName,
      record.stopLat,
      record.stopLong
    )
  }
}
