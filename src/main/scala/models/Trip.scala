package m.cheminot.models

import m.cheminot.TripRecord

case class Trip(id: String, calendar: Option[Calendar], direction: String, stopTimes: Seq[StopTime]) {

  lazy val stops: Seq[String] = {
    stopTimes.map(_.stopId).distinct
  }

  def hasStop(id: String): Boolean = {
    stops.find(_ == id).isDefined
  }

  def edgesOf(stopId: String): Seq[String] = {
    Option(stopTimes.indexWhere(_.stopId == stopId)).filter(_ >= 0).toList.flatMap { index =>
      stopTimes.lift(index - 1).toList ++ stopTimes.lift(index + 1).toList
    }.map(_.stopId)
  }
}

object Trip {

  import m.cheminot.data.CheminotBuf

  def fromRecord(record: TripRecord, routeId: String, calendar: Option[Calendar], stopTimes: Seq[StopTime]): Trip = {
    Trip(record.tripId, calendar, record.directionId, stopTimes)
  }

  def serializeStopIds(trip: Trip): CheminotBuf.TripStopIds = {
    val builder = CheminotBuf.TripStopIds.newBuilder()
    trip.stopTimes.sortBy(_.pos).map { stopTime =>
      builder.addStopIds(stopTime.stopId)
    }
    builder.build()
  }
}
