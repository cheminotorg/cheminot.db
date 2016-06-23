package org.cheminot.db

import org.joda.time.DateTime

case class StopTime(
  tripId: String,
  arrival: DateTime,
  departure: DateTime,
  stopId: String,
  pos: Int
) {
  lazy val id = StopTime.id(tripId, stopId)

  override def equals(o: Any): Boolean = {
    o match {
      case s: StopTime =>
        stopId == s.stopId && arrival == s.arrival && departure == s.departure
      case _ => false
    }
  }

  override def hashCode =
    List(stopId, arrival, departure).mkString("#").hashCode
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
}
