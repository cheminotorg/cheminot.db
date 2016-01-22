package m.cheminot.build

import org.joda.time.DateTime

case class StopTimeRecord(
  tripId: String,
  arrival: DateTime,
  departure: DateTime,
  stopId: String,
  stopSeq: Int,
  stopHeadSign: String,
  pickUpType: String,
  dropOffType: String
)

case class TripRecord(
  routeId: String,
  serviceId: String,
  tripId: String,
  tripHeadSign: String,
  directionId: String,
  blockId: String
)

case class StopRecord(
  stopId: String,
  stopName: String,
  stopDesc: String,
  stopLat: Double,
  stopLong: Double,
  zone: String,
  stopUrl: String,
  locationType: String,
  parentStation: String
) {

  override def equals(o: Any): Boolean =
    o match {
      case r:StopRecord => r.stopId == stopId
      case _ => false
    }

  override def hashCode = stopId.hashCode
}

case class CalendarRecord(
  serviceId: String,
  monday: Boolean,
  tuesday: Boolean,
  wednesday: Boolean,
  thursday: Boolean,
  friday: Boolean,
  saturday: Boolean,
  sunday: Boolean,
  startDate: DateTime,
  endDate: DateTime
)

case class CalendarDateRecord(
  serviceId: String,
  date: DateTime,
  exceptionType: Int
)
