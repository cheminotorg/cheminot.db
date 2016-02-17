package m.cheminot.build

case class Trip(id: String, serviceId: String, calendar: Option[Calendar], stopTimes: Seq[StopTime]) {

  lazy val stops: Seq[String] = {
    stopTimes.map(_.stopId).distinct
  }

  def isValid: Boolean =
    stopTimes.forall(stopTime => stopTimes.count(_.stopId == stopTime.stopId) == 1)

  def hasStop(id: String): Boolean = {
    stops.find(_ == id).isDefined
  }

  def edgesOf(stopId: String): Seq[String] = {
    Option(stopTimes.indexWhere(_.stopId == stopId)).filter(_ >= 0).toList.flatMap { index =>
      stopTimes.lift(index - 1).toList ++ stopTimes.lift(index + 1).toList
    }.map(_.stopId)
  }

  def contains(t: Trip): Boolean =
    stopTimes.containsSlice(t.stopTimes)

  override def equals(o: Any): Boolean =
    o match {
      case r: Trip if r.id == id => true
      case r: Trip =>
        (for {
          firstStopTime <- stopTimes.headOption
          otherFirstStopTime <- r.stopTimes.headOption
          if firstStopTime == otherFirstStopTime

          lastStopTime <- stopTimes.lastOption
          otherLastStopTime <- r.stopTimes.lastOption
          if lastStopTime == otherLastStopTime
        } yield true).isDefined
      case _ => false
    }

  override def hashCode =
    (for {
      firstStopTime <- stopTimes.headOption
      lastStopTime <- stopTimes.lastOption
      if firstStopTime != lastStopTime
    } yield {
      List(firstStopTime, lastStopTime, calendar.map(_.serviceId)).map(_.hashCode).mkString("").hashCode
    }) getOrElse id.hashCode
}

object Trip {

  def fromRecord(record: TripRecord, routeId: String, calendar: Option[Calendar], stopTimes: Seq[StopTime]): Trip = {
    Trip(record.tripId, record.serviceId, calendar, stopTimes)
  }
}
