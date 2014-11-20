package m.cheminot.models

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Trip(
  id: String,
  service: Option[Calendar],
  direction: String,
  stopTimes: Seq[StopTime]
) {
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

  def fromRow(data: List[String], routeId: String, service: Option[Calendar], stopTimes: Seq[StopTime]): Trip = {
    Trip(data(2), service, data(4), stopTimes)
  }

  def splitTrips(trips: Seq[Trip]): Seq[JsValue] = {
    val max = trips.size / 10

    def asGroup(ids: Seq[String], range: JsObject): JsObject = {
      Json.obj(
        "ids" -> ids,
        "trips" -> range
      )
    }
    @annotation.tailrec
    def step(trips: Seq[Trip], group: JsObject, ids: Seq[String], acc: Seq[JsObject], count: Int): Seq[JsObject] = {
      trips match {
        case h :: t =>
          if(count > 0) {
            val updated = Json.obj(h.id -> h) ++ group
            step(t, updated, h.id +: ids, acc, count - 1)
          } else {
            step(t, Json.obj(), Seq.empty, asGroup(ids, group) +: acc, max)
          }
        case Nil =>
          asGroup(ids, group) +: acc
      }
    }
    step(trips, Json.obj(), Seq.empty[String], Seq.empty[JsObject], max)
  }

  implicit val reader: Reads[Trip] =
    (
      (__ \ "id").read[String] and
      (__ \ "service").readNullable[Calendar] and
      (__ \ "direction").read[String] and
      (__ \ "stopTimes").read[Seq[StopTime]]
    )(Trip.apply _)

  implicit val readerSeq = Reads.seq(reader)

  implicit val writer: Writes[Trip] =
    (
      (__ \ "id").write[String] and
      (__ \ "service").writeNullable[Calendar] and
      (__ \ "direction").write[String] and
      (__ \ "stopTimes").write[Seq[StopTime]]
    )(unlift(Trip.unapply))

  implicit val writerSeq: Writes[Seq[Trip]] = new Writes[Seq[Trip]] {
    def writes(trips: Seq[Trip]): JsValue = {
      val sortedTrips = trips.sortBy(_.id)
      JsArray(splitTrips(trips))
    }
  }
}
