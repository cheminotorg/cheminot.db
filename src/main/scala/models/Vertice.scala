package m.cheminot.models

import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Vertice(id: String, name: String, edges: Seq[String], stopTimes: Seq[StopTime])

object Vertice {

  implicit val reader: Reads[Vertice] = (
    (__ \ "id").read[String] and
    (__ \ "name").read[String] and
    (__ \ "edges").read[Seq[String]] and
    (__ \ "stopTimes").read[Seq[StopTime]]
  )(Vertice.apply _)

  implicit val writer: Writes[Vertice] = (
    (__ \ "id").write[String] and
    (__ \ "name").write[String] and
    (__ \ "edges").write[Seq[String]] and
    (__ \ "stopTimes").write[Seq[StopTime]]
  )(unlift(Vertice.unapply))
}
