package m.cheminot.models

import play.api.libs.json._

case class Route(
  id: String,
  shortName: String,
  longName: String,
  desc: String
)

object Route {
  def fromRow(data: List[String]): Route = {
    Route(
      data(0),
      data(2),
      data(3),
      data(4)
    )
  }

  implicit val reader: Reads[Route] = Json.reads[Route]
  implicit val writer: Writes[Route] = Json.writes[Route]
}
