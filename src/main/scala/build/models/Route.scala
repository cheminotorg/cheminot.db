package m.cheminot.build

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
}
