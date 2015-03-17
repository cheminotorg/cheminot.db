package m.cheminot.models

case class Stop(id: String, name: String, lat: Double, lng: Double)

object Stop {

  private val STOP_PARIS_MONTP_VAUG = "StopPoint:OCETrain TER-87391102"
  private val STOP_PARIS_MONTP = "StopPoint:OCETrain TER-87391003"
  private val STOP_PARIS_BERCY = "StopPoint:OCETrain TER-87686667"
  private val STOP_PARIS_LYON = "StopPoint:OCETrain TER-87686006"
  private val STOP_PARIS_EST = "StopPoint:OCETrain TER-87113001"
  private val STOP_PARIS_NORD = "StopPoint:OCETrain TER-87271007"
  private val STOP_PARIS_LAZARE = "StopPoint:OCETrain TER-87384008"
  private val STOP_PARIS_AUSTERLITZ = "StopPoint:OCETrain TER-87547000"

  val STOP_PARIS = "StopPoint:OCETrain TER-PARISXXX"

  val parisStops = Seq(
    STOP_PARIS,
    STOP_PARIS_MONTP_VAUG,
    STOP_PARIS_MONTP,
    STOP_PARIS_BERCY,
    STOP_PARIS_LYON,
    STOP_PARIS_EST,
    STOP_PARIS_NORD,
    STOP_PARIS_LAZARE,
    STOP_PARIS_AUSTERLITZ
  )

  def fromRow(data: List[String]): Stop = {
    Stop(
      data(0),
      data(1),
      data(3).toDouble,
      data(4).toDouble
    )
  }
}
