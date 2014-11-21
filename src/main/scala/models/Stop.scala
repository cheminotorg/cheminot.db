package m.cheminot.models

case class Stop(id: String, name: String, lat: Double, lng: Double)

object Stop {

  private val STOP_PARIS_MONTP_VAUG = "StopArea:OCE87391102"
  private val STOP_PARIS_MONTP = "StopArea:OCE87391003"
  private val STOP_PARIS_BERCY = "StopArea:OCE87686667"
  private val STOP_PARIS_LYON = "StopArea:OCE87686006"
  private val STOP_PARIS_EST = "StopArea:OCE87113001"
  private val STOP_PARIS_NORD = "StopArea:OCE87271007"
  private val STOP_PARIS_LAZARE = "StopArea:OCE87384008"
  private val STOP_PARIS_AUSTERLITZ = "StopArea:OCE87547000"

  val parisStops = Seq(
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
