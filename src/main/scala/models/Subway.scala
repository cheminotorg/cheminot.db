package m.cheminot.models

object Subway {

  import Stop._

  lazy val stopTimes = Map(
    STOP_PARIS_MONTP -> List(
      PARISMONT_PARISBERCY_0,
      PARISMONT_PARISNORD_0,
      PARISMONT_PARISLYON_0,
      PARISMONT_PARISEST_0,
      PARISMONT_PARISLAZARE_0,
      PARISMONT_PARISAUSTERLITZ_0,
      PARISBERCY_PARISMONT_1,
      PARISLYON_PARISMONT_1,
      PARISEST_PARISMONT_1,
      PARISNORD_PARISMONT_1,
      PARISLAZARE_PARISMONT_1,
      PARISAUSTERLITZ_PARISMONT_1
    ),
    STOP_PARIS_BERCY -> List(
      PARISBERCY_PARISMONT_0,
      PARISBERCY_PARISNORD_0,
      PARISBERCY_PARISLYON_0,
      PARISBERCY_PARISEST_0,
      PARISBERCY_PARISLAZARE_0,
      PARISBERCY_PARISAUSTERLITZ_0,
      PARISMONT_PARISBERCY_1,
      PARISLYON_PARISBERCY_1,
      PARISEST_PARISBERCY_1,
      PARISNORD_PARISBERCY_1,
      PARISLAZARE_PARISBERCY_1,
      PARISAUSTERLITZ_PARISBERCY_1
    ),
    STOP_PARIS_LYON -> List(
      PARISLYON_PARISMONT_0,
      PARISLYON_PARISNORD_0,
      PARISLYON_PARISBERCY_0,
      PARISLYON_PARISEST_0,
      PARISLYON_PARISLAZARE_0,
      PARISLYON_PARISAUSTERLITZ_0,
      PARISMONT_PARISLYON_1,
      PARISBERCY_PARISLYON_1,
      PARISEST_PARISLYON_1,
      PARISNORD_PARISLYON_1,
      PARISLAZARE_PARISLYON_1,
      PARISAUSTERLITZ_PARISLYON_1
    ),
    STOP_PARIS_EST -> List(
      PARISEST_PARISMONT_0,
      PARISEST_PARISNORD_0,
      PARISEST_PARISBERCY_0,
      PARISEST_PARISLYON_0,
      PARISEST_PARISLAZARE_0,
      PARISEST_PARISAUSTERLITZ_0,
      PARISMONT_PARISEST_1,
      PARISBERCY_PARISEST_1,
      PARISLYON_PARISEST_1,
      PARISNORD_PARISEST_1,
      PARISLAZARE_PARISEST_1,
      PARISAUSTERLITZ_PARISEST_1
    ),
    STOP_PARIS_NORD -> List(
      PARISNORD_PARISMONT_0,
      PARISNORD_PARISEST_0,
      PARISNORD_PARISBERCY_0,
      PARISNORD_PARISLYON_0,
      PARISNORD_PARISLAZARE_0,
      PARISNORD_PARISAUSTERLITZ_0,
      PARISMONT_PARISNORD_1,
      PARISBERCY_PARISNORD_1,
      PARISLYON_PARISNORD_1,
      PARISEST_PARISNORD_1,
      PARISLAZARE_PARISNORD_1,
      PARISAUSTERLITZ_PARISNORD_1
    ),
    STOP_PARIS_LAZARE -> List(
      PARISLAZARE_PARISMONT_0,
      PARISLAZARE_PARISEST_0,
      PARISLAZARE_PARISBERCY_0,
      PARISLAZARE_PARISLYON_0,
      PARISLAZARE_PARISNORD_0,
      PARISLAZARE_PARISAUSTERLITZ_0,
      PARISMONT_PARISLAZARE_1,
      PARISBERCY_PARISLAZARE_1,
      PARISLYON_PARISLAZARE_1,
      PARISEST_PARISLAZARE_1,
      PARISNORD_PARISLAZARE_1,
      PARISLAZARE_PARISAUSTERLITZ_1,
      PARISAUSTERLITZ_PARISLAZARE_1
    ),
    STOP_PARIS_AUSTERLITZ -> List(
      PARISAUSTERLITZ_PARISMONT_0,
      PARISAUSTERLITZ_PARISEST_0,
      PARISAUSTERLITZ_PARISBERCY_0,
      PARISAUSTERLITZ_PARISLYON_0,
      PARISAUSTERLITZ_PARISNORD_0,
      PARISAUSTERLITZ_PARISLAZARE_0,
      PARISMONT_PARISAUSTERLITZ_1,
      PARISBERCY_PARISAUSTERLITZ_1,
      PARISLYON_PARISAUSTERLITZ_1,
      PARISEST_PARISAUSTERLITZ_1,
      PARISNORD_PARISAUSTERLITZ_1,
      PARISLAZARE_PARISAUSTERLITZ_1
    )
  )

  val TRIP_PARISMONTP_PARISBERCY = "SUBWAY:PARISMONTP-PARISBERCY"
  val TRIP_PARISMONTP_PARISNORD = "SUBWAY:PARISMONTP-PARISNORD"
  val TRIP_PARISMONTP_PARISEST = "SUBWAY:PARISMONTP-PARISEST"
  val TRIP_PARISMONTP_PARISLAZARE = "SUBWAY:PARISMONTP-PARISLAZARE"
  val TRIP_PARISMONTP_PARISLYON = "SUBWAY:PARISMONTP-PARISLYON"
  val TRIP_PARISMONTP_PARISAUSTERLITZ = "SUBWAY:PARISMONTP-PARISAUSTERLITZ"

  val TRIP_PARISBERCY_PARISMONTP = "SUBWAY:PARISBERCY-PARISMONTP"
  val TRIP_PARISBERCY_PARISNORD = "SUBWAY:PARISBERCY-PARISNORD"
  val TRIP_PARISBERCY_PARISEST = "SUBWAY:PARISBERCY-PARISEST"
  val TRIP_PARISBERCY_PARISLAZARE = "SUBWAY:PARISBERCY-PARISLAZARE"
  val TRIP_PARISBERCY_PARISLYON = "SUBWAY:PARISBERCY-PARISLYON"
  val TRIP_PARISBERCY_PARISAUSTERLITZ = "SUBWAY:PARISBERCY-PARISAUSTERLITZ"

  val TRIP_PARISLYON_PARISMONTP = "SUBWAY:PARISLYON-PARISMONTP"
  val TRIP_PARISLYON_PARISNORD = "SUBWAY:PARISLYON-PARISNORD"
  val TRIP_PARISLYON_PARISEST = "SUBWAY:PARISLYON-PARISEST"
  val TRIP_PARISLYON_PARISLAZARE = "SUBWAY:PARISLYON-PARISLAZARE"
  val TRIP_PARISLYON_PARISBERCY = "SUBWAY:PARISLYON-PARISBERCY"
  val TRIP_PARISLYON_PARISAUSTERLITZ = "SUBWAY:PARISLYON-PARISAUSTERLITZ"

  val TRIP_PARISEST_PARISMONTP = "SUBWAY:PARISEST-PARISMONTP"
  val TRIP_PARISEST_PARISNORD = "SUBWAY:PARISEST-PARISNORD"
  val TRIP_PARISEST_PARISLYON = "SUBWAY:PARISEST-PARISLYON"
  val TRIP_PARISEST_PARISLAZARE = "SUBWAY:PARISEST-PARISLAZARE"
  val TRIP_PARISEST_PARISBERCY = "SUBWAY:PARISEST-PARISBERCY"
  val TRIP_PARISEST_PARISAUSTERLITZ = "SUBWAY:PARISEST-PARISAUSTERLITZ"

  val TRIP_PARISNORD_PARISMONTP = "SUBWAY:PARISNORD-PARISMONTP"
  val TRIP_PARISNORD_PARISEST = "SUBWAY:PARISNORD-PARISEST"
  val TRIP_PARISNORD_PARISLYON = "SUBWAY:PARISNORD-PARISLYON"
  val TRIP_PARISNORD_PARISLAZARE = "SUBWAY:PARISNORD-PARISLAZARE"
  val TRIP_PARISNORD_PARISBERCY = "SUBWAY:PARISNORD-PARISBERCY"
  val TRIP_PARISNORD_PARISAUSTERLITZ = "SUBWAY:PARISNORD-PARISAUSTERLITZ"

  val TRIP_PARISLAZARE_PARISMONTP = "SUBWAY:PARISLAZARE-PARISMONTP"
  val TRIP_PARISLAZARE_PARISEST = "SUBWAY:PARISLAZARE-PARISEST"
  val TRIP_PARISLAZARE_PARISLYON = "SUBWAY:PARISLAZARE-PARISLYON"
  val TRIP_PARISLAZARE_PARISNORD = "SUBWAY:PARISLAZARE-PARISNORD"
  val TRIP_PARISLAZARE_PARISBERCY = "SUBWAY:PARISLAZARE-PARISBERCY"
  val TRIP_PARISLAZARE_PARISAUSTERLITZ = "SUBWAY:PARISLAZARE-PARISAUSTERLITZ"

  val TRIP_PARISAUSTERLITZ_PARISMONTP = "SUBWAY:PARISAUSTERLITZ-PARISMONTP"
  val TRIP_PARISAUSTERLITZ_PARISEST = "SUBWAY:PARISAUSTERLITZ-PARISEST"
  val TRIP_PARISAUSTERLITZ_PARISLYON = "SUBWAY:PARISAUSTERLITZ-PARISLYON"
  val TRIP_PARISAUSTERLITZ_PARISNORD = "SUBWAY:PARISAUSTERLITZ-PARISNORD"
  val TRIP_PARISAUSTERLITZ_PARISBERCY = "SUBWAY:PARISAUSTERLITZ-PARISBERCY"
  val TRIP_PARISAUSTERLITZ_PARISLAZARE = "SUBWAY:PARISAUSTERLITZ-PARISLAZARE"

  val PARISMONT_PARISBERCY_0 = StopTime(TRIP_PARISMONTP_PARISBERCY, None, None, STOP_PARIS_MONTP, 0)
  val PARISMONT_PARISBERCY_1 = StopTime(TRIP_PARISMONTP_PARISBERCY, None, None, STOP_PARIS_BERCY, 1)
  val PARISMONT_PARISNORD_0 = StopTime(TRIP_PARISMONTP_PARISNORD, None, None, STOP_PARIS_MONTP, 0)
  val PARISMONT_PARISNORD_1 = StopTime(TRIP_PARISMONTP_PARISNORD, None, None, STOP_PARIS_NORD, 1)
  val PARISMONT_PARISLYON_0 = StopTime(TRIP_PARISMONTP_PARISLYON, None, None, STOP_PARIS_MONTP, 0)
  val PARISMONT_PARISLYON_1 = StopTime(TRIP_PARISMONTP_PARISLYON, None, None, STOP_PARIS_LYON, 1)
  val PARISMONT_PARISEST_0 = StopTime(TRIP_PARISMONTP_PARISEST, None, None, STOP_PARIS_MONTP, 0)
  val PARISMONT_PARISEST_1 = StopTime(TRIP_PARISMONTP_PARISEST, None, None, STOP_PARIS_EST, 1)
  val PARISMONT_PARISLAZARE_0 = StopTime(TRIP_PARISMONTP_PARISLAZARE, None, None, STOP_PARIS_MONTP, 0)
  val PARISMONT_PARISLAZARE_1 = StopTime(TRIP_PARISMONTP_PARISLAZARE, None, None, STOP_PARIS_LAZARE, 1)
  val PARISMONT_PARISAUSTERLITZ_0 = StopTime(TRIP_PARISMONTP_PARISAUSTERLITZ, None, None, STOP_PARIS_MONTP, 0)
  val PARISMONT_PARISAUSTERLITZ_1 = StopTime(TRIP_PARISMONTP_PARISAUSTERLITZ, None, None, STOP_PARIS_AUSTERLITZ, 1)

  val PARISBERCY_PARISMONT_0 = StopTime(TRIP_PARISBERCY_PARISMONTP, None, None, STOP_PARIS_BERCY, 0)
  val PARISBERCY_PARISMONT_1 = StopTime(TRIP_PARISBERCY_PARISMONTP, None, None, STOP_PARIS_MONTP, 1)
  val PARISBERCY_PARISNORD_0 = StopTime(TRIP_PARISBERCY_PARISNORD, None, None, STOP_PARIS_BERCY, 0)
  val PARISBERCY_PARISNORD_1 = StopTime(TRIP_PARISBERCY_PARISNORD, None, None, STOP_PARIS_NORD, 1)
  val PARISBERCY_PARISLYON_0 = StopTime(TRIP_PARISBERCY_PARISLYON, None, None, STOP_PARIS_BERCY, 0)
  val PARISBERCY_PARISLYON_1 = StopTime(TRIP_PARISBERCY_PARISLYON, None, None, STOP_PARIS_LYON, 1)
  val PARISBERCY_PARISEST_0 = StopTime(TRIP_PARISBERCY_PARISEST, None, None, STOP_PARIS_BERCY, 0)
  val PARISBERCY_PARISEST_1 = StopTime(TRIP_PARISBERCY_PARISEST, None, None, STOP_PARIS_EST, 1)
  val PARISBERCY_PARISLAZARE_0 = StopTime(TRIP_PARISBERCY_PARISLAZARE, None, None, STOP_PARIS_BERCY, 0)
  val PARISBERCY_PARISLAZARE_1 = StopTime(TRIP_PARISBERCY_PARISLAZARE, None, None, STOP_PARIS_LAZARE, 1)
  val PARISBERCY_PARISAUSTERLITZ_0 = StopTime(TRIP_PARISBERCY_PARISAUSTERLITZ, None, None, STOP_PARIS_BERCY, 0)
  val PARISBERCY_PARISAUSTERLITZ_1 = StopTime(TRIP_PARISBERCY_PARISAUSTERLITZ, None, None, STOP_PARIS_AUSTERLITZ, 1)

  val PARISLYON_PARISMONT_0 = StopTime(TRIP_PARISLYON_PARISMONTP, None, None, STOP_PARIS_LYON, 0)
  val PARISLYON_PARISMONT_1 = StopTime(TRIP_PARISLYON_PARISMONTP, None, None, STOP_PARIS_MONTP, 1)
  val PARISLYON_PARISNORD_0 = StopTime(TRIP_PARISLYON_PARISNORD, None, None, STOP_PARIS_LYON, 0)
  val PARISLYON_PARISNORD_1 = StopTime(TRIP_PARISLYON_PARISNORD, None, None, STOP_PARIS_NORD, 1)
  val PARISLYON_PARISBERCY_0 = StopTime(TRIP_PARISLYON_PARISBERCY, None, None, STOP_PARIS_LYON, 0)
  val PARISLYON_PARISBERCY_1 = StopTime(TRIP_PARISLYON_PARISBERCY, None, None, STOP_PARIS_BERCY, 1)
  val PARISLYON_PARISEST_0 = StopTime(TRIP_PARISLYON_PARISEST, None, None, STOP_PARIS_LYON, 0)
  val PARISLYON_PARISEST_1 = StopTime(TRIP_PARISLYON_PARISEST, None, None, STOP_PARIS_EST, 1)
  val PARISLYON_PARISLAZARE_0 = StopTime(TRIP_PARISLYON_PARISLAZARE, None, None, STOP_PARIS_LYON, 0)
  val PARISLYON_PARISLAZARE_1 = StopTime(TRIP_PARISLYON_PARISLAZARE, None, None, STOP_PARIS_LAZARE, 1)
  val PARISLYON_PARISAUSTERLITZ_0 = StopTime(TRIP_PARISLYON_PARISAUSTERLITZ, None, None, STOP_PARIS_LYON, 0)
  val PARISLYON_PARISAUSTERLITZ_1 = StopTime(TRIP_PARISLYON_PARISAUSTERLITZ, None, None, STOP_PARIS_AUSTERLITZ, 1)

  val PARISEST_PARISMONT_0 = StopTime(TRIP_PARISEST_PARISMONTP, None, None, STOP_PARIS_EST, 0)
  val PARISEST_PARISMONT_1 = StopTime(TRIP_PARISEST_PARISMONTP, None, None, STOP_PARIS_MONTP, 1)
  val PARISEST_PARISNORD_0 = StopTime(TRIP_PARISEST_PARISNORD, None, None, STOP_PARIS_EST, 0)
  val PARISEST_PARISNORD_1 = StopTime(TRIP_PARISEST_PARISNORD, None, None, STOP_PARIS_NORD, 1)
  val PARISEST_PARISBERCY_0 = StopTime(TRIP_PARISEST_PARISBERCY, None, None, STOP_PARIS_EST, 0)
  val PARISEST_PARISBERCY_1 = StopTime(TRIP_PARISEST_PARISBERCY, None, None, STOP_PARIS_BERCY, 1)
  val PARISEST_PARISLYON_0 = StopTime(TRIP_PARISEST_PARISLYON, None, None, STOP_PARIS_EST, 0)
  val PARISEST_PARISLYON_1 = StopTime(TRIP_PARISEST_PARISLYON, None, None, STOP_PARIS_LYON, 1)
  val PARISEST_PARISLAZARE_0 = StopTime(TRIP_PARISEST_PARISLAZARE, None, None, STOP_PARIS_EST, 0)
  val PARISEST_PARISLAZARE_1 = StopTime(TRIP_PARISEST_PARISLAZARE, None, None, STOP_PARIS_LAZARE, 1)
  val PARISEST_PARISAUSTERLITZ_0 = StopTime(TRIP_PARISEST_PARISAUSTERLITZ, None, None, STOP_PARIS_EST, 0)
  val PARISEST_PARISAUSTERLITZ_1 = StopTime(TRIP_PARISEST_PARISAUSTERLITZ, None, None, STOP_PARIS_AUSTERLITZ, 1)

  val PARISNORD_PARISMONT_0 = StopTime(TRIP_PARISNORD_PARISMONTP, None, None, STOP_PARIS_NORD, 0)
  val PARISNORD_PARISMONT_1 = StopTime(TRIP_PARISNORD_PARISMONTP, None, None, STOP_PARIS_MONTP, 1)
  val PARISNORD_PARISEST_0 = StopTime(TRIP_PARISNORD_PARISEST, None, None, STOP_PARIS_NORD, 0)
  val PARISNORD_PARISEST_1 = StopTime(TRIP_PARISNORD_PARISEST, None, None, STOP_PARIS_EST, 1)
  val PARISNORD_PARISBERCY_0 = StopTime(TRIP_PARISNORD_PARISBERCY, None, None, STOP_PARIS_NORD, 0)
  val PARISNORD_PARISBERCY_1 = StopTime(TRIP_PARISNORD_PARISBERCY, None, None, STOP_PARIS_BERCY, 1)
  val PARISNORD_PARISLYON_0 = StopTime(TRIP_PARISNORD_PARISLYON, None, None, STOP_PARIS_NORD, 0)
  val PARISNORD_PARISLYON_1 = StopTime(TRIP_PARISNORD_PARISLYON, None, None, STOP_PARIS_LYON, 1)
  val PARISNORD_PARISLAZARE_0 = StopTime(TRIP_PARISNORD_PARISLAZARE, None, None, STOP_PARIS_NORD, 0)
  val PARISNORD_PARISLAZARE_1 = StopTime(TRIP_PARISNORD_PARISLAZARE, None, None, STOP_PARIS_LAZARE, 1)
  val PARISNORD_PARISAUSTERLITZ_0 = StopTime(TRIP_PARISNORD_PARISAUSTERLITZ, None, None, STOP_PARIS_NORD, 0)
  val PARISNORD_PARISAUSTERLITZ_1 = StopTime(TRIP_PARISNORD_PARISAUSTERLITZ, None, None, STOP_PARIS_AUSTERLITZ, 1)

  val PARISLAZARE_PARISMONT_0 = StopTime(TRIP_PARISLAZARE_PARISMONTP, None, None, STOP_PARIS_LAZARE, 0)
  val PARISLAZARE_PARISMONT_1 = StopTime(TRIP_PARISLAZARE_PARISMONTP, None, None, STOP_PARIS_MONTP, 1)
  val PARISLAZARE_PARISEST_0 = StopTime(TRIP_PARISLAZARE_PARISEST, None, None, STOP_PARIS_LAZARE, 0)
  val PARISLAZARE_PARISEST_1 = StopTime(TRIP_PARISLAZARE_PARISEST, None, None, STOP_PARIS_EST, 1)
  val PARISLAZARE_PARISBERCY_0 = StopTime(TRIP_PARISLAZARE_PARISBERCY, None, None, STOP_PARIS_LAZARE, 0)
  val PARISLAZARE_PARISBERCY_1 = StopTime(TRIP_PARISLAZARE_PARISBERCY, None, None, STOP_PARIS_BERCY, 1)
  val PARISLAZARE_PARISLYON_0 = StopTime(TRIP_PARISLAZARE_PARISLYON, None, None, STOP_PARIS_LAZARE, 0)
  val PARISLAZARE_PARISLYON_1 = StopTime(TRIP_PARISLAZARE_PARISLYON, None, None, STOP_PARIS_LYON, 1)
  val PARISLAZARE_PARISNORD_0 = StopTime(TRIP_PARISLAZARE_PARISNORD, None, None, STOP_PARIS_LAZARE, 0)
  val PARISLAZARE_PARISNORD_1 = StopTime(TRIP_PARISLAZARE_PARISNORD, None, None, STOP_PARIS_NORD, 1)
  val PARISLAZARE_PARISAUSTERLITZ_0 = StopTime(TRIP_PARISLAZARE_PARISAUSTERLITZ, None, None, STOP_PARIS_LAZARE, 0)
  val PARISLAZARE_PARISAUSTERLITZ_1 = StopTime(TRIP_PARISLAZARE_PARISAUSTERLITZ, None, None, STOP_PARIS_AUSTERLITZ, 1)

  val PARISAUSTERLITZ_PARISMONT_0 = StopTime(TRIP_PARISAUSTERLITZ_PARISMONTP, None, None, STOP_PARIS_AUSTERLITZ, 0)
  val PARISAUSTERLITZ_PARISMONT_1 = StopTime(TRIP_PARISAUSTERLITZ_PARISMONTP, None, None, STOP_PARIS_MONTP, 1)
  val PARISAUSTERLITZ_PARISEST_0 = StopTime(TRIP_PARISAUSTERLITZ_PARISEST, None, None, STOP_PARIS_AUSTERLITZ, 0)
  val PARISAUSTERLITZ_PARISEST_1 = StopTime(TRIP_PARISAUSTERLITZ_PARISEST, None, None, STOP_PARIS_EST, 1)
  val PARISAUSTERLITZ_PARISBERCY_0 = StopTime(TRIP_PARISAUSTERLITZ_PARISBERCY, None, None, STOP_PARIS_AUSTERLITZ, 0)
  val PARISAUSTERLITZ_PARISBERCY_1 = StopTime(TRIP_PARISAUSTERLITZ_PARISBERCY, None, None, STOP_PARIS_BERCY, 1)
  val PARISAUSTERLITZ_PARISLYON_0 = StopTime(TRIP_PARISAUSTERLITZ_PARISLYON, None, None, STOP_PARIS_AUSTERLITZ, 0)
  val PARISAUSTERLITZ_PARISLYON_1 = StopTime(TRIP_PARISAUSTERLITZ_PARISLYON, None, None, STOP_PARIS_LYON, 1)
  val PARISAUSTERLITZ_PARISNORD_0 = StopTime(TRIP_PARISAUSTERLITZ_PARISNORD, None, None, STOP_PARIS_AUSTERLITZ, 0)
  val PARISAUSTERLITZ_PARISNORD_1 = StopTime(TRIP_PARISAUSTERLITZ_PARISNORD, None, None, STOP_PARIS_NORD, 1)
  val PARISAUSTERLITZ_PARISLAZARE_0 = StopTime(TRIP_PARISAUSTERLITZ_PARISLAZARE, None, None, STOP_PARIS_AUSTERLITZ, 0)
  val PARISAUSTERLITZ_PARISLAZARE_1 = StopTime(TRIP_PARISAUSTERLITZ_PARISLAZARE, None, None, STOP_PARIS_LAZARE, 1)
}