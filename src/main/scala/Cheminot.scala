package m.cheminot

object Cheminot {

  def oops(message: String) = {
    throw new RuntimeException(message)
  }

  val gtfsDirectory = "gtfs"

  val ephemeralDirectory = "/tmp"

  val dbDirectory = "db"
}
