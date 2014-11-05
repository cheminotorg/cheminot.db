package m.cheminot.models

trait FormatReader {
  import org.joda.time.DateTime
  import org.joda.time.format.DateTimeFormat

  val TimeR = """(\d{2}):(\d{2}):(\d{2})""".r

  def asDateTime(str: String): DateTime = {
    val formatter = DateTimeFormat.forPattern("yyyyMMdd").withZoneUTC
    DateTime.parse(str, formatter)
  }

  def asTime(str: String): DateTime = {
    str match {
      case TimeR(hours, minutes, seconds) =>
        val now = DateTime.now
        val h = hours.toInt
        if(hours.toInt > 23) {
          now.plusDays(1).withTime(h - 24, minutes.toInt, seconds.toInt, 0)
        } else {
          now.withTime(h, minutes.toInt, seconds.toInt, 0)
        }
    }
  }
}
