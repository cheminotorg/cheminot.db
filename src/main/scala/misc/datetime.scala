package m.cheminot.misc

import java.util.Locale
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.joda.time.{DateTime => JDateTime, DateTimeZone}
import org.joda.time.format.{ISODateTimeFormat => JISODateTimeFormat}

object DateTime {

  private val parisZone = DateTimeZone.forID("Europe/Paris")

  private val isoFormatter: DateTimeFormatter =
    defaultSettings(JISODateTimeFormat.dateTime)

  def defaultSettings(f: DateTimeFormatter)=
    f.withZone(parisZone).withLocale(Locale.ENGLISH)

  def parse(s: String): Option[JDateTime] =
    scala.util.Try(isoFormatter.parseDateTime(s)).toOption

  def parseOrFail(s: String): JDateTime =
    parse(s) getOrElse sys.error(s"Unable to parse datetime $s")

  def forPattern(s: String) =
    defaultSettings(DateTimeFormat.forPattern(s))
}
