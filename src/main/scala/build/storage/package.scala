package m.cheminot.build

import org.joda.time.DateTime

package object storage {

  def formatDate(date: DateTime): String =
    (date.withTimeAtStartOfDay().getMillis() / 1000).toString

  def formatDateTime(date: org.joda.time.DateTime): String =
    (date.getMillis() / 1000).toString

  def formatTime(time: org.joda.time.DateTime): String = {
    val formatter = org.joda.time.format.DateTimeFormat.forPattern("HHmm")
    formatter.print(time)
  }
}
