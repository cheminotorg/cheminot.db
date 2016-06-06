package org.cheminot.db.build

import org.cheminot.db.misc
import org.joda.time.DateTime

package object storage {

  def formatDate(date: DateTime): String =
    (date.withTimeAtStartOfDay().getMillis() / 1000).toString

  def formatDateTime(date: DateTime): String =
    (date.getMillis() / 1000).toString

  def formatTime(time: DateTime): String = {
    val formatter = misc.DateTime.forPattern("HHmm")
    formatter.print(time)
  }
}
