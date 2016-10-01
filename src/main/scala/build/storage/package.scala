package org.cheminot.db

import org.joda.time.DateTime

package object storage {

  def formatDate(datetime: DateTime): String =
    (datetime.withTimeAtStartOfDay().getMillis() / 1000).toString

  def formatDateTime(datetime: DateTime): String =
    (datetime.getMillis() / 1000).toString

  def formatTime(datetime: DateTime): String =
    datetime.getMinuteOfDay.toString
}
