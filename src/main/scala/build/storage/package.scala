package org.cheminot.db

import org.joda.time.DateTime

package object storage {

  def formatDate(date: DateTime): String =
    (date.withTimeAtStartOfDay().getMillis() / 1000).toString

  def formatDateTime(date: DateTime): String =
    (date.getMillis() / 1000).toString

  def formatTime(time: DateTime): String =
    org.cheminot.misc.DateTime.minutesOfDay(time).toString
}
