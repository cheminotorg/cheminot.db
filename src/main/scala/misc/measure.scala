package org.cheminot.db.misc

import org.cheminot.db.log.Logger

object Measure {

  def duration[A](id: String)(f: => A): A = {
    Logger.debug(s"Starting ${id}...")
    val start = System.currentTimeMillis
    val res = f
    val end = System.currentTimeMillis
    val d: Long = (end - start) / 1000
    Logger.debug(s"$id finished in ${d}s")
    res
  }
}
