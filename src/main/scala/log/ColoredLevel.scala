package org.cheminot.db.log

import ch.qos.logback.classic._
import ch.qos.logback.classic.pattern._
import ch.qos.logback.classic.spi._

class ColoredLevel extends ClassicConverter {

  def convert(event: ILoggingEvent): String = {
    event.getLevel match {
      case Level.TRACE => "[" + Colors.blue("trace") + "]"
      case Level.DEBUG => "[" + Colors.cyan("debug") + "]"
      case Level.INFO => "[" + Colors.white("info") + "]"
      case Level.WARN => "[" + Colors.yellow("warn") + "]"
      case Level.ERROR => "[" + Colors.red("error") + "]"
    }
  }
}
