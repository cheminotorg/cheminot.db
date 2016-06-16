package org.cheminot.db.build.storage

import rapture.cli._

object Docker {

  def run(cmd: String): String =
    Process(Vector(
      "/bin/sh"
    )).exec[String]
}
