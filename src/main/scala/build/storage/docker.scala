package org.cheminot.db.storage

import rapture.cli._

object Docker {

  def upgradedb(): String =
    Process(Vector(
      "/bin/sh",
      "scripts/upgradedb"
    )).exec[String]
}
