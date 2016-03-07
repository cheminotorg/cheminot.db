package m.cheminot.misc

import rapture.fs._
import rapture.uri._

object File {

  def currentDir: FsUrl = { //TODO
    val dir = new java.io.File(".")
    val path = "file:/" / dir.getAbsolutePath
    rapture.fs.File.parse(path.toString).parent
  }
}
