package org.cheminot.db.misc

import java.io._
import java.util.zip._
import org.cheminot.db.log.Logger

object ZipUtils {

  def unzip(zipfile: File, directory: File): Unit = {
    Logger.info(s"Unzipping $zipfile to $directory")
    val zfile = new ZipFile(zipfile)
    val entries = zfile.entries()
    while (entries.hasMoreElements()) {
      val entry = entries.nextElement()
      val file = new File(directory, entry.getName())
      if (entry.isDirectory()) {
        file.mkdirs()
      } else {
        file.getParentFile().mkdirs()
        val in = zfile.getInputStream(entry)
        try {
          copy(in, file)
        } finally {
          in.close()
        }
      }
    }
    Logger.info("Unzipping done")
  }


  private def copy(in: InputStream, out: OutputStream): Unit = {
    val buffer = new Array[Byte](1024)
    @annotation.tailrec
    def loop(readCount: Int): Unit = {
      if(readCount >= 0) {
        out.write(buffer, 0, readCount)
        loop(in.read(buffer))
      }
    }
    loop(in.read(buffer))
  }

  private def copy(in: InputStream, file: File): Unit = {
    val out = new FileOutputStream(file)
    try {
      copy(in, out)
    } finally {
      out.close()
    }
  }
}
