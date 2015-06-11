package m.cheminot.misc

import java.io._
import java.util.zip._

object ZipUtils {

  def unzip(zipfile: File, directory: File) {
    println(s"Unzipping $zipfile to $directory")
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
    println("Unzipping done")
  }


  private def copy(in: InputStream, out: OutputStream) {
    val buffer = new Array[Byte](1024)
    @annotation.tailrec
    def loop(readCount: Int) {
      if(readCount >= 0) {
        out.write(buffer, 0, readCount)
        loop(in.read(buffer))
      }
    }
    loop(in.read(buffer))
  }

  private def copy(in: InputStream, file: File) {
    val out = new FileOutputStream(file)
    try {
      copy(in, out)
    } finally {
      out.close()
    }
  }
}
