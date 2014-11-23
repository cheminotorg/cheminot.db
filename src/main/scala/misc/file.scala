package m.cheminot.misc

import java.io._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object FileTools {

  def read(file: File): String = {
    val fis = new FileInputStream(file)
    val data = new Array[Byte](file.length().toInt)
    fis.read(data)
    fis.close()
    new String(data, "UTF-8")
  }

  def write(content: String, file: File): File = {
    val output = new java.io.FileOutputStream(file)
    output.write(content.getBytes("UTF-8"))
    file
  }
}
