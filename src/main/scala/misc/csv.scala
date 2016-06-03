package m.cheminot.misc

import java.io.{ File => JFile }
import com.github.tototoshi.csv._

object CSVReadFile {
  type CollectFunct[A] = Seq[String] => A
  case class Quiet(msg: String) extends Exception(msg)
  case class Verbose(msg: String) extends Exception(msg)
}

case class CSVReadFile(file: JFile) {

  lazy val reader = CSVReader.open(file)

  def read[A](collect: CSVReadFile.CollectFunct[A]): List[A] = {
    val records = scala.collection.mutable.ListBuffer.empty[A]
    reader.foreach { row =>
      scala.util.Try(collect(row)).foreach { record =>
        records += record
      }
    }
    records.toList
  }
}

case class CSVWriteFile(file: JFile) {

  val writer = CSVWriter.open(file)

  def write[A](data: List[List[String]]): Unit = {
    writer.writeAll(data)
  }
}
