package m.cheminot.misc

import java.io.File
import org.apache.commons.io.FileUtils
import scala.language.postfixOps
import scala.util.parsing.combinator._

object CSV extends RegexParsers {
  override protected val whiteSpace = """[\t]""".r

  def COMMA   = ","
  def DQUOTE  = "\""
  def DQUOTE2 = "\"\"" ^^ { case _ => "\"" }
  def CR      = "\r"
  def LF      = "\n"
  def CRLF    = "\r\n"
  def TXT     = "[^\",\r\n]".r

  def file: Parser[List[List[String]]] = repsep(record, CRLF) <~ opt(CRLF)
  def record: Parser[List[String]] = rep1sep(field, COMMA)
  def field: Parser[String] = (escaped|nonescaped)
  def escaped: Parser[String] = (DQUOTE~>((TXT|COMMA|CR|LF|DQUOTE2)*)<~DQUOTE) ^^ { case ls => ls.mkString("")}
  def nonescaped: Parser[String] = (TXT*) ^^ { case ls => ls.mkString("") }

  def parse(s: String): CSVFile.Rows =
    parseAll(file, s) match {
      case Success(res, _) => res
      case _ => List[List[String]]()
    }
}

object CSVFile {
  type Rows = List[List[String]]
}

case class CSVFile(file: File) {
  lazy val content = FileUtils.readFileToString(file, "utf-8")

  def read(): CSVFile.Rows = CSV.parse(content)
}

case class CSVDirectory(directory: File) {

  def read(): Map[String, CSVFile.Rows] = {
    directory.listFiles.filter(_.getName.endsWith(".txt")).map { csv =>
      Console.out.println("Reading " + csv.getName)
      val rows = CSVFile(csv).read()
      csv.getName -> rows
    }
  }.toMap
}
