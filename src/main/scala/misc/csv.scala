package m.cheminot.misc

import java.io.File
import org.apache.commons.io.FileUtils
import scala.language.postfixOps
import scala.util.parsing.combinator._

object CSV extends RegexParsers {

  import CSVFile._

  override protected val whiteSpace = """[\t]""".r

  def COMMA   = ","
  def DQUOTE  = "\""
  def DQUOTE2 = "\"\"" ^^ { case _ => "\"" }
  def CR      = "\r"
  def LF      = "\n"
  def CRLF    = "\r\n"
  def TXT     = "[^\",\r\n]".r

  def file(collect: PartialFunction[Record, Record]): Parser[List[Record]] = repsep(record(collect), CRLF|CR|LF) <~ opt(CRLF)
  def record(collect: PartialFunction[Record, Record]): Parser[Record] = rep1sep(field, COMMA).filter(collect.isDefinedAt).map(collect)
  def field: Parser[String] = (escaped|nonescaped)
  def escaped: Parser[String] = (DQUOTE~>((TXT|COMMA|CR|LF|DQUOTE2)*)<~DQUOTE) ^^ { case ls => ls.mkString("")}
  def nonescaped: Parser[String] = (TXT*) ^^ { case ls => ls.mkString("") }

  def parse(s: String, collect: PartialFunction[Record, Record]): Records =
    parseAll(file(collect), s) match {
      case Success(res, _) => res
      case _ => List[List[String]]()
    }
}

object CSVFile {
  type Record = List[String]
  type Records = List[Record]
}

case class CSVFile(file: File) {

  import CSVFile._

  lazy val content = FileUtils.readFileToString(file, "utf-8")

  def read(collect: PartialFunction[Record, Record]): Records = {
    CSV.parse(content, collect)
  }
}
