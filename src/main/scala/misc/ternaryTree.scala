package m.cheminot.misc

import scala.language.postfixOps

case class TTreeNode[A](c: Char, left: Option[TTreeNode[A]], eq: Option[TTreeNode[A]], right: Option[TTreeNode[A]], isEnd: Boolean, data: Seq[A] = Seq.empty[A])

object TTreeNode {

  def apply[A](c: Char, data: Seq[A]): TTreeNode[A] = TTreeNode[A](c, None, None, None, false, data)

  def apply[A](x: Seq[(String, A)]): TTreeNode[A] = {
    val (firstWord, data) = x.head
    val startNode = insert(firstWord.toList, None, data)
    x.tail.foldLeft(startNode) { (node, x) =>
      val (word, data) = x
      insert(word.toList, Some(node), data)
    }
  }

  def insert[A](word: List[Char], maybeNode: Option[TTreeNode[A]], data: A): TTreeNode[A] = {
    (maybeNode, word) match {
      case (None, (h :: t)) => insert(word, Some(TTreeNode[A](h, Seq.empty[A])), data)
      case (Some(node), (h :: t)) if h < node.c => node.copy(left = Some(insert(word, node.left, data)))
      case (Some(node), (h :: t)) if h > node.c => node.copy(right = Some(insert(word, node.right, data)))
      case (Some(node), (h :: _)) if h == node.c =>
        if(word.length > 1) {
          node.copy(eq = Some(insert(word.tail, node.eq, data)))
        } else {
          node.copy(isEnd = true, data = (data +: node.data).distinct)
        }
    }
  }

  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  implicit object charFormat extends Format[Char] {
    def reads(json: JsValue): JsResult[Char] =
      json.asOpt[String].map(_.toList) match {
        case Some(h :: _) => JsSuccess(h)
        case _ => JsError("Unable to read char from JSON")
      }
    def writes(c: Char): JsValue = JsString(c.toString)
  }

  implicit def writer[A]()(implicit writerA: Writes[A]): Writes[TTreeNode[A]] =
    ((__ \ "c").write[Char] and
     (__ \ "left").lazyWriteNullable(writer[A]()) and
     (__ \ "eq").lazyWriteNullable(writer[A]()) and
     (__ \ "right").lazyWriteNullable(writer[A]()) and
     (__ \ "isEnd").write[Boolean] and
     (__ \ "data").write[Seq[A]]
    )( node => {
      (node.c, node.left, node.eq, node.right, node.isEnd, node.data)
    })

  implicit def reader[A]()(implicit readerA: Reads[A]): Reads[TTreeNode[A]] =
    ((__ \ "c").read[Char] and
     (__ \ "left").lazyReadNullable(reader[A]()) and
     (__ \ "eq").lazyReadNullable(reader[A]()) and
     (__ \ "right").lazyReadNullable(reader[A]()) and
     (__ \ "isEnd").read[Boolean] and
     (__ \ "data").read[Seq[A]]
    )((c, left, eq, right, isEnd, data) => {
      TTreeNode(c, left, eq, right, isEnd, data)
    })

  implicit val writerTupleSeqAndString: Writes[(String, String)] = (
    (__ \ "id").write[String] and
    (__ \ "name").write[String]
  ) tupled

  implicit val readerTupleSeqAndString: Reads[(String, String)] = (
    (__ \ "id").read[String] and
    (__ \ "name").read[String]
  ) tupled

  implicit val writerNodeString: Writes[TTreeNode[(String, String)]] = writer[(String, String)]()

  implicit val readerNodeString: Reads[TTreeNode[(String, String)]] = reader[(String, String)]()
}
