package m.cheminot.misc

import scala.concurrent.{ Future, ExecutionContext }

object FutureUtils {

  def groupSequentially[A, B](seq: Seq[A], i: Int)(f: A => Future[B])(implicit ec: ExecutionContext): Future[Seq[B]] = {
    val groups: Seq[Seq[A]] = seq.grouped(i).toSeq
    traverseSequentially[Seq[A], Seq[B]](groups) { group =>
      Future.sequence(group.map(f))
    }.map(_.flatten)
  }

  def traverseSequentially[A, B](seq: Seq[A])(f: A => Future[B])(implicit ec: ExecutionContext): Future[Seq[B]] = seq match {
    case h +: t => f(h).flatMap { r =>
      traverseSequentially(t)(f) map (r +: _)
    }
    case _ => Future.successful(Seq.empty)
  }
}
