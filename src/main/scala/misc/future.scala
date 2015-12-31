package m.cheminot.misc

import scala.concurrent.{ Future, ExecutionContext }

object FutureUtils {

  def groupSequentially[A, B](seq: Seq[A], i: Int)(f: A => Future[B])(implicit ec: ExecutionContext): Future[Seq[B]] = {
    import play.api.libs.iteratee.{ Enumerator, Iteratee }

    val groups: Seq[Seq[A]] = seq.grouped(i).toSeq
    val todo = Enumerator(groups: _*)
    val consummer = Iteratee.foldM[Seq[A], Seq[B]](Seq.empty[B]) {
      case (acc, group) =>
        Future.sequence(group.map { a => f(a) }).map(_ ++: acc)
    }
    todo.run(consummer)
  }
}
