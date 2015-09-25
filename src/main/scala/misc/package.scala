import java.util.concurrent.Executors
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future, ExecutionContext }

package m.cheminot {

  package object misc {

    private val THREADS_PER_POOL = Option(System.getProperty("threads")).map(_.toInt).getOrElse(5)

    implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(THREADS_PER_POOL))

    def par[A, B](aaa: Seq[A])(f: (A) => B)(implicit ec: ExecutionContext): Seq[B] =
      Await.result(
        Future.sequence {
          aaa.grouped(THREADS_PER_POOL).toSeq.map { group =>
            Future(group.map(f))
          }
        }.map(_.flatten),
        1.hours
      )
  }
}
