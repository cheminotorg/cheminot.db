import java.util.concurrent.Executors
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future, ExecutionContext }

package m.cheminot {

  package object misc {

    private val THREADS_PER_POOL = Option(System.getProperty("threads")).map(_.toInt).getOrElse(20)

    implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(THREADS_PER_POOL))

    def par[A, B](aaa: Seq[A], debug: Boolean = false)(f: (A) => B)(implicit ec: ExecutionContext): Seq[B] = {
      val grouped = aaa.size / (THREADS_PER_POOL * 2)
      if(debug) { println(s"[progress] total: ${aaa.size} | grouped: ${grouped}") }
      var counter = 0;
      val groups = aaa.grouped(20).toList
      Await.result(
        Future.sequence {
          groups.map { group =>
            Future(group.map(f)).map { xxx =>
              if(debug) {
                synchronized { counter += xxx.size }
                println(s"[progress] ${counter} / ${aaa.size}")
              }
              xxx
            }
          }
        }.map(_.flatten),
        1.hours
      )
    }
  }
}
