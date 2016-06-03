import java.util.concurrent.Executors
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future, ExecutionContext }
import java.util.concurrent.atomic.AtomicInteger

package m.cheminot {

  package object misc {

    private val THREADS_PER_POOL = Option(System.getProperty("threads")).map(_.toInt).getOrElse(20)

    implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(THREADS_PER_POOL))

    def displayProgress(progress: Int) = {
      val p = (progress * 70) / 100
      val todo = 70 - p
      val total = List.fill(p)("#") ++: List.fill(todo)(" ")
      print(s"[${total.mkString}] ${p}%\r")
    }

    def par[A, B](aaa: Seq[A], debug: Boolean = false)(f: (A) => B)(implicit ec: ExecutionContext): Seq[B] = {
      val n = 20 //aaa.size / (THREADS_PER_POOL * 2)
      if(debug) { println(s"[progress] total: ${aaa.size} | grouped: ${n}") }
      val counter = new AtomicInteger(0);
      val result = Await.result(
        misc.FutureUtils.groupSequentially(aaa, n) { a =>
          Future(f(a)).andThen {
            case _ =>
              if(debug) {
                val progress = scala.math.round(counter.incrementAndGet.toFloat / aaa.size.toFloat * 100)
                displayProgress(progress.toInt)
              }
          }
        },
        1.hours
      )
      println("");
      result
    }
  }
}
