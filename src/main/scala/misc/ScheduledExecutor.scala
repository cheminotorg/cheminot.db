package org.cheminot.db.misc

import java.util.concurrent.ThreadPoolExecutor.AbortPolicy
import java.util.concurrent._

import scala.concurrent.{ Promise, Future }
import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions
import scala.util.Try
import scala.concurrent.stm._

object ScheduledExecutor {
  private val defaultHandler: RejectedExecutionHandler = new AbortPolicy
}

/**
 * A thread pool backed executor for tasks scheduled in the future
 * @param corePoolSize the number of threads to keep in the pool, even
 *                     if they are idle, unless { @code allowCoreThreadTimeOut} is set
 * @param threadFactory the factory to use when the executor
 *                      creates a new thread
 * @param handler the handler to use when execution is blocked
 *                because the thread bounds and queue capacities are reached
 * @throws IllegalArgumentException if { @code corePoolSize < 0}
 * @throws NullPointerException if { @code threadFactory} or
 *                                         { @code handler} is null
 */
case class ScheduledExecutor(corePoolSize: Int, threadFactory: ThreadFactory = Executors.defaultThreadFactory, handler: RejectedExecutionHandler = ScheduledExecutor.defaultHandler) {

  private val underlying: ScheduledExecutorService = new ScheduledThreadPoolExecutor(corePoolSize, threadFactory, handler)

  var started = Ref(false)

  /**
   * Creates a Future and schedules the operation to run after the given delay.
   *
   * @param operation to execute
   * @param by the time from now to delay execution
   * @tparam T the type of the operation's result
   * @return a Future that can be used to extract result
   * @throws RejectedExecutionException if the task cannot be
   *                                    scheduled for execution
   */
  def delayExecution[T](operation: ⇒ T)(by: FiniteDuration): CancellableFuture[T] = {
    val promise = Promise[T]()
    val scheduledFuture: ScheduledFuture[_] = underlying.schedule(new Runnable {
      override def run() = {
        promise.complete(Try(operation))
      }
    }, by.length, by.unit)
    new DelegatingCancellableFuture(promise.future, scheduledFuture.cancel)
  }

  def schedule(operation: ⇒ Unit)(period: FiniteDuration): Unit = {
    started.single() = true
    def step(operation: ⇒ Unit)(period: FiniteDuration): Unit = {
      if(started.single()) {
        delayExecution {
          operation
          step(operation)(period)
        }(period)
      }
    }
    operation
    step(operation)(period)
  }

  def stop(): Unit =
    started.single() = false
}

object CancellableFuture {
  implicit def extractFuture[T](cf: CancellableFuture[T]): Future[T] = cf.future
}

/**
 * Wraps a future, adding a method to cancel it
 * @tparam T
 */
trait CancellableFuture[T] {
  def future: Future[T]

  /**
   * Attempts to cancel execution of this task.  This attempt will
   * fail if the task has already completed, has already been cancelled,
   * or could not be cancelled for some other reason. If successful,
   * and this task has not started when {@code cancel} is called,
   * this task should never run.  If the task has already started,
   * then the {@code mayInterruptIfRunning} parameter determines
   * whether the thread executing this task should be interrupted in
   * an attempt to stop the task.
   *
   * <p>If this method returns true, the future should never complete.
   *
   * @param mayInterruptIfRunning { @code true} if the thread executing this
   *                                      task should be interrupted; otherwise, in-progress tasks are allowed
   *                                      to complete
   * @return { @code false} if the task could not be cancelled,
   *                 typically because it has already completed normally;
   *                 { @code true} otherwise
   */
  def cancel(mayInterruptIfRunning: Boolean): Boolean
}

private class DelegatingCancellableFuture[T](val future: Future[T], cancelMethod: (Boolean) ⇒ Boolean) extends CancellableFuture[T] {
  def cancel(interruptIfRunning: Boolean): Boolean = cancelMethod(interruptIfRunning)
}
