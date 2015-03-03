package m.cheminot.misc

object Measure {

  def duration[A](id: String)(f: => A): A = {
    println(s"[$id] Starting...")
    val start = System.currentTimeMillis
    val res = f
    val end = System.currentTimeMillis
    val d: Long = (end - start) / 1000
    println(s"[$id] Finished in ${d}s")
    res
  }
}
