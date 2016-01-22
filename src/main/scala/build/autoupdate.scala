package m.cheminot.build

import rapture.uri._
import rapture.net._
import rapture.io._
import rapture.json._, jsonBackends.jackson._

import java.io.File
import java.net.URL
import org.apache.commons.io.FileUtils
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import m.cheminot.Config
import m.cheminot.misc

object AutoUpdate {

  val DEFAULT_RATE: Long = 3600 * 1000

  case class Update(url: String, modified: DateTime)

  object Update {
    def apply(json: Json): Update = {
      val modified = json.metas.modified.as[String]
      val url = "https://ressources.data.sncf.com/api/datasets/1.0/sncf-ter-gtfs/attachments/export_ter_gtfs_last_zip/"
      Update(url, DateTime.parse(modified))
    }
  }

  def rateLimiter[A](rate: Long)(f: => A) = {
    val start = System.currentTimeMillis()
    val x = f
    val end = System.currentTimeMillis()
    val remaining = rate - (end - start)
    if(remaining > 0) {
      Thread.sleep(remaining)
    }
    x
  }

  def downloadGtfsZip(url: String, to: String): File = {
    println("Downloading " + url)
    val source = new URL(url)
    val destination = new File(to)
    FileUtils.copyURLToFile(source, destination)
    println("Downloading done");
    destination
  }

  def notify(config: Config, message: String): Unit = {
    ???
  }

  @annotation.tailrec
  def loop(config: Config, gtfsRootDir: File, dbDir: File, gtfsBundle: () => Option[GtfsBundle], rate: Long = DEFAULT_RATE): Unit = {
    val bestRate: Long = rateLimiter(rate) {
      val endpoint = uri"https://ressources.data.sncf.com/api/datasets/1.0/sncf-ter-gtfs/?extrametas=true&interopmetas=true&timezone=Europe%2FBerlin"
      println(s"GET $endpoint")
      val response = endpoint.httpGet().slurp[Char]
      scala.util.Try(Json.parse(response)).toOption map { json =>
        val update = Update(json)
        val bundle = gtfsBundle()
        if(bundle.exists(gtfs => update.modified.isAfter(gtfs.bundleId.date)) || bundle.isEmpty) {
          val name = BundleId.formatter.print(update.modified)
          val zip = downloadGtfsZip(update.url, gtfsRootDir.getAbsolutePath + "/" + name + ".zip")
          val bundleDir = new File(gtfsRootDir.getAbsolutePath + "/" + name)
          val terDir = new File(bundleDir.getAbsolutePath + "/ter/")
          terDir.mkdirs
          misc.ZipUtils.unzip(zip, terDir)
          val db = DB.fromDir(gtfsRootDir).map(storage.Neo4j.write(dbDir, _))
          DEFAULT_RATE
        } else {
          println(s"Nothing to update")
          val now = DateTime.now
          DEFAULT_RATE
        }
      } getOrElse DEFAULT_RATE
    }
    loop(config, gtfsRootDir, dbDir, gtfsBundle, bestRate)
  }
}
