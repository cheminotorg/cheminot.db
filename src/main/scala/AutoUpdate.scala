package m.cheminot

import java.io.File
import java.net.URL
import org.apache.commons.io.FileUtils
import play.api.libs.json._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scalaj.http._

object AutoUpdate {

  val DEFAULT_RATE: Long = 3600 * 1000

  case class Update(url: String, modified: DateTime)

  object Update {
    def apply(json: JsValue): Update = {
      val modified = (json \ "metas" \ "modified").as[String]
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
    for {
      oauth <- config.twitterOAuth
      pseudo <- config.twitterPseudo
    } yield {
      misc.Twitter.updateStatus(oauth, "@" + pseudo + " " + message)
    }
  }

  @annotation.tailrec
  def loop(config: Config, gtfsRootDir: File, dbDir: File, gtfsBundle: () => Option[GtfsBundle], rate: Long = DEFAULT_RATE): Unit = {
    val bestRate: Long = rateLimiter(rate) {
      val url = "https://ressources.data.sncf.com/api/datasets/1.0/sncf-ter-gtfs/?extrametas=true&interopmetas=true&timezone=Europe%2FBerlin"
      println(s"GET $url")
      val response = Http(url).asString
      scala.util.Try(Json.parse(response.body)).toOption map { json =>
        val update = Update(json)
        val bundle = gtfsBundle()
        if(bundle.exists(gtfs => update.modified.isAfter(gtfs.version.date)) || bundle.isEmpty) {
          val name = Version.formatter.print(update.modified)
          val zip = downloadGtfsZip(update.url, gtfsRootDir.getAbsolutePath + "/" + name + ".zip")
          val bundleDir = new File(gtfsRootDir.getAbsolutePath + "/" + name)
          val terDir = new File(bundleDir.getAbsolutePath + "/ter/")
          terDir.mkdirs
          misc.ZipUtils.unzip(zip, terDir)
          notify(config, "Je m'apprête à builder une nouvelle version de cheminotDB.")
          val db = DB.fromDir(gtfsRootDir).map { db =>
            storage.Neo4j.write(dbDir, db)
            db
          }
          notify(config, "Une nouvelle version de cheminotDB est disponible: " + db.map(_.version.value).getOrElse("N/A"))
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
