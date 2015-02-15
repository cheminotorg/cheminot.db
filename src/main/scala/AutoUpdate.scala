package m.cheminot

import java.io.File
import java.net.URL
import org.apache.commons.io.FileUtils
import play.api.libs.json._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scalaj.http._

object AutoUpdate {

  val DEFAULT_RATE: Int = 3600 * 1000
  val INTENSE_RATE: Int = 5 * 60 * 1000

  case class Update(url: String, modified: DateTime, next: DateTime)

  object Update {
    def apply(json: JsValue): Update = {
      val modified = (json \ "metas" \ "modified").as[String]
      val next = {
        val description = (json \ "metas" \ "description").as[String]
        val UpdateReg = """(.|\s)*?<p>Prochaine mise à jour : (.*)?</p>""".r
        description match {
          case UpdateReg(_, updateTime) =>
            val formatter = DateTimeFormat.forPattern("dd MMMM yyyy").withLocale(java.util.Locale.FRENCH)
            formatter.parseDateTime(updateTime)
          case _ => sys.error("Unable to parse next update date from: " + description)
        }
      }
      val url = "https://ressources.data.sncf.com/api/datasets/1.0/sncf-ter-gtfs/attachments/export_ter_gtfs_last_zip/"
      Update(url, DateTime.parse(modified), next)
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
    val source = new URL(url)
    val destination = new File(to)
    FileUtils.copyURLToFile(source, destination)
    destination
  }

  @annotation.tailrec
  def loop(twitterOAuth: Option[misc.TwitterOAuth], gtfsRootDir: File, dbDir: File, gtfsBundle: () => Option[GtfsDirectory], rate: Int = DEFAULT_RATE): Unit = {
    val bestRate: Int = rateLimiter(rate) {
      val url = "https://ressources.data.sncf.com/api/datasets/1.0/sncf-ter-gtfs/?extrametas=true&interopmetas=true&timezone=Europe%2FBerlin"
      Console.out.println(s"GET $url")
      val response = Http(url).asString
      scala.util.Try(Json.parse(response.body)).toOption map { json =>
        val update = Update(json)
        val bundle = gtfsBundle()
        if(bundle.exists(gtfs => update.modified.isAfter(gtfs.version.date)) || bundle.isEmpty) {
          val name = Version.formatter.print(update.modified)
          val zip = downloadGtfsZip(update.url, gtfsRootDir.getAbsolutePath + "/" + name + ".zip")
          val bundleDir = new File(gtfsRootDir.getAbsolutePath + "/" + name)
          bundleDir.mkdirs
          misc.ZipUtils.unzip(zip, bundleDir)
          DB.fromDir(bundleDir).foreach(db => Persist.all(dbDir, db))
          twitterOAuth.foreach { oauth =>
            misc.Twitter.updateStatus(oauth, "@srenault_ Une nouvelle version de cheminot.db est disponible!")
          }
          DEFAULT_RATE
        } else {
          val now = DateTime.now
          if(update.next.getYear == now.getYear && update.next.getDayOfYear == now.getDayOfYear) {
            INTENSE_RATE
          } else {
            DEFAULT_RATE
          }
        }
      } getOrElse DEFAULT_RATE
    }
    loop(twitterOAuth, gtfsRootDir, dbDir, gtfsBundle, bestRate)
  }
}
