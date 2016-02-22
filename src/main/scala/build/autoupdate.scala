package m.cheminot.build

import scala.concurrent.duration._
import org.joda.time.DateTime
import java.io.{File => JFile}

import rapture.fs._
import rapture.uri._
import rapture.net._
import rapture.io._
import rapture.json._, jsonBackends.jawn._
import rapture.time._

import m.cheminot.Config
import m.cheminot.misc
import m.cheminot.build
import m.cheminot.http

object AutoUpdate {

  case class Build(
    recordId: String,
    name: String,
    url: HttpUrl,
    updatedDate: Option[DateTime],
    startDate: Option[DateTime],
    endDate: Option[DateTime]
  ) {
    lazy val id = {
      def formatDate(date: Option[DateTime]): String =
        (date.map(SubsetDir.formatter.print) getOrElse "xxxxXXxx").toString

      s"${name}-${recordId}-${formatDate(updatedDate)}-${formatDate(startDate)}-${formatDate(endDate)}"
    }
  }

  private lazy val executor = m.cheminot.misc.ScheduledExecutor(1)

  def start(bundle: GtfsBundle)(implicit config: Config): Unit =
    executor.schedule {
      AutoUpdate.stop()
      doIt(
        maybeBundle = Option(bundle),
        onBeforeUpdate = AutoUpdate.stop(),
        onAfterUpdate = { newdb =>
          http.State.set(newdb)
          AutoUpdate.start(newdb.bundle)
        }
      )
    }(1.seconds)

  def stop(): Unit =
    executor.stop()

  private def fetchBuild(name: String, endpoint: HttpUrl, dataset: String): Build = {

    val formatter = misc.DateTime.forPattern("dd/MM/yyyy")

    val params = Map(
      'dataset -> dataset,
      'rows -> "1",
      'start -> "0",
      'timezone -> "UTC"
    )

    val ressource = endpoint.query(params)

    println(s"GET $ressource")

    val response = ressource.httpGet()

    val json = Json.parse(response.slurp[Char])

    val record = json.records.as[List[Json]].head

    val recordId = record.recordid.as[String]

    val fields = record.fields

    val updatedDate = fields.mise_a_jour.as[Option[String]].map(formatter.parseDateTime)

    val startDate = fields.date_de_debut_de_validite.as[Option[String]].map(formatter.parseDateTime)

    val endDate = fields.date_de_fin_de_validite.as[Option[String]].map(formatter.parseDateTime)

    val url = Http.parse(fields.url.as[String])

    Build(recordId, name, url, updatedDate, startDate, endDate)
  }

  private def fetchTerBuild(): Build = {
    val endpoint = uri"https://ressources.data.sncf.com/api/records/1.0/search"
    fetchBuild("ter", endpoint, dataset = "sncf-ter-gtfs")
  }

  private def fetchTransBuild(): Build = {
    val endpoint = uri"https://ressources.data.sncf.com/api/records/1.0/search"
    fetchBuild("trans", endpoint, dataset = "sncf-transilien-gtfs")
  }

  private def fetchInterBuild(): Build = {
    val endpoint = uri"https://ressources.data.sncf.com/api/records/1.0/search"
    fetchBuild("inter", endpoint, dataset = "sncf-intercites-gtfs")
  }

  def doIt(maybeBundle: Option[GtfsBundle] = None, onBeforeUpdate: => Unit = (), onAfterUpdate: DB => Unit = _ => ())(implicit config: Config): Option[DB] = {

    scala.util.Try {

      val terBuild = fetchTerBuild()

      val interBuild = fetchInterBuild()

      val transBuild = fetchTransBuild()

      val needUpdate = maybeBundle.map { currentBundle =>

        val subsetsById: Map[String, SubsetDir] =
          currentBundle.data.subsetDirs.map(s => s.id -> s).toMap

        val buildsWithSubsets: Map[Build, SubsetDir] =
          List(terBuild, interBuild, transBuild).flatMap { build =>
            subsetsById.get(build.recordId).map(build -> _)
          }.toMap

        buildsWithSubsets.foldLeft(false) {
          case (b, (build, subset)) =>
            if(b) true else {
              subset.id != build.recordId
            }
        }
      } getOrElse true

      if(needUpdate) {

        println("Update found")

        val rootDir = config.gtfsDir / BundleId.next.value

        setupBuilds(rootDir, terBuild, interBuild, transBuild)

        Option(build.DB.mount())

      } else {

        println("No update found")

        None
      }

    }.recover {
      case e: Exception =>
        e.printStackTrace
        None
    }.toOption.flatten
  }

  private def setupBuild(rootDir: FileUrl, build: Build): Unit = {
    val buildDir = rootDir / build.id
    buildDir.mkdir(makeParents = true)
    val buildFile = buildDir / s"${build.name}.zip"
    build.url > buildFile
    misc.ZipUtils.unzip(buildFile.javaFile, buildDir.javaFile)
  }

  private def setupBuilds(rootDir: FileUrl, builds: Build*): Unit =
    builds.foreach(setupBuild(rootDir, _))
}
