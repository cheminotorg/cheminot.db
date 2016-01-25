package m.cheminot.build

import scala.concurrent.duration._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import java.io.{File => JFile}

import rapture.fs._
import rapture.uri._
import rapture.net._
import rapture.io._
import rapture.json._, jsonBackends.jackson._
import rapture.time._

import m.cheminot.Config
import m.cheminot.misc
import m.cheminot.build
import m.cheminot.http

object AutoUpdate {

  case class Build(name: String, url: HttpUrl, updatedDate: DateTime, startDate: DateTime, endDate: DateTime) {
    lazy val id = s"${name}-${updatedDate.getMillis}-${startDate.getMillis}-${endDate.getMillis}"
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

  private def fetchBuild(name: String, endpoint: HttpUrl): Build = {

    val formatter = DateTimeFormat.forPattern("dd/MM/yyyy")

    val response = endpoint.httpGet()

    val json = Json.parse(response.slurp[Char])

    val fields = json.records.as[List[Json]].head.fields

    val updatedDate = formatter.parseDateTime(
      fields.mise_a_jour.as[String]
    )

    val startDate = formatter.parseDateTime(
      fields.date_de_debut_de_validite.as[String]
    )

    val endDate = formatter.parseDateTime(
      fields.date_de_fin_de_validite.as[String]
    )

    val url = Http.parse(fields.url.as[String])

    Build(name, url, updatedDate, startDate, endDate)
  }

  private def fetchTerBuild(): Build = {
    val endpoint = uri"https://ressources.data.sncf.com/api/records/1.0/search/?dataset=sncf-ter-gtfs&rows=40&start=0&timezone=Europe%2FBerlin"
    fetchBuild("ter", endpoint)
  }

  private def fetchTransBuild(): Build = {
    val endpoint = uri"https://ressources.data.sncf.com/api/records/1.0/search/?dataset=sncf-transilien-gtfs&rows=40&start=0&timezone=Europe%2FBerlin"
    fetchBuild("trans", endpoint)
  }

  private def fetchInterBuild(): Build = {
    val endpoint = uri"https://ressources.data.sncf.com/api/records/1.0/search/?dataset=sncf-intercites-gtfs&rows=40&start=0&timezone=Europe%2FBerlin"
    fetchBuild("inter", endpoint)
  }

  def doIt(maybeBundle: Option[GtfsBundle] = None, onBeforeUpdate: => Unit = (), onAfterUpdate: DB => Unit = _ => ())(implicit config: Config): Option[DB] = {

    val terBuild = fetchTerBuild()

    val interBuild = fetchInterBuild()

    val transBuild = fetchTransBuild()

    val needUpdate = maybeBundle.map { currentBundle =>

      val subsetsById: Map[String, SubsetDir] = currentBundle.data.subsetDirs.map(s => s.id -> s).toMap

      val buildsWithSubsets: Map[Build, SubsetDir] = List(terBuild, interBuild, transBuild).flatMap { build =>
        subsetsById.get(build.name).map(build -> _)
      }.toMap

      buildsWithSubsets.foldLeft(false) {
        case (b, (build, subset)) =>
          if(b) true else {
            subset.updatedDate.isAfter(build.updatedDate)
          }
      }
    } getOrElse true

    if(needUpdate) {

      val rootDir = config.gtfsDir / DateTime.now.getMillis.toString

      setupBuilds(rootDir, terBuild, interBuild, transBuild)

      Option(build.DB.setup())

    } else None
  }

  private def setupBuild(rootDir: FileUrl, build: Build): Unit = {
    val buildDir = rootDir / build.id
    val buildFile = buildDir / s"${build.name}.zip"
    build.url > buildFile
    misc.ZipUtils.unzip(buildFile.javaFile, buildDir.javaFile)
  }

  private def setupBuilds(rootDir: FileUrl, builds: Build*): Unit =
    builds.foreach(setupBuild(rootDir, _))
}
