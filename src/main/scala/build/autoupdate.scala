package org.cheminot.db.build

import scala.language.postfixOps
import scala.concurrent.duration._
import org.joda.time.DateTime

import rapture.fs._
import rapture.uri._
import rapture.net._
import rapture.io._
import rapture.json._, jsonBackends.jawn._
import rapture.time._

import org.cheminot.misc
import org.cheminot.misc.scheduler.Scheduler
import org.cheminot.db.{Config, Logger}

object AutoUpdate {

  case class Build(id: String, url: HttpUrl, timestamp: DateTime) {
    lazy val filename = s"${id}-${SubsetDir.formatter.print(timestamp)}"
  }

  def start(bundle: GtfsBundle)(implicit config: Config): Unit = {
    Scheduler.schedule(0 seconds, 10 seconds) { executor =>
      executor.stop()
      doIt(maybeBundle = Option(bundle))
    }
  }

  private def fetchBuild(id: String, endpoint: HttpUrl, dataset: String): Build = {

    val datetimeFormater = org.joda.time.format.ISODateTimeFormat.dateTimeParser()

    val params = Map(
      "dataset" -> dataset,
      "rows" -> "1",
      "start" -> "0",
      "timezone" -> "UTC"
    )

    val ressource = endpoint.query(params)

    Logger.info(s"GET $ressource")

    val response = ressource.httpGet()

    val json = Json.parse(response.slurp[Char])

    val record = json.records.as[List[Json]].head

    val url = Http.parse(record.fields.url.as[String])

    val timestamp = datetimeFormater.parseDateTime(record.record_timestamp.as[String])

    Build(id, url, timestamp)
  }

  private def fetchTerBuild(): Build = {
    val endpoint = Http.parse("https://ressources.data.sncf.com/api/records/1.0/search")
    fetchBuild("ter", endpoint, dataset = "sncf-ter-gtfs")
  }

  private def fetchTransBuild(): Build = {
    val endpoint = Http.parse("https://ressources.data.sncf.com/api/records/1.0/search")
    fetchBuild("trans", endpoint, dataset = "sncf-transilien-gtfs")
  }

  private def fetchInterBuild(): Build = {
    val endpoint = Http.parse("https://ressources.data.sncf.com/api/records/1.0/search")
    fetchBuild("inter", endpoint, dataset = "sncf-intercites-gtfs")
  }

  def doIt(maybeBundle: Option[GtfsBundle] = None)(implicit config: Config): DB = {

    val terBuild = fetchTerBuild()

    val interBuild = fetchInterBuild()

    val transBuild = fetchTransBuild()

    val needUpdate = maybeBundle.map { currentBundle =>
      val subsetsById = currentBundle.subsetDirs.map(s => s.id -> s.timestamp).toMap
      List(terBuild, interBuild, transBuild).exists { build =>
        val isUpToDate = subsetsById.get(build.id).filter(build.timestamp.isAfter(_)).isDefined
        val n = if(isUpToDate) "" else "NOT "
        Logger.info(s"** ${build.id} is ${n}up to date **")
        !isUpToDate
      }
    } getOrElse true

    if(needUpdate) {

      Logger.info("* UPDATE FOUND *")

      val rootDir = config.gtfsDir / BundleId.next.value

      setupBuilds(rootDir, terBuild, interBuild, transBuild)

    } else {

      Logger.info("* NO UPDATE FOUND *")

      maybeBundle.map(DB.apply).getOrElse {
        sys.error("Something goes wrong: No update were found and nothing were found locally")
      }
    }
  }

  private def setupBuild(rootDir: FsUrl, build: Build): Unit = {
    val buildDir = rootDir / build.filename
    buildDir.mkdir(makeParents = true)
    val buildFile = buildDir / s"${build.filename}.zip"
    build.url > buildFile
    misc.ZipUtils.unzip(buildFile.javaFile, buildDir.javaFile)
  }

  private def setupBuilds(rootDir: FsUrl, builds: Build*): DB = {
    builds.foreach(setupBuild(rootDir, _))
    DB.fromDirOrFail(rootDir)
  }
}
