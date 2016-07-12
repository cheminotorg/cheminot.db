package org.cheminot.db

import org.joda.time.DateTime

import rapture.fs._
import rapture.uri._
import rapture.net._
import rapture.io._
import rapture.json._, jsonBackends.jawn._
import rapture.time._

import org.cheminot.misc

object Upgrade {

  case class Build(id: String, url: HttpQuery, timestamp: DateTime) {
    lazy val filename = s"${id}-${SubsetDir.formatter.print(timestamp)}"
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

    val datasetId = record.datasetid.as[String]

    val downloadId = record.fields.download.id.as[String]

    val url = HttpQuery.parse(s"https://ressources.data.sncf.com/explore/dataset/${datasetId}/files/${downloadId}/download/")

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

    lazy val noUpdateFound = {
      Logger.info("* NO UPDATE FOUND *")

      maybeBundle.map(DB.apply).getOrElse {
        sys.error("Something goes wrong: No update were found and nothing were found locally")
      }
    }

    try {

      val terBuild = fetchTerBuild()

      val interBuild = fetchInterBuild()

      val transBuild = fetchTransBuild()

      val needUpdate = maybeBundle match {
        case Some(currentBundle) =>
          val subsetsById = currentBundle.subsetDirs.map(s => s.id -> s.timestamp).toMap
          List(terBuild, interBuild, transBuild).exists { build =>
            val isNotUpToDate = subsetsById.get(build.id).exists(build.timestamp.isAfter(_))
            val n = if(isNotUpToDate) "" else "NOT "
            Logger.info(s"** ${build.id} is ${n}up to date **")
            isNotUpToDate
          }
        case None =>
          true
      }

      if(needUpdate) {

        Logger.info("* UPDATE FOUND *")

        val rootDir = config.gtfsDir / BundleId.next.value

        setupBuilds(rootDir, terBuild, interBuild, transBuild)

      } else {
        noUpdateFound
      }

    } catch {
      case e: Exception =>
        Logger.error("Unable to fetch build", e)
        noUpdateFound
    }
  }

  private def setupBuild(rootDir: FsUrl, build: Build): Unit = {
    val buildDir = rootDir / build.filename
    buildDir.mkdir(makeParents = true)
    val buildFile = buildDir / s"${build.filename}.zip"
    Logger.info(s"Downloading ${build.url} to ${buildFile}")
    build.url > buildFile
    misc.ZipUtils.unzip(buildFile.javaFile, buildDir.javaFile)
  }

  private def setupBuilds(rootDir: FsUrl, builds: Build*): DB = {
    builds.foreach(setupBuild(rootDir, _))
    DB.fromDirOrFail(rootDir)
  }
}
