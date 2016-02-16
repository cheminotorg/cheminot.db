package m.cheminot.build

import m.cheminot.misc._

object Builder {

  def build(gtfsBundle: GtfsBundle): (Map[VerticeId, Vertice], Map[TripId, Trip]) = {
    val t = Builder.buildTrips(gtfsBundle.data)
    val (vertices, refs) = Builder.buildGraph(gtfsBundle.data.stops, t)
    Builder.fixGraph(vertices, refs) -> Builder.fixTrips(t.filter(_.isValid), refs)
  }

  private def fixVertice(vertice: Vertice)(f: StopId => Option[StopId]): Vertice = {
    vertice.copy(
      edges = vertice.edges.map { edge =>
        f(edge) getOrElse edge
      }.distinct
    )
  }

  private def fixGraph(graph: Map[VerticeId, Vertice], refs: Map[StopId, VerticeId]): Map[VerticeId, Vertice] = {
    graph.mapValues { vertice =>
      fixVertice(vertice)(refs.get)
    }
  }

  private def fixTrip(trip: Trip)(f: StopId => Option[VerticeId]): Trip = {
    trip.copy(
      stopTimes = trip.stopTimes.map { stopTime =>
        f(stopTime.stopId) map (stopId => stopTime.copy(stopId = stopId)) getOrElse stopTime
      }.distinct
    )
  }

  private[build] def distinctTrips(trips: List[Trip]): List[Trip] = {
    val distinctTrips = trips.distinct
    val table = distinctTrips.foldLeft(Map.empty[StopTime, Seq[Trip]]) {
      case (acc, trip) =>
      trip.stopTimes.foldLeft(acc) {
        case (acc, stopTime) =>
          val updatedStopTimes = acc.get(stopTime) match {
            case None => Seq(trip)
            case Some(trips) => (trip +: trips).distinct
          }
          acc + (stopTime -> updatedStopTimes)
      }
    }
    distinctTrips.filterNot { trip =>
      (for {
        departure <- trip.stopTimes.headOption
        arrival <- trip.stopTimes.lastOption
        tripsA <- table.get(departure)
        tripsB <- table.get(arrival)
      } yield {
        val tripsAB = tripsA ++: tripsB
        tripsAB.exists { t =>
          trip != t && t.contains(trip)
        }
      }) getOrElse false
    }
  }

  private def fixTrips(trips: List[Trip], refs: Map[StopId, VerticeId]): Map[TripId, Trip] = {
    distinctTrips(trips.map(fixTrip(_)(refs.get))).map(trip => trip.id -> trip).toMap
  }

  private def buildGraph(stopRecords: List[StopRecord], trips: List[Trip]): (Map[VerticeId, Vertice], Map[StopId, VerticeId]) = {
    import scala.collection.JavaConverters._

    Measure.duration("Graph") {
      val refs = new java.util.concurrent.ConcurrentHashMap[StopId, VerticeId]()
      val groupedByParent: Map[StopId, Seq[StopRecord]] = stopRecords.groupBy(_.parentStation)
      val graph: Map[VerticeId, Vertice] = par(groupedByParent.toSeq, debug = true) {
        case (parentStationId, child :: children) =>
          val vertice = Vertice.fromStopRecord(child, trips).copy(id = parentStationId)
          val merged = Vertice.merge(children, vertice) { vertice =>
            Option(Vertice.fromStopRecord(vertice, trips))
          }
          (child +: children).foreach { stopRecord =>
            refs.put(stopRecord.stopId, vertice.id)
          }
          merged.id -> merged
        case x => sys.error("Unable to build graph: " + x)
      }.toMap
      graph -> refs.asScala.toMap
    }
  }

  private def buildTrips(parsed: ParsedGtfsDirectory): List[Trip] =
    Measure.duration("Trips") {
      println(s"** Trips: ${parsed.trips.size}\n** StopTimes: ${parsed.stopTimes.size}\n** Calendar: ${parsed.calendar.size}")
      par(parsed.trips, debug = true) { tripRecord =>
        val maybeService = parsed.calendar.view.find(_.serviceId == tripRecord.serviceId).map(Calendar.fromRecord)
        val stopTimesForTrip = parsed.stopTimes.collect {
          case stopTimeRecord if(stopTimeRecord.tripId == tripRecord.tripId) =>
            StopTime.fromRecord(stopTimeRecord)
        }.toList

        Trip.fromRecord(tripRecord, tripRecord.routeId, maybeService, stopTimesForTrip)
      }.toList
    }
}
