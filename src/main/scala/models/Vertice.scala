package m.cheminot.models

import org.joda.time.DateTime

case class Vertice(id: String, name: String, lat: Double, lng: Double, edges: Seq[String], stopTimes: Seq[StopTime])

object Vertice {

  lazy val PARIS = Vertice(Stop.STOP_PARIS, "Paris", 48.858859, 2.3470599, Nil, Nil)

  lazy val PARIS_LYON = Vertice(Stop.STOP_PARIS_LYON, "Paris Gare De Lyon", 48.844266, 2.373755, Nil, Nil)

  lazy val PARIS_NORD = Vertice(Stop.STOP_PARIS_NORD, "Paris Nord", 48.880845, 2.356722, Nil, Nil)

  def fromStopRecord(stopRecord: StopRecord, trips: List[Trip]): Vertice = {
    val zEdges = if(Stop.isParis(stopRecord.stopId)) Stop.parisStops.filterNot(_ == stopRecord.stopId).toList else Seq.empty[StopId]
    val zStopTimes = if(Stop.isParis(stopRecord.stopId)) Subway.stopTimes.get(stopRecord.stopId).getOrElse(Nil) else Seq.empty[StopTime]
    val (edges, stopTimes) = trips.foldLeft((zEdges, zStopTimes)) { (acc, trip) =>
      val (accEdges, accStopTimes) = acc
      val edges = trip.edgesOf(stopRecord.stopId)
      val stopTimes = trip.stopTimes.find(_.stopId == stopRecord.stopId).toList.map { st =>
        StopTime(st.tripId, st.arrival, st.departure, stopRecord.stopId, st.pos)
      }
      (edges ++: accEdges) -> (stopTimes ++: accStopTimes)
    }
    Vertice(stopRecord.stopId, stopRecord.stopName, stopRecord.stopLat, stopRecord.stopLong, edges.distinct, stopTimes.distinct)
  }

  def merge[A](vertices: List[A], z: Vertice)(get: A => Option[Vertice]): Vertice = {
    vertices.map(get).flatten.foldLeft(z) { (acc, vertice) =>
      acc.copy(
        edges = (acc.edges ++: vertice.edges).distinct,
        stopTimes = (acc.stopTimes ++: vertice.stopTimes).map { stopTime =>
          stopTime.copy(stopId = acc.id)
        }
      )
    }
  }
}
