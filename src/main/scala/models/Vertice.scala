package m.cheminot.models

import org.joda.time.DateTime

case class Vertice(id: String, name: String, lat: Double, lng: Double, edges: Seq[String], stopTimes: Seq[StopTime])

object Vertice {

  import m.cheminot.data.CheminotBuf

  lazy val PARIS = Vertice(Stop.STOP_PARIS, "Paris", 48.858859, 2.3470599, Nil, Nil)

  lazy val PARIS_LYON = Vertice(Stop.STOP_PARIS_LYON, "Paris Gare De Lyon", 48.844266, 2.373755, Nil, Nil)

  lazy val PARIS_NORD = Vertice(Stop.STOP_PARIS_NORD, "Paris Nord", 48.880845, 2.356722, Nil, Nil)

  def serialize(vertice: Vertice): CheminotBuf.Vertice = {
    val builder = CheminotBuf.Vertice.newBuilder()
    builder
      .setId(vertice.id)
      .setName(vertice.name)
      .setLat(vertice.lat)
      .setLng(vertice.lng)

    vertice.edges.foreach { edge =>
      builder.addEdges(edge)
    }

    vertice.stopTimes.foreach { stopTime =>
        builder.addStopTimes(StopTime.serialize(stopTime))
    }

    builder.build()
  }

  def serializeGraph(vertices: Seq[Vertice]): CheminotBuf.Graph = {
    val builder = CheminotBuf.Graph.newBuilder()
    vertices.foreach { vertice =>
      builder.getMutableVertices().put(vertice.id, serialize(vertice))
    }
    builder.build()
  }
}
