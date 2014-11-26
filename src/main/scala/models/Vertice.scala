package m.cheminot.models

import org.joda.time.DateTime

case class Vertice(id: String, name: String, edges: Seq[String], stopTimes: Seq[StopTime])

object Vertice {

  import m.cheminot.data.CheminotBuf

  def serialize(vertice: Vertice): CheminotBuf.Vertice = {
    val builder = CheminotBuf.Vertice.newBuilder()
    builder.setId(vertice.id).setName(vertice.name)
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
