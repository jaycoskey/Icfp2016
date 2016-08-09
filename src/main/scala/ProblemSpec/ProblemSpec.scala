package ProblemSpec

import scala.collection.mutable.ArrayBuffer

import Icfp2016.Util._
import Icfp2016.Vertex
import Icfp2016.Vertex._

import Silhouette._
import Skeleton._

class Polygon(val vertexCount: Int, val vertices: ArrayBuffer[Vertex]) {
  override def toString(): String = {
    var lines: LineBuffer = new ArrayBuffer
    appendLine(lines, vertexCount.toString())
    vertices.foreach(vertex => {
      appendLine(lines, vertex.toString())
    })
    lines.mkString
  }
}

class ProblemSpec(val silhouette: Silhouette, val skeleton: Skeleton) {
  override def toString(): String = {
    val lines: LineBuffer = new ArrayBuffer
    lines.append(silhouette.toString())
    lines.append(skeleton.toString())
    lines.mkString.dropRight(2) //  init
  }
}

object ProblemSpec {
  def parseProblemSpecFromResourceFile(filename: String): ProblemSpec = {
    val lines: LineBuffer = getLinesFromResourceFile(filename)
    parseProblemSpecFromStrings(lines)
  }

  def parseProblemSpecFromStrings(lines: LineBuffer): ProblemSpec = {
    val silhouette: Silhouette = parseSilhouetteFromStrings(lines)
    val skeleton: Skeleton = parseSkeletonFromStrings(lines)
    new ProblemSpec(silhouette, skeleton)
  }

  val trivialProblemSpec: ProblemSpec = {
    // Silhouette
    val vertices: ArrayBuffer[Vertex] = new ArrayBuffer
    val (sw, nw, se, ne) = Vertex.unitSquareCorners
    vertices.appendAll(Array(sw, se, ne, nw))
    val polygons: ArrayBuffer[Polygon] = new ArrayBuffer
    val polygon = new Polygon(vertices.length, vertices)
    polygons.append(polygon)
    val silhouette = new Silhouette(polygons.length, polygons)

    // Skeleton
    val edges: ArrayBuffer[Edge] = new ArrayBuffer
    edges.appendAll(Array((sw, se), (se, ne), (ne, nw), (nw, sw)))
    val skeleton: Skeleton = new Skeleton(edges.length, edges)

    val trivialProblemSpec = new ProblemSpec(silhouette, skeleton)
    trivialProblemSpec
  }
}
