package ProblemSpec

import Icfp2016.Util._
import Icfp2016.Vertex
import Icfp2016.Vertex._
import _root_.ProblemSpec.ProblemSpec._

import scala.collection.mutable.ArrayBuffer

class Silhouette(val polygonCount: Int, val polygons: ArrayBuffer[Polygon]) {
  override def toString(): String = {
    val lines: LineBuffer = new ArrayBuffer
    appendLine(lines, polygonCount.toString)
    polygons.foreach(polygon => lines.append(polygon.toString))
    lines.mkString
  }
}

object Silhouette {
  def parsePolygonCountFromStrings(lines: LineBuffer): Int = readLine(lines).toInt

  def parseVertexCountFromStrings(lines: LineBuffer): Int = {
    readLine(lines).toInt
  }

  def parseVertexFromStrings(lines: LineBuffer): Vertex = {
    val line = readLine(lines)
    val vertex = parseVertexFromString(line)
    vertex
  }

  def parsePolygonFromStrings(lines: LineBuffer): Polygon = {
    val vertexCount = parseVertexCountFromStrings(lines)
    val vertices: ArrayBuffer[Vertex] = new ArrayBuffer
    (1 to vertexCount).foreach(_ => vertices.append(parseVertexFromStrings(lines)))
    new Polygon(vertexCount, vertices)
  }

  def parseSilhouetteFromStrings(lines: LineBuffer): Silhouette = {
    val polygonCount: Int = parsePolygonCountFromStrings(lines)
    val polygons: ArrayBuffer[Polygon] = new ArrayBuffer
    (1 to polygonCount).foreach(_ => polygons.append(parsePolygonFromStrings(lines)))
    new Silhouette(polygonCount, polygons)
  }
}