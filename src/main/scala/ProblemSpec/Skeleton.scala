package ProblemSpec

import Icfp2016.Util._
import Icfp2016.Vertex
import Icfp2016.Vertex._
import _root_.ProblemSpec.ProblemSpec._

import scala.collection.mutable.ArrayBuffer

class Skeleton(val edgeCount: Int, edges: ArrayBuffer[Edge]) {
  override def toString(): String = {
    val lines: ArrayBuffer[String] = new ArrayBuffer
    appendLine(lines, edgeCount.toString)
    edges.foreach(edge => {
      appendLine(
        lines,
        "%s %s".format(edge._1.toString, edge._2.toString)
      )
    })
    lines.mkString
  }
}

object Skeleton {
  def parseEdgeCountFromStrings(lines: ArrayBuffer[String]): Int = readLine(lines).toInt

  def parseEdgeFromStrings(lines: ArrayBuffer[String]): Edge = {
    val line = readLine(lines)
    val vs: Array[Vertex] = line.split(" ").map((v: String) => parseVertexFromString(v))
    new Edge(vs(0), vs(1))
  }

  def parseSkeletonFromStrings(lines: ArrayBuffer[String]): Skeleton = {
    val edgeCount = parseEdgeCountFromStrings(lines)
    val edges: ArrayBuffer[Edge] = new ArrayBuffer
    (1 to edgeCount).foreach(_ => edges.append(parseEdgeFromStrings(lines)))
    new Skeleton(edgeCount, edges)
  }
}