package ProblemSoln

import scala.collection.mutable.ArrayBuffer

import Icfp2016.Util._
import Icfp2016.Vertex
import Icfp2016.Vertex._

class SrcPosn(val vertexId: Int, val vertex: Vertex) {
  override def equals(other: Any): Boolean = {
    other match {
      case o: SrcPosn => vertex == o.vertex  // Note: Ignore vertexId
      case _ => false
    }
  }
  override def hashCode: Int = {
    vertex.hashCode  //  Note: Ignore vertexId
  }

  override def toString(): String = vertex.toString
}

object SrcPosn {
  def getSrcPosnCountFromStrings(lines: LineBuffer): Int = readLine(lines).toInt

  def parseSrcPosnsFromStrings(lines: LineBuffer): ArrayBuffer[SrcPosn] = {
    val srcPosnCount = getSrcPosnCountFromStrings(lines)
    val srcPosns: ArrayBuffer[SrcPosn] = new ArrayBuffer
    (0 until srcPosnCount).foreach(vertexId => {
      val vertex = parseVertexFromString(readLine(lines))
      srcPosns.append(new SrcPosn(vertexId, vertex))
    })
    srcPosns
  }

  val srcPosnSortFunc = (a: SrcPosn, b: SrcPosn) => {
    val av = a.vertex
    val bv = b.vertex
    val result = av.x < bv.x || (av.x == bv.x && av.y < bv.y)
    result
  }
}