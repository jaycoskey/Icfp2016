package ProblemSoln

import Icfp2016.Util._
import Icfp2016.Vertex
import Icfp2016.Vertex._

import scala.collection.mutable.ArrayBuffer

class DstPosn(val vertexId: Int, val vertex: Vertex) {
  override def toString(): String = vertex.toString
}

object DstPosn {
  def parseDstPosnsFromStrings(dstPosnCount: Int, lines: LineBuffer): ArrayBuffer[DstPosn] = {
    val dstPosns: ArrayBuffer[DstPosn] = new ArrayBuffer
    (0 until dstPosnCount).foreach(vertexId => {
      val vertex = parseVertexFromString(readLine(lines))
      dstPosns.append(new DstPosn(vertexId, vertex))
    })
    dstPosns
  }
}



