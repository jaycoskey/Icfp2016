package ProblemSoln

import Icfp2016.Util._
import Icfp2016.Vertex

import scala.collection.mutable.ArrayBuffer

class Facet(val vertexIds: ArrayBuffer[Int]) {
  override def toString(): String = {
    vertexIds.length.toString + " " + vertexIds.map(id => id.toString).mkString(" ")
  }
}

object Facet {
  def parseFacetCountFromStrings(lines: LineBuffer): Int = readLine(lines).toInt

  def parseFacetsFromStrings(lines: LineBuffer): ArrayBuffer[Facet] = {
    val facetCount = parseFacetCountFromStrings(lines)
    val facets: ArrayBuffer[Facet] = new ArrayBuffer
    (1 to facetCount).foreach(_ => {
      val line: String = readLine(lines)
      val vertexIds: ArrayBuffer[Int] = new ArrayBuffer
      vertexIds.appendAll(line.split(" ").map(_.toInt))  // First value is actually a count
      val vertexIdCount = vertexIds.head
      vertexIds.remove(0)
      require(vertexIds.length == vertexIdCount)
      facets.append(new Facet(vertexIds))
    })
    facets
  }
}
