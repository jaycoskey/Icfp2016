package ProblemSoln

import scala.collection.mutable.ArrayBuffer

import Icfp2016.Rational
import Icfp2016.Util._
import Icfp2016.Vertex
import Icfp2016.Vertex._

import _root_.ProblemSoln.DstPosn._
import _root_.ProblemSoln.Facet._
import _root_.ProblemSoln.SrcPosn._

import _root_.ProblemSpec.ProblemSpec

class ProblemSoln(
                   val fileSize: Int
                   , val srcPosns: ArrayBuffer[SrcPosn]
                   , val facets: ArrayBuffer[Facet]
                   , val dstPosns: ArrayBuffer[DstPosn])
{
  def isValid: Boolean = {
    class VTest(val testId: Int, val title: String, val success: Boolean)
    val test1 = new VTest(1, "All src vertices are in the unit square",
      srcPosns.forall(srcPosn => {
        val v = srcPosn.vertex
        v.x.isInUnitInterval && v.y.isInUnitInterval
      }))
    val test2 = new VTest(2, "Source position coords do not repeat",
      srcPosns.distinct.size == srcPosns.size
      )
    val test3 = new VTest(3, "Facet edges have positive length", {
      def facetTest(facet: Facet): Boolean = {
        val facetEdges = facet.vertexIds zip facet.vertexIds.tail
        facetEdges.forall((edge: (Int, Int)) => {
          val v1 = srcPosns(edge._1).vertex
          val v2 = srcPosns(edge._2).vertex
          !(v1 == v2)
        })
      }
      facets.forall(facet => facetTest(facet))
    })
    val test4 = new VTest(4, "No crazy edge crossings",
      false)
    val test5 = new VTest(5, "All facet polygons are simple", {
      def doesEdgeListCross(edges: ArrayBuffer[Edge]): Boolean = {
        edges.indices.exists(i => {
          edges.indices.exists(j => {
            if (i == j) {
              false
            }
            else {
              val (iType, vertex): (IT.Value, Vertex) = edgePairIntersectionInfo(edges(i), edges(j))
              iType == IT.Mid_Mid // Don't count cases where ends meet
            }
          })
        })
      }
      facets.forall(facet => {
        val verts = facet.vertexIds.map(vertexId => srcPosns(vertexId).vertex)
        val edges = verts zip verts.tail
        !doesEdgeListCross(edges)
      })
    })
    val test6 = new VTest(6, "Src and dest related through congruent maps",
      false)
    val test7 = new VTest(7, "Facets intersections have zero area",
      false)
    val test8 = new VTest(8, "Union of src facets covers unit square",{
      def facetArea(facet: Facet): Rational = {
        val verts = facet.vertexIds.map(vertexId => srcPosns(vertexId).vertex)
        Vertex.area(verts)
      }
      val totalArea = Rational.sum(facets.map(facet => facetArea(facet)))
      if (totalArea != Rational.one) {
        println("DEBUG: Total area = %s".format(totalArea.toString()))
      }
      totalArea == Rational.one
    })

    val test9 = new VTest(9, "Solution size is <= 5000 bytes",
      fileSize <= 5000)
    val tests = Array(test1, test2, test3, /* test4, */ test5, /* test6, test7, */ test8, test9)
    val results = tests.map(test => (test.testId, test.title, test.success))
    val failures = results.filter((result: (Int, String, Boolean)) => !result._3)
    if (failures.nonEmpty) {
      println("List of solution validation tests that failed:")
      failures.foreach(failure => println("\t%d: %s".format(failure._1, failure._2)))
      false
    } else {
      true
    }
  }

  override def toString(): String = {
    val lines: LineBuffer = new ArrayBuffer
    appendLine(lines, srcPosns.length.toString())
    Console.flush()
    srcPosns.sortBy(_.vertexId).foreach(srcPosn => appendLine(lines, srcPosn.toString()))
    appendLine(lines, facets.length.toString())
    facets.foreach(facet => appendLine(lines, facet.toString()))
    dstPosns.sortBy(_.vertexId).foreach(dstPosn => appendLine(lines, dstPosn.toString()))
    lines.mkString.dropRight(2) //  init
  }
}

object ProblemSoln {
  /** Find solution, or an approximation thereof.
    * Strategy: Iterate over (some) foldings.  Choose one with highest resemblance.
    * A folding is a choice of a folding edge (chosen from the skeleton) and a direction of fold.
    *
    * @param spec  Problem Specification
    * @return  Problem Solution
    */
  def mkGoodProblemSolnFromProblemSpec(spec: ProblemSpec): ProblemSoln = {
    // TODO: Complete
    val dummyProblemSoln: ProblemSoln = new ProblemSoln(
      0, new ArrayBuffer[SrcPosn](0), new ArrayBuffer[Facet](0), new ArrayBuffer[DstPosn](0)
    )
    dummyProblemSoln
  }

  def parseProblemSolnFromResourceFile(filename: String): ProblemSoln = {
    val lines: LineBuffer = getLinesFromResourceFile(filename)
    parseProblemSolnFromStrings(lines)
  }

  def parseProblemSolnFromStrings(lines: LineBuffer): ProblemSoln = {
    val fileSize = lines.mkString.filter(char => !char.isWhitespace).length
    val srcPosns: ArrayBuffer[SrcPosn] = parseSrcPosnsFromStrings(lines)
    val facets: ArrayBuffer[Facet] = parseFacetsFromStrings(lines)
    val dstPosns: ArrayBuffer[DstPosn] = parseDstPosnsFromStrings(srcPosns.length, lines)
    new ProblemSoln(fileSize, srcPosns, facets, dstPosns)
  }

  def resemblance(spec: ProblemSpec, soln: ProblemSoln): Rational = {
    // TODO: Complete
    Rational.zero
  }

  val trivialProblemSoln: ProblemSoln = {
    val fileSize = -1

    val srcPosns: ArrayBuffer[SrcPosn] = new ArrayBuffer
    val (sw, nw, se, ne) = Vertex.unitSquareCorners
    val indexedCorners: Array[(Vertex, Int)] = Array(sw, se, ne, nw).zipWithIndex
    def mkSrcPosn(vi: (Vertex, Int)) = new SrcPosn(vi._2, vi._1)
    val srcCorners: Array[SrcPosn] = indexedCorners.map(mkSrcPosn)
    srcPosns.appendAll(srcCorners)

    val facets: ArrayBuffer[Facet] = new ArrayBuffer
    val vertexIds: ArrayBuffer[Int] = new ArrayBuffer
    vertexIds.appendAll(Array(0, 1, 2, 3))
    val facet = new Facet(vertexIds)
    facets.append(facet)

    val dstPosns: ArrayBuffer[DstPosn] = new ArrayBuffer
    def mkDstPosn(vi: (Vertex, Int)) = new DstPosn(vi._2, vi._1)
    val dstCorners: Array[DstPosn] = indexedCorners.map(mkDstPosn)
    dstPosns.appendAll(dstCorners)

    val trivialProblemSoln = new ProblemSoln(fileSize, srcPosns, facets, dstPosns)
    trivialProblemSoln
  }
}