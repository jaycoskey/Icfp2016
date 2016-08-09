package Icfp2016

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Vertex._

@RunWith(classOf[JUnitRunner])
class VertexSuite extends FunSuite {
  test("VertexSuite: Area test") {
    val v1 = Vertex(Rational(0), Rational(0))
    val v2 = Vertex(Rational(1), Rational(0))
    val v3 = Vertex(Rational(2, 3), Rational(1))
    val v4 = Vertex(Rational(1, 3), Rational(1))
    val vs = Array(v1, v2, v3, v4, v1)
    val area = Vertex.area(vs)
    assert(area == Rational(2, 3))
  }

  test("VertexSuite: Edge crossing test (crossing)") {
    val (sw, nw, se, ne): (Vertex, Vertex, Vertex, Vertex) = Vertex.unitSquareCorners
    val edge1: Edge = (nw, se)
    val edge2: Edge = (sw, ne)
    assert(Vertex.doesEdgePairIntersect(edge1, edge2))
  }

  test("VertexSuite: Edge crossing test (no crossing)") {
    val (sw, nw, se, ne): (Vertex, Vertex, Vertex, Vertex) = Vertex.unitSquareCorners
    assert(! Vertex.doesEdgePairIntersect((nw, sw), (ne, se)))
  }

  test("VertexSuite: Edge crossing test (ends meet)") {
    val (sw, nw, se, ne): (Vertex, Vertex, Vertex, Vertex) = Vertex.unitSquareCorners
    assert(Vertex.doesEdgePairIntersect((nw, sw), (nw, ne)))
  }
}
