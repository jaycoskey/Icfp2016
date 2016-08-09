package Icfp2016

import scala.collection.mutable.ArrayBuffer

/**
  */
class Vector(val x: Rational, val y: Rational) {
  def * (r: Rational): Vector = Vector(r * x, r * y)

  def dot(other: Vector): Rational = x * other.x + y * other.y
}

object Vector {
  def apply(x: Rational, y: Rational) = {
    new Vector(x, y)
  }
}

/**
  */
class Vertex(val x: Rational, val y: Rational) {
  def + (vector: Vector): Vertex = Vertex(x + vector.x, y + vector.y)
  def - (vector: Vector): Vertex = Vertex(x - vector.x, y - vector.y)

  def - (vertex: Vertex): Vector = Vector(x - vertex.x, y - vertex.y)

  override def equals(other: Any): Boolean = {
    other match {
      case o: Vertex => x == o.x && y == o.y
      case _ => false
    }
  }
  override def hashCode: Int = {
    val prime: Int = 13
    x.hashCode + prime * y.hashCode
  }

  override def toString(): String = {
    "%s,%s".format(x.toString, y.toString)
  }
}

object Vertex {
  type Edge = (Vertex, Vertex)

  sealed abstract class IntersectionType(val name: String) extends IT.Value

  object IT extends Enumeration {
    val None
    , Mid_Mid
    , E1A_E2A
    , E1A_E2B
    , E1B_E2A
    , E1B_E2B
    = Value
  }

  def apply(x: Rational, y: Rational) = {
    new Vertex(x, y)
  }

  def area(vertices: ArrayBuffer[Vertex]): Rational = {
    val vertexLoop = vertices
    vertexLoop.append(vertices.head)
    val vertexPairs = vertexLoop zip vertexLoop.tail
    val rs = vertexPairs.map(vertexPair => {
      val v1 = vertexPair._1
      val v2 = vertexPair._2
      v1.x * v2.y - v1.y * v2.x
    })
    Rational.half * Rational.sum(rs)
  }

  /**
    * Note: Adapted from: http://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/
    */
  def doesEdgePairIntersect(edge1: Edge, edge2: Edge): Boolean = {
    // println("DEBUG: Entering doesEdgePairIntersect: edge1=[(%s), (%s)], edge2=[(%s), (%s)]".format(
    //   edge1._1.toString(), edge1._2.toString(), edge2._1.toString(), edge2._2.toString()
    // ))
    // In the referenced web page, this was called "onSegment".
    def _inOrthobox(p: Vertex, q: Vertex, r: Vertex): Boolean = {
      // println("\tDEBUG: Entering _inOrthobox: p=(%s), q=(%s), r=(%s)".format(p.toString(), q.toString(), r.toString()))
      if (q.x <= Rational.max(p.x, r.x)
        && q.x >= Rational.min(p.x, r.x)
        && q.y <= Rational.max(p.y, r.y)
        && q.y >= Rational.min(p.y, r.y))
        {
          // println("\t\tDEBUG: _inOrthobox=true")
          true
        }
      else {
        // println("\t\tDEBUG: _inOrthobox=false")
        false
      }
    }
    // To find orientation of ordered triplet (p, q, r).
    // The function returns following values
    // 0 --> p, q and r are collinear
    // 1 --> Clockwise
    // 2 --> Counterclockwise
    def _orientation(p: Vertex, q: Vertex, r: Vertex): Int = {
      // println("\tDEBUG: Entering _orientation: p=(%s), q=(%s), r=(%s)".format(p.toString(), q.toString(), r.toString()))
      val tmp: Rational = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)
      if (tmp == Rational.zero) {
        // println("\t\tDEBUG: _orientation=%s".format("collinear"))
        0
      } // collinear, i.e., slope_of_a-b == slope_of_p-q
      else {
        // println("\t\tDEBUG: _orientation=%s".format("non-collinear"))
        if (tmp > Rational.zero) 1 // p-q-r turns clockwise
        else 2 // p-q-r turns counterclockwise
      }
    }

    // Returns true iff the line segment 'a->b' intersects 'p->q'.
    def _doIntersect(a: Vertex, b: Vertex, p: Vertex, q: Vertex): Boolean = {
      // println("\tDEBUG: Entering _doIntersect: a=(%s), b=(%s), p=(%s), q=(%s)".format(
      //   a.toString(), b.toString(), p.toString(), q.toString()))
      // Find the four orientations needed for general and special cases
      val o_sans_a: Int = _orientation(p, q, b)
      val o_sans_b: Int = _orientation(p, q, a)
      val o_sans_p: Int = _orientation(a, b, q)
      val o_sans_q: Int = _orientation(a, b, p)

      // General case
      if (o_sans_a != o_sans_b && o_sans_p != o_sans_q) {
        // println("\t\tDEBUG: _doIntersect: true: Case #1")
        true
      }
      // ====================
      // Special Cases
      // ====================
      // p-q-b are collinear and b lies on segment p-q
      else
      if (o_sans_a == 0 && _inOrthobox(p, b, q)) {
        // println("\t\tDEBUG: _doIntersect: true: Case #2")
        true
      }
      else
      // p-q-a are collinear and a lies on segment p-q
      if (o_sans_b == 0 && _inOrthobox(p, a, q)) {
        // println("\t\tDEBUG: _doIntersect: true: Case #3")
        true
      }
      else
      // a-b-q are collinear and q lies on segment a-b
      if (o_sans_p == 0 && _inOrthobox(a, q, b)) {
        // println("\t\tDEBUG: _doIntersect: true: Case #4")
        true
      }
      else
      // a-b-p are collinear and p lies on segment a-b
      if (o_sans_q == 0 && _inOrthobox(a, p, b)) {
        // println("\t\tDEBUG: _doIntersect: true: Case #5")
        true
      }
      else {
        // println("\t\tDEBUG: _doIntersect: false")
        false  // Doesn't fall in any of the above cases
      }
    }
    _doIntersect(edge1._1, edge1._2, edge2._1, edge2._2)
  }

  /*
   *
   */
  def edgePairIntersectionInfo(edge1: Edge, edge2: Edge): (IT.Value, Vertex) = {
    if (!doesEdgePairIntersect(edge1, edge2)) {
      val dummyIntersection = Vertex(Rational.zero, Rational.zero)
      (IT.None, dummyIntersection)
    } else {
      if (edge1._1 == edge2._1) (IT.E1A_E2A, edge1._1)
      else if (edge1._1 == edge2._2) (IT.E1A_E2B, edge1._1)
      else if (edge1._2 == edge2._1) (IT.E1B_E2A, edge1._2)
      else if (edge1._2 == edge2._2) (IT.E1B_E2B, edge1._2)
      else throw new IllegalStateException("Inconsistent intersection info")
    }
  }

  def parseVertexFromString(str: String): Vertex = {
    val rs: Array[Rational] = str.split(",").map(r => Rational.parseFromString(r))
    new Vertex(rs(0), rs(1))
  }

  def reflectAcrossEdge(edge: Edge, p: Vertex): Vertex = {
    val mirror: Vector = edge._2 - edge._1
    def rot90(v: Vector) = Vector(-v.y, v.x)
    val normal: Vector = rot90(mirror)
    val tmp: Vector = p - edge._1
    val scale: Rational = tmp.dot(normal) / normal.dot(normal)
    val reflected: Vertex = p - Rational.two * scale * tmp
    reflected
  }

  val unitSquareCorners: (Vertex, Vertex, Vertex, Vertex) = {
    val sw = Vertex(Rational(0), Rational(0))
    val nw = Vertex(Rational(0), Rational(1))
    val se = Vertex(Rational(1), Rational(0))
    val ne = Vertex(Rational(1), Rational(1))
    (sw, nw, se, ne)
  }
}
