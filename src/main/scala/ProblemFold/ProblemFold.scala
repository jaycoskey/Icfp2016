package ProblemFold
// ============================================================
// NOTE: ENTIRE ProblemFold PACKAGE IS A WORK IN PROGRESS
// ============================================================

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import Icfp2016.Vertex
import Icfp2016.Vertex._

import _root_.ProblemSoln.ProblemSoln
import _root_.ProblemSpec.ProblemSpec
import _root_.ProblemSpec.Skeleton

class ProblemFold(var folds: ArrayBuffer[Fold]
                 , var curGen: Int
                 , var curGenVertexId: Int
                 , var curGenEdgeId: Int
                 , var curGenFacetId: Int
                 , var genVertices: ArrayBuffer[GenVertex]
                 , var genEdges: ArrayBuffer[GenEdge]
                 , var genFacets: ArrayBuffer[GenFacet]
                 , var maxFoldGen: Int
                 , var probSkeleton: Skeleton
                 , var probSpec: ProblemSpec
                 , var probSoln: ProblemSoln)
{
  initialize()

  def appendFold(fold: Fold): Unit = folds.append(fold)

  def addGenEdge(srcId: Int, dstId: Int, et: EdgeType): Unit = {
    val id = curGenEdgeId
    val genEdge = new GenEdge(id, /* born */ curGen, /* died */ -1, srcId, dstId, et)
    genEdges.append(genEdge)
    curGenEdgeId += 1
  }
  def addGenFacet(srcId: Int, vertexIds: ArrayBuffer[Int]): Unit = {
    val id: Int = curGenEdgeId
    val genFacet = new GenFacet(id, /* born */ curGen, /* died */ -1, vertexIds)
    genFacets.append(genFacet)
    curGenEdgeId += 1
  }
  def addGenVertex(v: Vertex): Unit = {
    val id = curGenVertexId
    val stack: mutable.Stack[(Vertex, Int)] = new mutable.Stack
    stack.push((v, id))
    genVertices.append(new GenVertex(id, stack))
    curGenVertexId += 1
  }

  // sealed abstract class IntersectionType(val name: String) extends IT.Value
  // object IT extends Enumeration {
  //   val MidMid
  //   , E1A
  //   , E1B
  //   , E2A
  //   , E2B
  //   = Value
  // }
  def executeFold(fold: Fold): Unit = {
    // Find where mirror intersects edges

    val mirror = fold.edge
    genEdges.foreach(genEdge => {
      val edge: Edge = (getTopVertexById(genEdge.srcId), getTopVertexById(genEdge.dstId))
      val (iType, intersection): (IT.Value, Vertex) = Vertex.edgePairIntersectionInfo(mirror, edge)
      iType match {
        case IT.None => { Unit }
        case IT.Mid_Mid => {
          //
        }
        case IT.E1A_E2A => { Unit }
        case IT.E1A_E2B => { Unit }
        case IT.E1B_E2A => { Unit }
        case IT.E1B_E2B => { Unit }
      }
    })
    // Determine which vertices will be reflected
    //   - Create new GenVertexes @ points of intersection
    //   - Create new GenVertexes @ reflected coords
    // Split edges
    //   - Retire edges that are split
    //   - Create new Edges (EdgeType=Remaining)
    // Split facets
    //   - Retire facets that are split
    //   - Create new Facets
  }

  def executeFolds(): Unit = {
    folds.foreach(fold => executeFold(fold))
    maxFoldGen = curGen
  }

  /**
    * Create new Vertexes and Facets by conceptually running the folds in reverse from maFoldTime
    * Note: curGen still advances
    */
  def executeUnfolds(): Unit = {
    // TODO
  }

  def getTopVertexById(id: Int): Vertex = genVertices(id).gVerts.head._1

  def initialize(): Unit = {
    // Initialization
    folds = new ArrayBuffer
    curGen = 0
    curGenVertexId = 0
    curGenEdgeId = 0
    curGenFacetId = 0

    // Initialize genVertices
    genVertices = new ArrayBuffer[GenVertex]
    val (sw, nw, se, ne): (Vertex, Vertex, Vertex, Vertex) = unitSquareCorners
    val corners = Array(sw, nw, se, ne)
    corners.foreach((v: Vertex) => addGenVertex(v))

    // Initialize genEdges
    genEdges = new ArrayBuffer

    // Initialize genFacets

    maxFoldGen = -1
    probSpec = null
    probSoln = null
  }

  def mkSpecAndSoln(): Unit = {
    executeFolds()
    saveSkeleton()
    executeUnfolds()
    saveProblemSpec()
    saveProblemSoln()
  }

  def saveSkeleton(): Unit = {
    require(folds.isEmpty || (maxFoldGen >= 0 && curGen == maxFoldGen))
  }

  def saveProblemSpec(): Unit = {
    require(folds.isEmpty || curGen > maxFoldGen)
  }

  def saveProblemSoln(): Unit = {
    require(folds.isEmpty || curGen > maxFoldGen)
  }
}
