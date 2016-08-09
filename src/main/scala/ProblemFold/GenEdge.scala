package ProblemFold
// ============================================================
// NOTE: ENTIRE ProblemFold PACKAGE IS A WORK IN PROGRESS
// ============================================================

sealed abstract class EdgeType(val name: String) extends ET.Value
object ET extends Enumeration {
  val NewCrease  // Edge is new as of denoted generation #
    , Reflected  // Edge previously existed, but at different location.
    , Remaining  // Parent edge was split into two at specified generation
    = Value
}

class GenEdge(val id: Int
             , val genBorn: Int
             , val genDied: Int
             , val srcId: Int
             , val dstId: Int
             , val edgeType: EdgeType)


