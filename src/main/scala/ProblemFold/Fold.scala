package ProblemFold
// ============================================================
// NOTE: ENTIRE ProblemFold PACKAGE IS A WORK IN PROGRESS
// ============================================================

import _root_.Icfp2016.Vertex._

sealed abstract class FoldDirection(val name: String) extends FD.Value
object FD extends Enumeration {
  val Clockwise, Counterclockwise = Value
}

sealed abstract class GenSelected(val name: String) extends GS.Value
object GS extends Enumeration {
  val All, Max, Min = Value
}

/** Means of selecting which portion of the origami to fold.
  * Note: This class might be changed to be made more flexible.
  *
  * @param gen
  * @param genSelected
  */
class FoldSelector(val gen: Int, genSelected: GenSelected) {
  override def toString(): String = genSelected match {
    case GS.All => "gen_all"  // In this case, the value of gen is ignored.
    case GS.Max => "gen" + "<=" + gen.toString()
    case GS.Min => "gen" + ">=" + gen.toString()

  }
}
class Fold(val edge: Edge, val foldDirection: FD.Value, val foldSelector: FoldSelector) {
  override def toString(): String = "Fold %s over (%s, %s) using selector=%s".format(
    foldDirection, edge._1.toString(), edge._2.toString(), foldSelector.toString()
  )
}
