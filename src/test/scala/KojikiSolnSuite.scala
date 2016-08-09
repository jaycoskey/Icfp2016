package Icfp2016

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Util._

import ProblemSoln.ProblemSoln
import _root_.ProblemSoln.ProblemSoln._

@RunWith(classOf[JUnitRunner])
class KojikiSolnSuite extends FunSuite {
  test("KojikiSolnSuite: Problem Soln: 01: Parse and toString test") {
    val filename: String = "kojiki/soln/01.soln"
    val lines: LineBuffer = getLinesFromResourceFile(filename)
    val initialText = lines.mkString(newline)
    val problemSoln: ProblemSoln = parseProblemSolnFromStrings(lines)
    val finalText = problemSoln.toString
    assert(initialText == finalText)
  }

  test("KojikiSolnSuite: Problem Soln: Example: Parse and toString test") {
    val filename: String = "kojiki/soln/taskDescriptionExample.soln"
    val lines: LineBuffer = getLinesFromResourceFile(filename)
    val initialText = lines.mkString(newline)
    val problemSoln: ProblemSoln = parseProblemSolnFromStrings(lines)
    val finalText = problemSoln.toString()
    assert(initialText == finalText)
  }
}
