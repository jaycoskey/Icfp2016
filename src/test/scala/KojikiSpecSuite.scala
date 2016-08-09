package Icfp2016

import scala.collection.mutable.HashMap

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import Util._

import ProblemSpec.ProblemSpec
import _root_.ProblemSpec.ProblemSpec._

@RunWith(classOf[JUnitRunner])
class KojikiSpecSuite extends FunSuite {
  val successStr = "success"

  test("KojikiSpecSuite: Problem Spec: Example: Parse and toString test") {
    val filename: String = "kojiki/spec/taskDescriptionExample.spec"
    val lines: LineBuffer = getLinesFromResourceFile(filename)
    val initialText = lines.mkString(newline)
    val problemSpec: ProblemSpec = parseProblemSpecFromStrings(lines)
    val finalText = problemSpec.toString()
    // println("DEBUG: Initial text:\n%s".format(initialText))
    // println("DEBUG: Final text:\n%s".format(finalText))
    // println("DEBUG: Delta: <<%s>>".format(initialText diff finalText))
    assert(initialText == finalText)
  }

  def getTestDigest(results: HashMap[String, String]): (Int, Int) = {
    val successCount = results.values.filter(result => result == successStr).size
    val failureCount = results.values.filter(result => result != successStr).size
    (successCount, failureCount)
  }

  def getTestResults(): HashMap[String, String] = {
    var results: HashMap[String, String] = new HashMap
    val baseDir = "kojiki/spec/"
    println("")
    (1 to 101).foreach(n => {
      print(".")
      val testId = "%02d".format(n)
      var result = ""
      try {
        val filename = "%s%s.spec".format(baseDir, testId)
        val lines: LineBuffer = getLinesFromResourceFile(filename)
        val initialText = lines.mkString(newline)
        val problemSpec: ProblemSpec = parseProblemSpecFromStrings(lines)
        val finalText = problemSpec.toString()
        if (initialText == finalText) {
          result = successStr
        } else {
          result = "(initialText, finalText) lengths = (%d, %d)"
            .format(initialText.length, finalText.length)
        }
      }
      catch {
        case ex: Exception => { result = "msg: " + ex.getMessage() }
        case _: Throwable => { result = "non-Exception Throwable" }
      }
      results(testId) = result
    })
    println("")
    results
  }

  test("KojikiSpecSuite: Lightning specs: Parse and toString test") {
    val results: HashMap[String, String] = getTestResults()
    val (successCount, failureCount) = getTestDigest(results)
    println("Test results(%d): successes=%d; failures=%d"
      .format(successCount + failureCount, successCount, failureCount)
    )
    val success = failureCount == 0
    if (!success) {
      val sortFunc = (a: String, b: String) => a.toInt < b.toInt
      val failureIds = results.keys.toSeq
        .filter(_ != successStr)
        .sortWith(sortFunc)
        .foreach(testId => println("\t%s: %s: ".format(testId, results(testId)))
      )
    }
    assert(success)
  }
}
