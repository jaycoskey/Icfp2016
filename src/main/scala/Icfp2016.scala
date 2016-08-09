package Icfp2016

import sys.process._

import ProblemSoln._
import ProblemSpec._
import Util._

object Icfp2016 {
  def main(args: Array[String]) = {
    // println("Hello, Origammi")
    val spec: ProblemSpec = ProblemSpec.trivialProblemSpec
    val specFilename: String = "kojiki/spec/trivial.spec"

    val soln: ProblemSoln = ProblemSoln.trivialProblemSoln
    val solnFilename: String = "kojiki/soln/trivial.soln"

    println("DEBUG: Problem spec:\n%s".format(spec))
    println("DEBUG: Problem soln:\n%s".format(soln))

    if (soln.isValid) {
      println("No solution validity test failures")
      writeToResourcesFile(solnFilename, soln.toString())
      val epochTime = 1470610800
      val cmd = Curl.mkSubmitProbCmd(epochTime, Util.resourcesDir + solnFilename)
      val cmdOutput = cmd!!
    } else {
      println("Error: Solution validity test failures found")
    }
  }
}
