package Icfp2016

// TODO: Complete this.  Use this to invoke the curl command.

class Curl(val foo: Int, val bar: Int)

object Curl {
  val curlPath = raw"C:\Anaconda3\Library\bin\curl.exe"
  val baseArgs = "--compressed -L -H Expect:"
  val submitUrl = "http://2016sv.icfpcontest.org/api/solution/submit"
  // def getSpecCmd(specId: Int)
  val xApiKey = "108-9963664260961f23ab17418507205151"

  def mkSubmitProbCmd(epochTime: Int, solnPath: String): String = {
    val apiKeyHeader = "-H 'X-API-Key: %s".format(xApiKey)
    val publishTimeForm = "-F publish_time=%d".format(epochTime)
    val solnForm = "-F solution_spec=@%s".format(solnPath)
    val cmd = Array(curlPath, baseArgs, apiKeyHeader, publishTimeForm, solnForm, submitUrl).mkString(" ")
    cmd
  }

  def mkSubmitSolnCmd(probId: Int, solnPath: String): String = {
    val apiKeyHeader = "-H 'X-API-Key: %s".format(xApiKey)
    val probIdForm = "-F problem_id=%d".format(probId)
    val solnForm = "-F solution_spec=@%s".format(solnPath)
    val cmd = Array(curlPath, baseArgs, apiKeyHeader, probIdForm, solnForm, submitUrl).mkString(" ")
    cmd
  }
}
