package Icfp2016

import java.io
import java.io.File

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Util {
  // ========================================
  // Debug support
  // ========================================
  type LineBuffer = ArrayBuffer[String]

  class RequirementException(msg: String) extends Exception(msg) {}

  def appendLine(lines: ArrayBuffer[String], str: String): Unit = {
    lines.append(str ++ newline)
  }

  def assertWrapper(cond: Boolean, funcName: => String, msg: String) = {
    if (!cond) {
      throw new RequirementException(s"$funcName: $msg")
    }
  }

  def dprint(banner: String, msg: String) = {
    print(banner ++ msg)
    Console.flush()
  }

  def dprintln(banner: String, msg: String) = {
    println(banner ++ msg)
    Console.flush()
  }

  def getLinesFromResourceFile(filename: String): ArrayBuffer[String] = {
    try {
      getLinesFromPath(resourcesDir + filename)
    }
    catch {
      case ex: java.io.FileNotFoundException => {
        println("File not found: %s".format(ex.getMessage))
        sys.exit(1)
      }
    }
  }

  def getLinesFromPath(path: String): ArrayBuffer[String] = {
    val lines: ArrayBuffer[String] = new ArrayBuffer
    lines.appendAll(Source.fromFile(path).getLines())
    lines
  }

  var isVerbose: Boolean = false

  val newline = sys.props("line.separator")

  def readLine(lines: ArrayBuffer[String]): String = {
    require(lines.nonEmpty)
    val line = lines.head
    lines.remove(0)
    line
  }

  val resourcesDir = "src/test/resources/"

  def writeToResourcesFile(filename: String, text: String): Unit = {
    // From the book "Beginning Scala", by David Pollak
    def using[A <: {def close() : Unit}, B](param: A)(f: A => B): B = {
      try {
        f(param)
      } finally {
        param.close()
      }
    }

    try {
      using(new java.io.FileWriter(resourcesDir + filename)) {
        fileWriter => fileWriter.write(text)
      }
    }
    catch {
      case ex: java.io.FileNotFoundException => {
        println("File not found: %s".format(ex.getMessage))
        sys.exit(1)
      }
    }
  }
}