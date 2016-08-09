package Icfp2016

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RationalSuite extends FunSuite {
  test("RationalSuite: Addition test") {
    val a = Rational(5, 6)
    val b = Rational(7, 9)
    val c = Rational(29, 18)
    assert(a + b == c)
  }
}
