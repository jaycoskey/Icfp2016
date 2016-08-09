package Icfp2016

class Rational(var numer: BigInt, var denom: BigInt = 1) {
  val gcd: BigInt = Rational.gcd(numer, denom)
  numer = numer / gcd
  denom = denom / gcd
  if (denom < 0) {
    numer *= -1
    denom *= -1
  }

  def * (b: Rational): Rational = Rational(numer * b.numer, denom * b.denom)

  def * (v: Vector): Vector = {
    val xn: BigInt = numer * v.x.numer
    val xd: BigInt = denom * v.x.denom
    val yn: BigInt = numer * v.y.numer
    val yd: BigInt = denom * v.y.denom
    Vector(Rational(xn, xd), Rational(yn, yd))
  }

  def + (b: Rational): Rational = {
    val lcm = Rational.lcm(denom, b.denom)
    Rational(numer * (lcm/denom) + b.numer * (lcm/b.denom), lcm)
  }

  def unary_- = Rational(-numer, -denom)

  def - (b: Rational): Rational = {
    val lcm = Rational.lcm(denom, b.denom)
    Rational(numer * (lcm/denom) - b.numer * (lcm/b.denom), lcm)
  }

  def / (b: Rational): Rational = {
    // require(b.numer != 0)
    Rational(numer * b.denom, denom * b.numer)
  }

  def < (b: Rational): Boolean = numer * b.denom < denom * b.numer

  def <= (b: Rational): Boolean = this.<(b) || this.==(b)

  def > (b: Rational): Boolean = b.<(this)

  def >= (b: Rational): Boolean = b.<=(this)

  // Assume that Rational has been reduced, and denom > 0.
  override def equals (b: Any): Boolean = b match {
    case b: Rational => numer == b.numer && denom == b.denom
    case _ => false
  }
  override def hashCode: Int = {
    val largePrime: BigInt = 15485863  // millionth prime #
    val smallPrime: Int = 11
    (denom % largePrime).toInt + smallPrime * (numer % largePrime).toInt
  }

  def isInUnitInterval(): Boolean = {
    val result: Boolean = numer >= 0 && numer <= denom
    // if (!result) { println("No in unit interval: (%d, %d)".format(numer, denom)) }
    result
  }

  override def toString(): String = {
    if (denom == 1) { numer.toString }
    else { "%d/%d".format(numer, denom) }
  }
}

object Rational {
  val half = Rational(1, 2)
  val two = Rational(2, 1)
  val zero = Rational(0)

  def apply(numer: BigInt, denom: BigInt = 1) = {
    new Rational(numer, denom)
  }

  def parseFromString(str: String): Rational = {
    val parts: Array[String] = str.split("/")
    if (parts.length == 1) {
      Rational(BigInt(str), 1)
    } else {
      if(parts.length != 2) {
        throw new IllegalArgumentException("error in format of Rational: \"%s\"".format(str))
      }
      val numer = BigInt(parts(0))
      val denom = BigInt(parts(1))
      Rational(numer, denom)
    }
  }

  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)

  def lcm(a: BigInt, b: BigInt): BigInt = (a * b).abs / gcd(a, b)

  def max(r1: Rational, r2: Rational): Rational = if (r1 > r2) r1 else r2

  def min(r1: Rational, r2: Rational): Rational = if (r1 < r2) r1 else r2

  val one = Rational(1, 1)

  def sum(rats: Traversable[Rational]): Rational = {
    rats.fold(Rational(0, 1))(_+_)
  }
}