package ceres.common

import scala.math.{ScalaNumericConversions, ScalaNumber}
import java.math.{BigInteger, BigDecimal}

/*   TODO
  - investigate why 4.45 is not translated into 445/100 but some huge numbers

*/


object Rational {

  case class RationalCannotBeCastToIntException(s: String) extends Exception

  private val one = new BigInt(new BigInteger("1"))
  private val zero = new BigInt(new BigInteger("0"))
  private val two = new BigInt(new BigInteger("2"))

  /*
    Constructors for rationals.
  */
  def apply(dbl: Double): Rational = {
    val (n, d) = double2Fraction(dbl)
    Rational(n, d)
  }

  def apply(n: Int): Rational = Rational(new BigInt(new BigInteger(n.toString)), one)
 
  def apply(n: Long, d: Long): Rational = {
    /*if (n == 0 && d == 0) Rational(0)
    val (num, den) = formatFraction(new BigInt(new BigInteger(n.toString)), new BigInt(new BigInteger(d.toString)))
    if(den < 0) new Rational(-num, -den)
    else new Rational(num, den)
    */
    Rational(new BigInt(new BigInteger(n.toString)), new BigInt(new BigInteger(d.toString)))
  }

  // This constructor does all the canonicalisation and only this constructor
  //should actually create the objects.
  def apply(n: BigInt, d: BigInt): Rational = {
    // 0/0 -> 0/1
    if (n == 0 && d == 0) {
      new Rational(zero, one)
    }
    else if (d == 0) {
      throw new Exception("Zero denominator is not allowed.")
      null
    }
    else {
      // reduce fraction
      val (num, den) = formatFraction(n, d)
      // only nominator can be negative
      if(den < 0) new Rational(-num, -den)
      else new Rational(num, den)
    }
  }

  // How to get the different parts is taken from the Java API documentation
  // of longBitsToDouble
  def double2Fraction(double: Double): (BigInt, BigInt) = {
    val bits: Long = java.lang.Double.doubleToLongBits(double)
    //println("bits: \n" + java.lang.Long.toBinaryString(bits))

    if (bits == 0x7ff0000000000000L)
      throw new ArithmeticException("cannot convert +infinity to a fraction")
    if (bits == 0xfff0000000000000L)
      throw new ArithmeticException("cannot convert -infinity to a fraction")
    if (bits == 0x7ff8000000000000L)
      throw new ArithmeticException("cannot convert NaN to a fraction")
    
    val sign = if ((bits >> 63) == 0) 1 else -1
    //println("sign: " + sign)

    val exponent = ((bits >> 52) & 0x7ffL).toInt
    //println("exponent: " + java.lang.Long.toBinaryString(exponent))
    //println("exponent: " + exponent)
    //println("true exponent: " + (exponent - 1023))

    val mantissa =
      if (exponent == 0) (bits & 0xfffffffffffffL) << 1
      else (bits & 0xfffffffffffffL) | 0x10000000000000L
    //println("mantissa: " + java.lang.Long.toBinaryString(mantissa))

    val trueExponent = exponent - 1075
    
    val result = if (trueExponent > 0) (two.pow(trueExponent) * mantissa * sign, one)
    else (new BigInt(new BigInteger((sign * mantissa).toString)), two.pow(-trueExponent))
    //println(result)
    //println(result._1.toDouble / result._2.toDouble)
    result
  }


  val MIN_INT_RATIONAL = 1.0 / Int.MaxValue

  /**
    Creates a new rational number where the nominator and denominator consists
    of 32 bit integers. The returned number is bigger than the original.
    This will throw and exception if the number is too large to fit into an Int.
  */
  def scaleToIntsUp(r: Rational): Rational = {
    //println("\n------------------")
    //println("Original: " + r.toDouble)
    
    // Too large
    if (math.abs(r.toDouble) > Int.MaxValue) {
      //println("too big")
      throw new RationalCannotBeCastToIntException(
        "Rational too big to be cast to integer rational.")
    }
    // Too small
    if (math.abs(r.toDouble) < MIN_INT_RATIONAL) {
      //println("too small")
      //throw new RationalCannotBeCastToIntException(
      //  "Rational too big to be cast to integer rational.")
      
      // Underflow
      if (r < Rational(0)) return Rational(0l, 1l)
      else return Rational(1l, Int.MaxValue.toLong)
    }
    val num = r.n
    val den = r.d
   
    // Already small enough
    if (num.abs < Int.MaxValue && den < Int.MaxValue) {
      //println("already good")
      return r
    }
    //println("n: " + num.toString + "   bitlength: " + num.bitLength)
    //println("d: " + den.toString + "   bitlength: " + den.bitLength)
 
    val divN = if (num.bitLength < 32) one else two.pow(num.bitLength - 31)
    val divD = if (den.bitLength < 32) one else two.pow(den.bitLength - 31)
    val div = divN.max(divD)

    var nn = num / div
    //println("nn: " + nn.toString + "      valid Int? " + nn.isValidInt)

    var dd = den / div
    //println("dd: " + dd.toString + "      valid Int? " + dd.isValidInt)
    //println("initial diff: " + (r.toDouble - nn.toDouble/dd.toDouble))
    //println((nn.toDouble/dd.toDouble) < r.toDouble)

    var i = 0
    while ((nn.toDouble/dd.toDouble) < r.toDouble && i < 10000) {
      if (nn < Int.MaxValue)
        nn = nn + 1
      else
        dd = dd - 1
      //println(nn + " ---- " + (nn.toDouble/dd.toDouble))
      i = i + 1
    }
    /*println("nn: " + nn.toString + "      valid Int? " + nn.isValidInt)
    println("dd: " + dd.toString + "      valid Int? " + dd.isValidInt)
    println("final diff:   " + (r.toDouble - nn.toDouble/dd.toDouble))
    println("produced: " + nn.toDouble/dd.toDouble)
    */
    return Rational(nn, dd)
  }
  
  /**
    Creates a new rational number where the nominator and denominator consists
    of 32 bit integers. The number will be smaller than the original.
  */
  def scaleToIntsDown(r: Rational): Rational = {
    //println("\n------------------\n Original: " + r.toDouble)
    
    // Too large
    if (math.abs(r.toDouble) > Int.MaxValue) {
      //println("too big")
      throw new RationalCannotBeCastToIntException(
        "Rational too big to be cast to integer rational.")
    }
    // Too small
    if (math.abs(r.toDouble) < MIN_INT_RATIONAL) {
      //println("too small")
      //throw new RationalCannotBeCastToIntException(
      //  "Rational too big to be cast to integer rational.")
      // Underflow
      if (r < Rational(0)) return Rational(-1l, Int.MaxValue.toLong) 
      else return Rational(0.0)
    }
    val num = r.n
    val den = r.d
   
    // Already small enough
    if (num.abs < Int.MaxValue && den < Int.MaxValue) {
      //println("already good")
      return r
    }
    //println("n: " + num.toString + "   bitlength: " + num.bitLength)
    //println("d: " + den.toString + "   bitlength: " + den.bitLength)   
 
    val divN = if (num.bitLength < 32) one else two.pow(num.bitLength - 31)
    val divD = if (den.bitLength < 32) one else two.pow(den.bitLength - 31)
    val div = divN.max(divD)

    var nn = num / div
    //println("nn: " + nn.toString + "      valid Int? " + nn.isValidInt)

    var dd = den / div
    //println("dd: " + dd.toString + "      valid Int? " + dd.isValidInt)
    //println("initial diff: " + (r.toDouble - nn.toDouble/dd.toDouble))

    var i = 0
    while ((nn.toDouble/dd.toDouble) > r.toDouble && i < 10000) {
      if (nn > Int.MinValue) nn = nn - 1
      else dd = dd + 1
      i = i + 1
    }

    //println("final diff:   " + (r.toDouble - nn.toDouble/dd.toDouble))
    //println("produced: " + nn.toDouble/dd.toDouble)

    return Rational(nn, dd)
  }


  def abs(r: Rational): Rational = {
    if (r.n < 0) return new Rational(-r.n, r.d)
    else return r
  }

  def max(x: Rational, y: Rational): Rational = {
    if (x >= y) return x
    else return y
  }

  def min(x: Rational, y: Rational): Rational = {
    if (x < y) return x
    else return y
  }

  private def formatFraction(n: BigInt, d: BigInt): (BigInt, BigInt) = {
    val g = gcd(n.abs, d.abs)
    (n / g, d / g)
  }

  private def gcd(a: BigInt, b: BigInt): BigInt =
    if (b == 0) a else gcd(b, a % b)

  val MAX_DOUBLE = new BigInt(new BigInteger(Double.MaxValue.toInt.toString))
  //private val MIN_DOUBLE = new BigInt(new BigInteger(Double.MinValue))
}

/**
 * Rational number class.
 * The constructor is private so that only rationals in canonical form can be
 * created.
 */
class Rational private(val n: BigInt, val d: BigInt) extends ScalaNumber with ScalaNumericConversions
  with Ordered[Rational] {
  import Rational._
  assert(d > 0, "Rational denominator negative! " + d)  // number can be negative only through nominator
  assert(math.abs(gcd(n, d).toLong) == 1, "Rational not reduced %d / %d!".format(n, d))  // fraction is reduced

  def unary_-(): Rational = Rational(-n, d)
  def +(other: Rational): Rational = Rational(n * other.d + other.n * d, d * other.d)
  def -(other: Rational): Rational = Rational(n * other.d - other.n * d, d * other.d)
  def *(other: Rational): Rational = Rational(n * other.n, d * other.d)
  def /(other: Rational): Rational = Rational(n * other.d, d* other.n)

  override def toString: String = {
    niceDoubleString(this.toDouble)//"%.16g".format(this.toDouble)
  }
  def toFractionString: String = "(%s)/(%s)".format(n.toString, d.toString)

  def integerPart: Int = doubleValue.toInt

  def compare(other: Rational): Int = {
    val xNom = this.n * other.d
    val yNom = other.n * this.d
    val denom = this.d * other.d
    return xNom.compare(yNom)
  }

  override def equals(other: Any): Boolean = other match {
    case x: Rational => this.compare(x) == 0
    case x: Double => this.toDouble == x  // Fixme: not ideal
    case x: Short => this.compare(Rational(x)) == 0
    case x: Char => this.compare(Rational(x)) == 0
    case x: Byte => this.compare(Rational(x)) == 0
    case x: Int => this.compare(Rational(x)) == 0
    case x: Float => this.toFloat == x
    case x: Long => this.toLong == x
    case _ => false
  }

  override def byteValue(): Byte = Predef.double2Double(doubleValue).byteValue
  override def doubleValue(): Double = {
    // It seems to work with just double conversion before anyway.
    //println("\n" + (new BigDecimal(n.bigInteger).divide(new BigDecimal(d.bigInteger), 300, BigDecimal.ROUND_HALF_EVEN)).doubleValue)
    //println(n.toDouble / d.toDouble)
    n.toDouble / d.toDouble
  }
  override def floatValue(): Float = Predef.double2Double(doubleValue).floatValue
  override def intValue(): Int = Predef.double2Double(doubleValue).intValue
  override def isValidByte: Boolean = false
  override def isValidChar: Boolean = false
  override def isValidInt: Boolean = false
  override def isValidShort: Boolean = false
  override def longValue(): Long = Predef.double2Double(doubleValue).longValue
  override def shortValue(): Short = Predef.double2Double(doubleValue).shortValue
  override def toByte: Byte = doubleValue.toByte
  override def toChar: Char = doubleValue.toChar
  override def toDouble: Double = doubleValue
  override def toFloat: Float = doubleValue.toFloat
  override def toInt: Int = doubleValue.toInt
  override def toLong: Long = doubleValue.toLong
  override def toShort: Short = doubleValue.toShort
  def underlying(): AnyRef = this
  override def isWhole(): Boolean = d == 1.0
}
