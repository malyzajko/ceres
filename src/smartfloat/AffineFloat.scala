package ceres.smartfloat

import CenterForm._
import ceres.common.DirectedRounding._
import ceres.common.{Interval, NormalInterval, EmptyInterval}
import java.lang.Math.{nextUp}
import scala.Double.{PositiveInfinity => PlusInf, NegativeInfinity => MinusInf}
import scala.math.{ScalaNumericConversions, ScalaNumber}

import scala.language.implicitConversions

//Uses only the affine form to track roundoffs, this is the most basic version
object AffineFloat {

  var opCounts = Map("+" -> 0, "*" -> 0, "divsqrt" -> 0, "trig" -> 0, "other" -> 0)
  def incr(s: String) = {
    opCounts = opCounts + (s -> (opCounts(s) + 1))
  }

  implicit def int2AffineFloat(i : Int): AffineFloat = new AffineFloat(i.toDouble)
  implicit def double2AffineFloat(d : Double): AffineFloat = new AffineFloat(d)

  def apply(d: Double): AffineFloat = {
    if (d != d) new AffineFloat(d, EmptyForm)
    else if (d == PlusInf || d == MinusInf) new AffineFloat(d, FullForm)
    else new AffineFloat(d, new CenterForm(d))
  }

  /*
    Exception handler for comparisons.
   */
  def certainly(b: => Boolean): Boolean = {
    try { b }
    catch { case e: ComparisonUndeterminedException => false }
  }

  def possibly(b: => Boolean): Boolean = {
    try { b }
    catch { case e: ComparisonUndeterminedException => true }
  }


  /**
   * If set to false due to some operation, then some loop or condition guard has failed
   * due to too big errors: i.e. for some of the values, the control would be different.
   */
  var conditionFlag = true

  val E = AffineFloat(math.E)
  val Pi = AffineFloat(math.Pi)

  def sqrt(x: AffineFloat): AffineFloat = {/*incr("divsqrt"); */ new AffineFloat(math.sqrt(x.d), x.aa.squareRoot)}
  def log(x: AffineFloat): AffineFloat = {/*incr("other");*/ new AffineFloat(math.log(x.d), x.aa.ln)}
  def exp(x: AffineFloat): AffineFloat = {/*incr("other");*/ new AffineFloat(math.exp(x.d), x.aa.exponential)}
  def pow(x: AffineFloat, y: AffineFloat): AffineFloat = {/* incr("other") */ new AffineFloat(math.pow(x.d, y.d), (y.aa * x.aa.ln).exponential)}
  def pow2(x: AffineFloat): AffineFloat = x * x
  def pow3(x: AffineFloat): AffineFloat = x * x * x
  def pow4(x: AffineFloat): AffineFloat = x * x * x * x

  def cos(x: AffineFloat): AffineFloat = {/*incr("trig");*/ new AffineFloat(math.cos(x.d), x.aa.cosine)}
  def sin(x: AffineFloat): AffineFloat = {/*incr("trig");*/ new AffineFloat(math.sin(x.d), x.aa.sine)}
  def tan(x: AffineFloat): AffineFloat = {/*incr("trig");*/ new AffineFloat(math.tan(x.d), x.aa.tangent)}
  def acos(x: AffineFloat): AffineFloat = {/*incr("trig");*/ new AffineFloat(math.acos(x.d), x.aa.arccosine)}
  def asin(x: AffineFloat): AffineFloat = {/*incr("trig");*/ new AffineFloat(math.asin(x.d), x.aa.arcsine)}
  def atan(x: AffineFloat): AffineFloat = {/*incr("trig");*/ new AffineFloat(math.atan(x.d), x.aa.arctangent)}
  def random: AffineFloat = new AffineFloat(math.random)  //constant

  //Other functions. We'll skip for now: ceil, floor, signum, round.

  def abs(x: AffineFloat): AffineFloat = {
    if(x.compare(0.0) == 0)
      return new AffineFloat(math.abs(x.d), x.aa.absValue)
    else
      return new AffineFloat(math.abs(x.d), x.aa.absValue)
  }

  /**
   * Returns the bigger of two values.
   * If comparison cannot be decided soundly, double values of AffineFloat will be used.
   */
  def max(x: AffineFloat, y: AffineFloat): AffineFloat = {
    val i = x.compare(y)
    if (i == -1 || i == 0) return y
    else return x
  }

  /**
   * Returns the smaller of two values.
   * If comparison cannot be decided soundly, double values of AffineFloat will be used.
   */
  def min(x: AffineFloat, y: AffineFloat): AffineFloat = {
    val i = x.compare(y)
    if (i == -1 || i == 0) return x
    else return y
  }


  def computeRelErr(f: Double, aa: AffineForm): Double = {
    val a = aa.interval.xlo;  val b = aa.interval.xhi
    val aD = aa.intervalExt._1; val bD = aa.intervalExt._2

    if(math.abs(f) < Double.MinPositiveValue) { //denormalized
      return math.max(f - a, b - f)
    }
    else if (a == b) 0.0
    else {
      val re1 = math.abs(divUp(subUp(f, a), a))
      val re2 = math.abs(divUp(subUp(b, f), b))
      return math.max(re1, re2)
    }
  }
}


class AffineFloat(val d: Double, val aa: AffineForm) extends ScalaNumber
  with ScalaNumericConversions with Ordered[AffineFloat] {
  import AffineFloat._
  import AffineForm._

  assert(aa.interval.contains(d), "affine does not contain value " + d + " " + aa.interval)
  assert(aa.x0 != aa.x0 || d == aa.x0, "d != aa.x0,  d=" + d + "    aa.x0="+aa.x0)

  def this(ff: Double) =
    this(ff, if (ff != ff) EmptyForm
             else if (ff == PlusInf || ff == MinusInf) FullForm
             else new CenterForm(ff))

  def +(y: AffineFloat): AffineFloat = {/*incr("+")*/
    new AffineFloat(this.d + y.d, this.aa + y.aa)}
  def -(y: AffineFloat): AffineFloat = {//incr("+")
    new AffineFloat(this.d - y.d, this.aa - y.aa)}
  def *(y: AffineFloat): AffineFloat = {//incr("*")
    new AffineFloat(this.d * y.d, this.aa * y.aa)}
  def /(y: AffineFloat): AffineFloat = {//incr("divsqrt")
    new AffineFloat(this.d / y.d, this.aa / y.aa)}
  def unary_-(): AffineFloat = {//incr("+")
    new AffineFloat(-this.d, -this.aa)}

  def %(y: AffineFloat): AffineFloat = {
    this - (y * (this/y).toInt)
  }

  // For Java compatibility
  def plus(y: AffineFloat): AffineFloat = new AffineFloat(this.d + y.d, this.aa + y.aa)
  def minus(y: AffineFloat): AffineFloat = new AffineFloat(this.d - y.d, this.aa - y.aa)
  def times(y: AffineFloat): AffineFloat = new AffineFloat(this.d * y.d, this.aa * y.aa)
  def by(y: AffineFloat): AffineFloat = new AffineFloat(this.d / y.d, this.aa / y.aa)
  def unary_minus(): AffineFloat = new AffineFloat(-this.d, -this.aa)
  def mod(y: AffineFloat): AffineFloat = {
    this - (y * (this/y).toInt)
  }


  /**
   * Returns true of the other is a value and within the error tolerance.
   * Note that the comparison flag may be set, since we use the compare method.
   */
  override def equals(other: Any): Boolean = other match {
    case x: AffineFloat => this.compare(x) == 0
    case x: Double => this.compare(x) == 0
    case x: Short => this.compare(x) == 0
    case x: Char => this.compare(x) == 0
    case x: Byte => this.compare(x) == 0
    case x: Int => this.compare(x) == 0
    case x: Float => this.compare(x) == 0
    case x: Long => this.compare(x) == 0
    case _ => false
  }

  override def hashCode(): Int = d.hashCode

  override def toString: String = toStringWithErrors //d.toString

  def toStringWithErrors = {
    var str = d.toString//"%1.16e".format(d)
    aa match {
      case FullForm => str += " (not computable)"
      case EmptyForm => str += " (not computable)"
      case _ => str += "(abs: "+doubleFormat.format(absError)+") (rel: " + doubleFormat.format(relError) + ")"
    }
    str
  }

  def toStringWithIntervals = {
    var str = doubleFormat.format(d)
    aa match {
      case FullForm => str += " (not computable)"
      case EmptyForm => str += " (not computable)"
      case _ => str += " (" + interval + ")"
    }
    str
  }

  def toStringWithAbsErrors = {
    var str = doubleFormat.format(d)
    aa match {
      case FullForm => str += " (not computable)"
      case EmptyForm => str += " (not computable)"
      case _ => str += " (" + doubleFormat.format(aa.radius) + ")"
    }
    str
  }

  def value: Double = d
  def interval: Interval = aa.interval
  def relError: Double = computeRelErr(d, aa)
  def absError: Double = aa.radius

  def compare(y: AffineFloat): Int = {
    var diff = y.aa - this.aa
    val iDiff = diff.interval

    if (iDiff.xlo == 0.0 && iDiff.xhi == 0.0) return 0
    else if (iDiff.xlo > 0.0) return -1 //x is smaller
    else if (iDiff.xhi < 0.0) return 1 //x is bigger

    else {
      if (printComparisonFailure) {
        //comparison has failed: go by the middle value
        conditionFlag = false
        println("comparison failed! " + failMessage + "  uncertainty interval: " + iDiff)
        return java.lang.Double.compare(this.d, y.d)  
      } else {
        throw ComparisonUndeterminedException(this.interval + " and "+ y.interval +" are incomparable.")
      }
      
    }
  }

  override def byteValue(): Byte = Predef.double2Double(d).byteValue
  override def doubleValue(): Double = d
  override def floatValue(): Float = Predef.double2Double(d).floatValue
  override def intValue(): Int = Predef.double2Double(d).intValue
  override def isValidByte: Boolean = false
  override def isValidChar: Boolean = false
  override def isValidInt: Boolean = false
  override def isValidShort: Boolean = false
  override def longValue(): Long = Predef.double2Double(d).longValue
  override def shortValue(): Short = Predef.double2Double(d).shortValue
  override def toByte: Byte = d.toByte
  override def toChar: Char = d.toChar
  override def toDouble: Double = d
  override def toFloat: Float = d.toFloat
  override def toInt: Int = d.toInt
  override def toLong: Long = d.toLong
  override def toShort: Short = d.toShort
  def underlying(): AnyRef = this
  override def isWhole(): Boolean = d % 1.0 == 0.0
}


