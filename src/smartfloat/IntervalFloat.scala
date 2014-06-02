package ceres.smartfloat

import ceres.common.DirectedRounding._
import ceres.common.{Interval, NormalInterval, EmptyInterval}

import java.lang.Math.{nextUp}
import scala.math.{ScalaNumericConversions, ScalaNumber}

import scala.language.implicitConversions

object IntervalFloat {

  implicit def double2IntervalFloat(d : Double): IntervalFloat = new IntervalFloat(d)

  //variables
  def apply(d: Double): IntervalFloat = new IntervalFloat(d)
  def apply(d: Double, u: Double): IntervalFloat = new IntervalFloat(d, u)

  /**
   * If set to false due to some operation, then some loop or condition guard has failed
   * due to too big errors: i.e. for some of the values, the control would be different.
   */
  var conditionFlag = true

  val E = IntervalFloat(math.E)
  val Pi = IntervalFloat(math.Pi)

  def sqrt(x: IntervalFloat): IntervalFloat = new IntervalFloat(math.sqrt(x.d), x.aa.squareRoot)
  def log(x: IntervalFloat): IntervalFloat = new IntervalFloat(math.log(x.d), x.aa.ln)
  def exp(x: IntervalFloat): IntervalFloat = new IntervalFloat(math.exp(x.d), x.aa.exponential)
  def pow(x: IntervalFloat, y: IntervalFloat): IntervalFloat =
    new IntervalFloat(math.pow(x.d, y.d), (y.aa * x.aa.ln).exponential)
  def pow2(x: IntervalFloat): IntervalFloat = x * x
  def pow3(x: IntervalFloat): IntervalFloat = x * x * x
  def pow4(x: IntervalFloat): IntervalFloat = x * x * x * x


  def cos(x: IntervalFloat): IntervalFloat = new IntervalFloat(math.cos(x.d), x.aa.cosine)
  def sin(x: IntervalFloat): IntervalFloat = new IntervalFloat(math.sin(x.d), x.aa.sine)
  def tan(x: IntervalFloat): IntervalFloat = new IntervalFloat(math.tan(x.d), x.aa.tangent)
  def acos(x: IntervalFloat): IntervalFloat = new IntervalFloat(math.acos(x.d), x.aa.arccosine)
  def asin(x: IntervalFloat): IntervalFloat = new IntervalFloat(math.asin(x.d), x.aa.arcsine)
  def atan(x: IntervalFloat): IntervalFloat = new IntervalFloat(math.atan(x.d), x.aa.arctangent)
  def random: IntervalFloat = new IntervalFloat(math.random)  //constant

  //Other functions. We'll skip for now: ceil, floor, signum, round.

  def abs(x: IntervalFloat): IntervalFloat = {
    if(x.compare(0.0) == 0) //to trigger check
      return new IntervalFloat(math.abs(x.d), x.aa.absValue)
    else
      return new IntervalFloat(math.abs(x.d), x.aa.absValue)
  }

  /**
   * Returns the bigger of two values.
   * If comparison cannot be decided soundly, double values of IntervalFloat will be used.
   */
  def max(x: IntervalFloat, y: IntervalFloat): IntervalFloat = {
    val i = x.compare(y)
    if (i == -1 || i == 0) return y
    else return x
  }

  /**
   * Returns the smaller of two values.
   * If comparison cannot be decided soundly, double values of IntervalFloat will be used.
   */
  def min(x: IntervalFloat, y: IntervalFloat): IntervalFloat = {
    val i = x.compare(y)
    if (i == -1 || i == 0) return x
    else return y
  }

}



class IntervalFloat(val d: Double, val aa: Interval) extends ScalaNumber with ScalaNumericConversions
  with Ordered[IntervalFloat] {
  import IntervalFloat._
  import AffineForm._

  assert(aa.contains(d), "affine does not contain value " + d + " " + aa)

  def this(ff: Double) = this(ff, Interval(ff))
  def this(ff: Double, un: Double) =
    this(ff, Interval(subDown(ff, un), addUp(ff, un)))


  def +(y: IntervalFloat): IntervalFloat = new IntervalFloat(this.d + y.d, this.aa + y.aa)
  def -(y: IntervalFloat): IntervalFloat = new IntervalFloat(this.d - y.d, this.aa - y.aa)
  def *(y: IntervalFloat): IntervalFloat = new IntervalFloat(this.d * y.d, this.aa * y.aa)
  def /(y: IntervalFloat): IntervalFloat = new IntervalFloat(this.d / y.d, this.aa / y.aa)
  def unary_-(): IntervalFloat = new IntervalFloat(-this.d, -this.aa)

  def %(y: IntervalFloat): IntervalFloat = {
    this - (y * (this/y).toInt)
  }



  /**
   * Returns true if the other is a value and within the error tolerance.
   * Note that the comparison flag may be set, since we use the compare method.
   */
  override def equals(other: Any): Boolean = other match {
    case x: IntervalFloat => this.compare(x) == 0
    case x: Double => this.compare(x) == 0
    case x: Short => this.compare(x) == 0
    case x: Char => this.compare(x) == 0
    case x: Byte => this.compare(x) == 0
    case x: Int => this.compare(x) == 0
    case x: Float => this.compare(x) == 0
    case x: Long => this.compare(x) == 0
    case _ => false
  }

  /**
   * This interval represents a whole bunch of numbers in a given interval,
   * so the hascode should not be for the middle value.
   */
  override def hashCode(): Int = d.hashCode + interval.xlo.hashCode + interval.xhi.hashCode

  //following the interpretation of this being a range of floats
  override def toString: String = d.toString + "(abs: " + absError+ ") (rel: " + relError + ")"

  //middle value +/- max deviation
  def toStringAsDeviation: String = d.toString + " +/- " + doubleFormat.format(aa.radius)

  def interval: Interval = aa

  /**
   * Returns the relative error for normalized numbers and the absolute error for
   * denormalized ones.
   */
  def relError: Double = {
    if(math.abs(d) < Double.MinPositiveValue) { //denormalized
      return math.max(d - aa.xlo, aa.xhi - d)
    }
    else {
      if (aa.xlo == aa.xhi) 0.0
      else {
        val re1 = divUp(subUp(d, aa.xlo), math.abs(aa.xlo))
        val re2 = divUp(subUp(aa.xhi, d), math.abs(aa.xhi))
        math.max(re1, re2)
      }
    }
  }

  //absolute Error
  def absError: Double = {
    subUp(aa.xhi, aa.xlo)
  }

  //with relative errors
  def toStringWithErrors: String = {
    var str = toString   //interval
    return str + " (" + doubleFormat.format(relError) + ")"
  }

  def toStringWithAbsErrors: String = {
    var str = toString
    return str + " (" + doubleFormat.format(absError) + ")"
  }



  def compare(y: IntervalFloat): Int = {
    val iDiff = y.aa - this.aa

    if (iDiff.xlo == 0.0 && iDiff.xhi == 0.0) return 0
    else if (iDiff.xlo > 0.0) return -1 //x is smaller
    else if (iDiff.xhi < 0.0) return 1 //x is bigger

    else {
      //comparison has failed: go by the middle value
      conditionFlag = false
      if (printComparisonFailure) println("comparison failed! " + failMessage
          + "  uncertainty interval: " + iDiff)
      return java.lang.Double.compare(this.d, y.d)
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


