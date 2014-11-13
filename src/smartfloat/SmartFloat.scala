package ceres.smartfloat

import GeneralForm._
import ceres.DirectedRounding._
import ceres.Interval
import java.lang.Math.{nextUp}

import scala.math.{ScalaNumericConversions, ScalaNumber}

import scala.language.implicitConversions

object SmartFloat {

  class SmartDouble(d: Double) {
    def +/-(un: Double): SmartFloat = new SmartFloat(d, new GeneralForm(d, un))
  }

  implicit def double2SmartDouble(d: Double): SmartDouble = new SmartDouble(d)

  implicit def double2SmartFloat(d : Double): SmartFloat = new SmartFloat(d)

  //variables
  def apply(d: Double): SmartFloat = new SmartFloat(d)
  def apply(d: Double, u: Double): SmartFloat = new SmartFloat(d, u)


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


  val E = SmartFloat(math.E)
  val Pi = SmartFloat(math.Pi)

  def sqrt(x: SmartFloat): SmartFloat = new SmartFloat(math.sqrt(x.d), x.aa.squareRoot)
  def log(x: SmartFloat): SmartFloat = new SmartFloat(math.log(x.d), x.aa.ln)
  def exp(x: SmartFloat): SmartFloat = new SmartFloat(math.exp(x.d), x.aa.exponential)
  def pow(x: SmartFloat, y: SmartFloat): SmartFloat =
    new SmartFloat(math.pow(x.d, y.d), (y.aa * x.aa.ln).exponential)
  def pow2(x: SmartFloat): SmartFloat = x * x
  def pow3(x: SmartFloat): SmartFloat = x * x * x
  def pow4(x: SmartFloat): SmartFloat = x * x * x * x
  def cos(x: SmartFloat): SmartFloat = new SmartFloat(math.cos(x.d), x.aa.cosine)
  def sin(x: SmartFloat): SmartFloat = new SmartFloat(math.sin(x.d), x.aa.sine)
  def tan(x: SmartFloat): SmartFloat = new SmartFloat(math.tan(x.d), x.aa.tangent)
  def acos(x: SmartFloat): SmartFloat = new SmartFloat(math.acos(x.d), x.aa.arccosine)
  def asin(x: SmartFloat): SmartFloat = new SmartFloat(math.asin(x.d), x.aa.arcsine)
  def atan(x: SmartFloat): SmartFloat = new SmartFloat(math.atan(x.d), x.aa.arctangent)
  def random: SmartFloat = new SmartFloat(math.random)  //constant

  //Other functions. We'll skip for now: ceil, floor, signum, round.

  def abs(x: SmartFloat): SmartFloat = {
    if(x.compare(0.0) == 0) //to trigger check
      return new SmartFloat(math.abs(x.d), x.aa.absValue)
    else
      return new SmartFloat(math.abs(x.d), x.aa.absValue)
  }

  /**
   * Returns the bigger of two values.
   * If comparison cannot be decided soundly, double values of SmartFloat will be used.
   */
  def max(x: SmartFloat, y: SmartFloat): SmartFloat = {
    val i = x.compare(y)
    if (i == -1 || i == 0) return y
    else return x
  }

  /**
   * Returns the smaller of two values.
   * If comparison cannot be decided soundly, double values of SmartFloat will be used.
   */
  def min(x: SmartFloat, y: SmartFloat): SmartFloat = {
    val i = x.compare(y)
    if (i == -1 || i == 0) return x
    else return y
  }

  //At one of the endpoints. Note that if we staddle 0, this is meaningless, or when it comes
  //very close to zero...
  def computeRelErr(f: Interval, absError: Double): Double = {
    val rel1 = if (f.xlo == 0.0) 0.0 else divUp(absError, math.abs(f.xlo))
    val rel2 = if (f.xhi == 0.0) 0.0 else divUp(absError, math.abs(f.xhi))
    return math.max(rel1, rel2)
  }
}



class SmartFloat(val d: Double, val aa: AffineForm) extends ScalaNumber with ScalaNumericConversions
  with Ordered[SmartFloat] {
  import SmartFloat._
  import AffineForm._

  //println("new Range: " + this.interval)

  assert(aa.interval.contains(d), "affine does not contain value " + d + " " + aa.interval)

  def this(ff: Double) = this(ff, new GeneralForm(ff))
  def this(ff: Double, un: Double) = this(ff, new GeneralForm(ff, un))


  def +(y: SmartFloat): SmartFloat = new SmartFloat(this.d + y.d, this.aa + y.aa)
  def -(y: SmartFloat): SmartFloat = new SmartFloat(this.d - y.d, this.aa - y.aa)
  def *(y: SmartFloat): SmartFloat = new SmartFloat(this.d * y.d, this.aa * y.aa)
  def /(y: SmartFloat): SmartFloat = new SmartFloat(this.d / y.d, this.aa / y.aa)
  def unary_-(): SmartFloat = new SmartFloat(-this.d, -this.aa)

  def %(y: SmartFloat): SmartFloat = {
    this - (y * (this/y).toInt)
  }

  def addError(err: SmartFloat): SmartFloat = {
    return new SmartFloat(d, GeneralForm.addError(aa, err.aa))
  }

  /**
   * Returns true if the other is a value and within the error tolerance.
   * Note that the comparison flag may be set, since we use the compare method.
   */
  override def equals(other: Any): Boolean = other match {
    case x: SmartFloat => this.compare(x) == 0
    case x: Double => this.compare(x) == 0
    case x: Short => this.compare(x) == 0
    case x: Char => this.compare(x) == 0
    case x: Byte => this.compare(x) == 0
    case x: Int => this.compare(x) == 0
    case x: Float => this.compare(x) == 0
    case x: Long => this.compare(x) == 0
    case _ => false
  }

  override def hashCode(): Int = d.hashCode + interval.xlo.hashCode + interval.xhi.hashCode

  //following the interpretation of this being a range of floats
  override def toString: String = this.interval.toString +
    "  (abs: " + this.absError + ")"

  //middle value +/- max deviation
  def toStringAsDeviation: String = d.toString + " +/- " + doubleFormat.format(aa.radius)

  def interval: Interval = {
    aa match {
      case e: GeneralForm => e.interval
      //case p: ParamAForm => p.interval
      case _=> aa.interval
    }
  }

  def relError: Double = {
    aa match {
      case e: GeneralForm => computeRelErr(e.interval, e.maxRoundoff)
      //case p: ParamAForm => computeRelErr(p.interval, p.maxRoundoff)
      case FullForm => scala.Double.NaN
      case EmptyForm => scala.Double.NaN
      case _ => println("wrong AF! " + aa.getClass()); scala.Double.NaN
    }
  }

  //absolute Error
  def absError: Double = {
    aa match {
      case e: GeneralForm => e.maxRoundoff
      //case p: ParamAForm => p.maxRoundoff
      case FullForm => scala.Double.NaN
      case EmptyForm => scala.Double.NaN
      case _ => println("wrong AF!"); scala.Double.NaN
    }
  }

  //with relative errors
  def toStringWithErrors: String = {
    toString + " (rel: " + doubleFormat.format(relError) + ")"
  }

  def toStringWithAbsErrors: String = {
    toString + " (" + doubleFormat.format(absError) + ")"
  }

  def analyzeRoundoff = {
    aa match {
      //case p: ParamAForm => p.analyzeRoundoff
      case _ => println("sorry, no analysis is possible")
    }
  }


  def compare(y: SmartFloat): Int = {
    var diff = y.aa - this.aa
    val iDiff = diff.interval

    if (iDiff.xlo == 0.0 && iDiff.xhi == 0.0) return 0
    else if (iDiff.xlo > 0.0) return -1 //x is smaller
    else if (iDiff.xhi < 0.0) return 1 //x is bigger

    else {
      if (printComparisonFailure) {
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


