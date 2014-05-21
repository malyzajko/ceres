package ceres.common 

import scala.math.{ScalaNumericConversions, ScalaNumber}

import ceres.common.{QuadDoubleInterface => QDI}

import scala.language.implicitConversions

object QuadDouble {

  private var old_cw: Long = 0L
  def qdStart = old_cw = QDI.fpuFixStart()
  def qdEnd = QDI.fpuFixEnd(old_cw)

  implicit def int2QuadDouble(i: Int): QuadDouble = new QuadDouble(i.toDouble)
  implicit def double2QuadDouble(d: Double): QuadDouble = new QuadDouble(d)

  def apply(d: Double): QuadDouble = new QuadDouble(d)

  def sqrt(x: QuadDouble): QuadDouble =
    new QuadDouble(QDI.sqrt(x.x0, x.x1, x.x2, x.x3))
  def log(x: QuadDouble): QuadDouble =
    new QuadDouble(QDI.log(x.x0, x.x1, x.x2, x.x3))
  def exp(x: QuadDouble): QuadDouble =
    new QuadDouble(QDI.exp(x.x0, x.x1, x.x2, x.x3))
  def pow(x: QuadDouble, y: QuadDouble): QuadDouble =
    new QuadDouble(QDI.pow(x.x0, x.x1, x.x2, x.x3, y.x0, y.x1, y.x2, y.x3))
  def pow2(x: QuadDouble): QuadDouble = pow(x, QuadDouble(2.0))
  def pow3(x: QuadDouble): QuadDouble = pow(x, QuadDouble(3.0))
  def pow4(x: QuadDouble): QuadDouble = pow(x, QuadDouble(4.0))

  def cos(x: QuadDouble): QuadDouble =
    new QuadDouble(QDI.cos(x.x0, x.x1, x.x2, x.x3))
  def sin(x: QuadDouble): QuadDouble =
    new QuadDouble(QDI.sin(x.x0, x.x1, x.x2, x.x3))
  def tan(x: QuadDouble): QuadDouble =
    new QuadDouble(QDI.tan(x.x0, x.x1, x.x2, x.x3))
  def acos(x: QuadDouble): QuadDouble =
    new QuadDouble(QDI.acos(x.x0, x.x1, x.x2, x.x3))
  def asin(x: QuadDouble): QuadDouble =
    new QuadDouble(QDI.asin(x.x0, x.x1, x.x2, x.x3))
  def atan(x: QuadDouble): QuadDouble =
    new QuadDouble(QDI.atan(x.x0, x.x1, x.x2, x.x3))

  def abs(x: QuadDouble): QuadDouble = if (x.x0 < 0.0) -x else x

  def max(x: QuadDouble, y: QuadDouble): QuadDouble = if (x > y) x else y
  def min(x: QuadDouble, y: QuadDouble): QuadDouble = if (x < y) x else y
}

class QuadDouble(val x0: Double, val x1: Double, val x2: Double, val x3: Double) extends ScalaNumber with Ordered[QuadDouble] {
//with ScalaNumericConversions with Ordered[QuadDouble] {
  import QuadDouble._

  def this(d: Double) = this(d, 0.0, 0.0, 0.0)
  def this(d0: Double, d1: Double) = this(d0, d1, 0.0, 0.0)
  def this(x: Array[Double]) = this(x(0), x(1), x(2), x(3))

  def +(y: QuadDouble): QuadDouble =
    new QuadDouble(QDI.add(x0, x1, x2, x3, y.x0, y.x1, y.x2, y.x3))
  def -(y: QuadDouble): QuadDouble =
    new QuadDouble(QDI.sub(x0, x1, x2, x3, y.x0, y.x1, y.x2, y.x3))
  def *(y: QuadDouble): QuadDouble =
    new QuadDouble(QDI.mult(x0, x1, x2, x3, y.x0, y.x1, y.x2, y.x3))
  def /(y: QuadDouble): QuadDouble =
    new QuadDouble(QDI.div(x0, x1, x2, x3, y.x0, y.x1, y.x2, y.x3))

  def %(y: QuadDouble): QuadDouble = {
    this - (y * (this/y).toInt)
  }

  def unary_-(): QuadDouble = new QuadDouble(-x0, -x1, -x2, -x3)

  def isNaN: Boolean = x0 != x0 || x1 != x1 || x2 != x2 || x3 != x3

  override def equals(other: Any): Boolean = other match {
    case x: QuadDouble => this.compare(x) == 0
    case x: Double => this.compare(x) == 0
    case x: Short => this.compare(x) == 0
    case x: Char => this.compare(x) == 0
    case x: Byte => this.compare(x) == 0
    case x: Int => this.compare(x) == 0
    case x: Float => this.compare(x) == 0
    case x: Long => this.compare(x) == 0
    case _ => false
  }

  override def hashCode(): Int = x0.hashCode

  override def toString: String = (x0 + x1).toString

  def toLongString(digits: Int = 60): String = QDI.toString(digits, x0, x1, x2, x3)

  def compare(y: QuadDouble): Int = {
    if (x0 < y.x0) -1
    else if(x0 > y.x0) 1
    else {
      // x0 == y.x0
      if (x1 < y.x1) -1
      else if(x1 > y.x1) 1
      else {
        // x0 == y.x0 && x1 == y.x1
        if (x2 < y.x2) -1
        else if(x2 > y.x2) 1
        else {
          // x0 == y.x0 && x1 == y.x1 && x2 == y.x2
          if (x3 < y.x3) -1
          else if(x3 > y.x3) 1
          else {
            0
          }
        }
      }
    }
  }

  override def byteValue(): Byte = Predef.double2Double(x0).byteValue
  override def doubleValue(): Double = (x3 + x2 + x1 + x0)
  override def floatValue(): Float = Predef.double2Double(x0).floatValue
  override def intValue(): Int = Predef.double2Double(x0).intValue
  /*override def isValidByte: Boolean = false
  override def isValidChar: Boolean = false
  override def isValidInt: Boolean = false
  override def isValidShort: Boolean = false*/
  override def longValue(): Long = Predef.double2Double(x0).longValue
  override def shortValue(): Short = Predef.double2Double(x0).shortValue
   def toByte: Byte = x0.toByte
   def toChar: Char = x0.toChar
   def toDouble: Double = (x3 + x2 + x1 + x0)
   def toFloat: Float = x0.toFloat
   def toInt: Int = x0.toInt
   def toLong: Long = x0.toLong
   def toShort: Short = x0.toShort
  def underlying(): AnyRef = this
  override def isWhole(): Boolean = x0 % 1.0 == 0.0 && x1 == 0.0 && x2 == 0.0 && x3 == 0.0

}
