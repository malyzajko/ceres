package ceres.common 

import scala.math.{ScalaNumericConversions, ScalaNumber}

//import scala.language.implicitConversions

object DoubleDouble {

  implicit def int2DoubleDouble(i: Int): DoubleDouble = new DoubleDouble(i.toDouble)
  implicit def double2DoubleDouble(d: Double): DoubleDouble = new DoubleDouble(d)

  def apply(d: Double): DoubleDouble = new DoubleDouble(d)

  def sqrt(x: DoubleDouble): DoubleDouble =
    new DoubleDouble(DDouble.sqrt(x.x0, x.x1))

  def abs(x: DoubleDouble): DoubleDouble = if (x.x0 < 0.0) -x else x
  def max(x: DoubleDouble, y: DoubleDouble): DoubleDouble = if (x > y) x else y
  def min(x: DoubleDouble, y: DoubleDouble): DoubleDouble = if (x < y) x else y
}

class DoubleDouble(val x0: Double, val x1: Double) extends ScalaNumber
  with ScalaNumericConversions with Ordered[DoubleDouble] {
  import DoubleDouble._

  def this(d: Double) = this(d, 0.0)
  def this(d: Array[Double]) = this(d(0), d(1))

  def +(y: DoubleDouble): DoubleDouble = new DoubleDouble(DDouble.add(x0, x1, y.x0, y.x1))
  def -(y: DoubleDouble): DoubleDouble = new DoubleDouble(DDouble.sub(x0, x1, y.x0, y.x1))
  def *(y: DoubleDouble): DoubleDouble = new DoubleDouble(DDouble.mult(x0, x1, y.x0, y.x1))
  def /(y: DoubleDouble): DoubleDouble = new DoubleDouble(DDouble.div(x0, x1, y.x0, y.x1))

  def %(y: DoubleDouble): DoubleDouble = this - (y * (this/y).toInt)

  def unary_-(): DoubleDouble = new DoubleDouble(-x0, -x1)

  def isNaN: Boolean = x0 != x0 || x1 != x1

  override def equals(other: Any): Boolean = other match {
    case x: DoubleDouble => this.compare(x) == 0
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

  // TODO: def toLongString(digits: Int = 30): String = DDouble.toString(digits, x0, x1, x2, x3)

  def compare(y: DoubleDouble): Int = {
    if (x0 < y.x0) -1
    else if(x0 > y.x0) 1
    else {
      if (x1 < y.x1) -1
      else if(x1 > y.x1) 1
      else {
        0
      }
    }
  }

  override def byteValue(): Byte = Predef.double2Double(x0).byteValue
  override def doubleValue(): Double = (x0 + x1)
  override def floatValue(): Float = Predef.double2Double(x0).floatValue
  override def intValue(): Int = Predef.double2Double(x0).intValue
  override def isValidByte: Boolean = false
  override def isValidChar: Boolean = false
  override def isValidInt: Boolean = false
  override def isValidShort: Boolean = false
  override def longValue(): Long = Predef.double2Double(x0).longValue
  override def shortValue(): Short = Predef.double2Double(x0).shortValue
  override def toByte: Byte = x0.toByte
  override def toChar: Char = x0.toChar
  override def toDouble: Double = (x0 + x1)
  override def toFloat: Float = x0.toFloat
  override def toInt: Int = x0.toInt
  override def toLong: Long = x0.toLong
  override def toShort: Short = x0.toShort
  def underlying(): AnyRef = this
  override def isWhole(): Boolean = x0 % 1.0 == 0.0 && x1 == 0.0

}
