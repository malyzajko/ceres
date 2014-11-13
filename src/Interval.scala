package ceres

import java.lang.Math.{nextUp}
import scala.math.{min, max}
import scala.Double.{PositiveInfinity => PlusInf}
import scala.Double.{NegativeInfinity => MinusInf}
import scala.Double.{MaxValue, MinValue}
import DirectedRounding._

import scala.language.implicitConversions

object Interval {
  implicit def double2Interval(f: Double): Interval = {
    if(f != f) EmptyInterval
    else new NormalInterval(f)
  }

  def apply(f: Double): Interval = {
    if(f != f) EmptyInterval
    else new NormalInterval(f)
  }

  def apply(a: Double, b: Double): Interval = {
    if(a != a || b != b) EmptyInterval
    else new NormalInterval(a, b)
  }
  def pow(x: Interval, y: Interval): Interval = {
    if (y.xlo == y.xhi && y.xlo.isWhole) {
      val power = y.xlo.toInt
      if (power == 0) return Interval(1.0)
      else if (power > 0) {
        var result = x
        for (i <- 1 until power)
          result = result * x
        return result
      }
      else {
        var denom = x
        for (i <- 1 until -power)
          denom = denom * x
        return 1.0/denom
      }
    }
    else {
      (y * x.ln).exponential
    }
  }
  def sqrt(x: Interval): Interval = x.squareRoot
  def log(x: Interval): Interval = x.ln
  def exp(x: Interval): Interval = x.exponential
  def cos(x: Interval): Interval = x.cosine
  def sin(x: Interval): Interval = x.sine
  def tan(x: Interval): Interval = x.tangent
  def acos(x: Interval): Interval = x.arccosine
  def asin(x: Interval): Interval = x.arcsine
  def atan(x: Interval): Interval = x.arctangent

  //Pi is correctly rounded and the bound corresponds to the lower one (tested by inspection)
  val PI = NormalInterval(math.Pi, nextUp(math.Pi))
  val PI_half = NormalInterval(divDown(math.Pi, 2.0), divUp(math.Pi, 2.0))

  var doubleString = "%.16g"
}


abstract class Interval {

  def unary_-(): Interval

  def +(y: Interval): Interval
  def -(y: Interval): Interval
  def *(y: Interval): Interval
  def /(y: Interval): Interval

  def squareRoot: Interval
  def ln: Interval
  def exponential: Interval

  def cosine: Interval
  def sine: Interval
  def tangent: Interval

  def arccosine: Interval
  def arcsine: Interval
  def arctangent: Interval

  val mid: Double
  def radius: Double
  val width: Double
  val xlo: Double
  val xhi: Double
  def contains(y: Double): Boolean
  def contains(y: Interval) : Boolean
  val isRealLine: Boolean
  val isExact: Boolean
  def absValue: Interval
}


/**
 * A classic interval.
 * The bounds can be infinities, but not NaNs.
 * The lower bound must be smaller than the upper bound.
 */
case class NormalInterval(val xlo: Double, val xhi: Double) extends Interval {
  import Interval._

  assert(xlo <= xhi, "lower bound on interval ("+xlo+") is greater than upper bound ("+xhi+")")
  assert(xlo == xlo, "lower bound is NaN")
  assert(xhi == xhi, "upper bound is NaN")

  /**
   * If the value can be represented exactly in binary, no rounding is performed.
   */
  def this(d: Double) = {
    this(if(DirectedRounding.isExact(d)) d else nextDown(d),
      if(DirectedRounding.isExact(d)) d else nextUp(d))
  }

  /** Middle point of this interval. */
  lazy val mid: Double = {
    if(isRealLine) 0.0
    else if(xlo == xhi) xlo
    else if(xlo == MinusInf) MinValue
    else if(xhi == PlusInf) MaxValue
    else
      divUp(xlo,2.0) + divDown(xhi,2.0)
  }

  /** Distance between the middle point to the outer bound. */
  def radius: Double = {
    if(xlo == xhi) 0.0
    else max(subUp(mid,xlo), subUp(xhi,mid))
  }

  //only informative, no directed rounding
  lazy val width = math.abs(xhi - xlo)

  def contains(y: Double): Boolean = xlo <= y && y <= xhi

  def contains(y: Interval): Boolean = y match {
    case EmptyInterval => false  // TODO: this may not be exactly the right choice
    case NormalInterval(lo, hi) => return xlo <= lo && hi <= xhi
  }

  lazy val isRealLine: Boolean = (xlo == MinusInf && xhi == PlusInf)

  lazy val isExact: Boolean = (xlo == xhi)

  override def toString: String = "[" + doubleString.format(xlo) + "," + doubleString.format(xhi) + "]"

  def unary_-(): Interval = NormalInterval(-xhi, -xlo)

  def +(y: Interval): Interval = y match {
    case EmptyInterval => EmptyInterval
    case NormalInterval(ylo, yhi) => NormalInterval(addDown(xlo,ylo), addUp(xhi,yhi))
  }

  def -(y: Interval): Interval = y match {
    case EmptyInterval => EmptyInterval
    case NormalInterval(ylo, yhi) =>  NormalInterval(subDown(xlo,yhi), subUp(xhi,ylo))
  }

  def *(y: Interval): Interval = y match {
    case EmptyInterval => EmptyInterval
    case NormalInterval(0.0, 0.0) => y
    case NormalInterval(ylo, yhi) =>
      if(xlo == 0.0 && xhi == 0.0) return NormalInterval(0.0, 0.0)
      else if(xlo >= 0.0) {
        if(ylo >= 0.0) NormalInterval(multDown(xlo,ylo), multUp(xhi,yhi))
        else if(yhi <= 0.0) NormalInterval(multDown(xhi,ylo), multUp(xlo,yhi))
        else NormalInterval(multDown(xhi,ylo), multUp(xhi,yhi))
      }
      else if(xhi <= 0.0) {
        if(ylo >= 0.0) NormalInterval(multDown(xlo,yhi), multUp(xhi,ylo))
        else if(yhi <= 0.0) NormalInterval(multDown(xhi,yhi), multUp(xlo,ylo))
        else NormalInterval(multDown(xlo,yhi), multUp(xlo,ylo))
      }
      else {
        if(ylo >= 0.0) NormalInterval(multDown(xlo,yhi), multUp(xhi,yhi))
        else if(yhi <= 0.0) NormalInterval(multDown(xhi,ylo), multUp(xlo,ylo))
        else {
          val a = min(multDown(xlo,yhi), multDown(xhi,ylo))
          val b = max(multUp(xlo,ylo), multUp(xhi,yhi))
          NormalInterval(a, b)
        }
      }
  }

  def /(y: Interval): Interval = y match {
    case EmptyInterval => EmptyInterval
    case NormalInterval(0.0, 0.0) => EmptyInterval
    case NormalInterval(ylo, yhi) =>

      if(xlo == 0.0 && ylo == 0.0) NormalInterval(0.0, 0.0)
      else if(ylo >= 0.0) {
        if(xlo >= 0.0) NormalInterval(divDown(xlo,yhi), divUp(xhi,ylo))
        else if(xhi <= 0.0) NormalInterval(divDown(xlo,ylo), divUp(xhi,yhi))
        else NormalInterval(divDown(xlo,ylo), divUp(xhi,ylo))
      }
      else if(yhi <= 0.0) {
        if(xlo >= 0.0) NormalInterval(divDown(xhi,yhi), divUp(xlo,ylo))
        else if(xhi <= 0.0) NormalInterval(divDown(xhi,ylo), divUp(xlo,yhi))
        else NormalInterval(divDown(xhi,yhi), divUp(xlo,yhi))
      }
      else NormalInterval(MinusInf, PlusInf)
  }

  def squareRoot: Interval = {
    if(xhi < 0.0) EmptyInterval
    else if(xlo <= 0.0)
      NormalInterval(0.0, sqrtUp(xhi))
    else
      NormalInterval(sqrtDown(xlo), sqrtUp(xhi))
  }

  def absValue: Interval = {
    if(xlo >= 0.0) return this
    else if(xhi <= 0.0) return NormalInterval(-xhi, -xlo)
    else
      NormalInterval(0.0, max(-xlo, xhi))
  }

  def ln: Interval = {
    if(xhi <= 0.0) return EmptyInterval
    else if(xlo <= 0.0) return NormalInterval(MinusInf, up1(math.log(xhi)))
    else NormalInterval(down1(math.log(xlo)), up1(math.log(xhi)))
  }

  def exponential: Interval = NormalInterval(down1(math.exp(xlo)), up1(math.exp(xhi)))

  def cosine: Interval = {
    val a = if(xlo > 0.0) divDown(xlo, PI.xhi) else divDown(xlo,PI.xlo)
    val b = if(xhi > 0.0) divUp(xhi, PI.xlo) else divUp(xhi, PI.xhi)

    val m = math.floor(a).toLong
    val n = math.ceil(b).toLong

    if(n-m < 2.0) {
      if(m % 2.0 == 0.0) {
        return NormalInterval(down1(math.cos(xhi)), up1(math.cos(xlo)))
      }
      else {
        return NormalInterval(down1(math.cos(xlo)), up1(math.cos(xhi)))
      }
    }
    else if(n-m == 2.0) {
      if(m % 2.0 == 0.0) return NormalInterval(-1.0, max(up1(math.cos(xlo)), up1(math.cos(xhi))))
      else return NormalInterval(min(down1(math.cos(xlo)), down1(math.cos(xhi))), 1.0)
    }
    else return NormalInterval(-1.0, 1.0)
  }


  def sine: Interval = {
    val a = if(xlo > 0.0) subDown(divDown(xlo, PI.xhi), 0.5) else subDown(divDown(xlo,PI.xlo), 0.5)
    val b = if(xhi > 0.0) subUp(divUp(xhi, PI.xlo), 0.5) else subUp(divUp(xhi, PI.xhi), 0.5)

    val m = math.floor(a).toLong
    val n = math.ceil(b).toLong

    if(n-m < 2.0) {
      if(m % 2.0 == 0.0) {
        return NormalInterval(down1(math.sin(xhi)), up1(math.sin(xlo)))
      }
      else {
        return NormalInterval(down1(math.sin(xlo)), up1(math.sin(xhi)))
      }
    }
    else if(n-m == 2.0) {
      if(m % 2.0 == 0.0) return NormalInterval(-1.0, max(up1(math.sin(xlo)), up1(math.sin(xhi))))
      else return NormalInterval(min(down1(math.sin(xlo)), down1(math.sin(xhi))), 1.0)
    }
    else return NormalInterval(-1.0, 1.0)
  }

  def tangent: Interval = {
    val ka = if(xlo > 0.0) divDown(xlo,PI_half.xhi) else divDown(xlo,PI_half.xlo)
    val kb = if(xhi > 0.0) divUp(xhi, PI_half.xlo) else divUp(xhi, PI_half.xhi)

    val m = math.floor(ka).toLong
    val n = math.ceil(kb).toLong

    if(n-m < 2.0) //constant f"
      NormalInterval(down1(math.tan(xlo)), up1(math.tan(xhi)))
    else if(n-m == 2.0 && m % 2.0 != 0.0) //odd
      NormalInterval(down1(math.tan(xlo)), up1(math.tan(xhi)))
    else
      NormalInterval(MinusInf, PlusInf)
  }

  def arccosine: Interval = {
    if(xhi < -1.0 || xlo > 1.0) EmptyInterval
    else {
      val a = if(xlo < -1.0) -1.0 else xlo
      val b = if(xhi > 1.0) 1.0 else xhi
      NormalInterval(down1(math.acos(b)), up1(math.acos(a)))
    }
  }

  def arcsine: Interval = {
    if(xhi < -1.0 || xlo > 1.0) EmptyInterval
    else {
      val a = if(xlo < -1.0) -1.0 else xlo
      val b = if(xhi > 1.0) 1.0 else xhi
      NormalInterval(down1(math.asin(a)), up1(math.asin(b)))
    }
  }

  def arctangent: Interval = NormalInterval(down1(math.atan(xlo)), up1(math.atan(xhi)))

}

case object EmptyInterval extends Interval {
  def unary_-(): Interval = EmptyInterval

  def +(rhs: Interval): Interval = EmptyInterval
  def -(rhs: Interval): Interval = EmptyInterval

  def +(f: Double): Interval = EmptyInterval
  def -(f: Double): Interval = EmptyInterval
  def *(f: Double): Interval = EmptyInterval
  def /(f: Double): Interval = EmptyInterval

  def *(rhs: Interval): Interval = EmptyInterval
  def /(rhs: Interval): Interval = EmptyInterval
  def squareRoot: Interval = EmptyInterval
  def ln: Interval = EmptyInterval
  def exponential: Interval = EmptyInterval
  def sine: Interval = EmptyInterval
  def cosine: Interval = EmptyInterval
  def tangent: Interval = EmptyInterval
  def arcsine: Interval = EmptyInterval
  def arccosine: Interval = EmptyInterval
  def arctangent: Interval = EmptyInterval
  def hypsine: Interval = EmptyInterval
  def hypcosine: Interval = EmptyInterval
  def hyptangent: Interval = EmptyInterval
  def absValue: Interval = EmptyInterval
  def ceiling: Interval = EmptyInterval
  def floorFnc: Interval = EmptyInterval
  val mid: Double = Double.NaN
  def radius: Double = 0.0
  val width = -1.0
  def contains(y: Double): Boolean = {
    if(y != y) true //to be consistent with mid and assertions
    else false
  }
  def contains(y: Interval): Boolean = false

  val xlo: Double = 1.0
  val xhi: Double = -1.0

  val isRealLine: Boolean = false
  val isExact: Boolean = false
  override def toString: String = "[]"
}


