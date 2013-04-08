package ceres.smartfloat

import java.lang.Math.{nextUp}
import scala.Double.{PositiveInfinity => PlusInf}
import scala.Double.{NegativeInfinity => MinusInf}
import scala.Double.{MaxValue, MinValue}
import ceres.common.{DirectedRounding => DirRound}
import ceres.common.{Interval, NormalInterval, EmptyInterval}

import ceres.common.DDouble
import DDouble._
import math.{min => mmin, max => mmax, abs => mabs}
//So they don't get confused...
import ceres.common.DirectedRounding.{multDown => multD, multUp => multU, divUp => divU,
      divDown => divD, subDown => subD, subUp => subU, addUp => addU, addDown => addD,
      sqrtDown => sqrtD, sqrtUp => sqrtU}
import ceres.common.DirectedRounding.{down1 => d1, up1 => u1, nextDown}



abstract class AffineForm {
  assert(x0 != PlusInf && x0 != MinusInf, "affine form with infinity as central value")
  assert(radius != PlusInf, "radius of affine form is infinite")
  assert(x0 == x0, "central value of affine form is NaN")
  assert(radius == radius, "radius of affine form is NaN")

  def unary_-(): AffineForm

  def +(y: AffineForm): AffineForm
  def -(y: AffineForm): AffineForm
  def *(y: AffineForm): AffineForm
  def /(y: AffineForm): AffineForm

  def squareRoot: AffineForm
  def ln: AffineForm
  def exponential: AffineForm

  def cosine: AffineForm
  def sine: AffineForm
  def tangent: AffineForm

  def arccosine: AffineForm
  def arcsine: AffineForm
  def arctangent: AffineForm

  val x0: Double
  val interval: Interval
  val intervalExt: (Array[Double], Array[Double])
  val radius: Double
  val radiusExt: Array[Double]
  def absValue: AffineForm
  def isNonZero: Boolean

}

object AffineForm {

  private var currIndex: Int = 0
  def newIndex: Int = {
    currIndex += 1
    assert(currIndex != Int.MaxValue, "Affine indices just ran out...")
    currIndex
  }
  def currentIndex = currIndex

  /** ONLY USE THIS WHEN U KNOW WHAT U'RE DOING! */
  def resetCounter = currIndex = 0
}


abstract class AbstractAForm extends AffineForm {
  import AffineForm._
  import AffineUtils._

  def isNonZero: Boolean = return (x0 != 0.0 || xnoise.size > 0)

  def xnoise: Queue


  /**
   * Computes the radius with outwards rounding.
   */
  val radiusExt: Array[Double] = {
    var sum = Array(0.0, 0.0)
    if(xnoise.size == 1)
      sum =  abs(xnoise.head.value)
    else {
      val iter = xnoise.getIterator
      while(iter.hasNext) {
        val xi = iter.next
        sum = addUp(sum ,  abs(xi.value))
      }
    }
    sum
  }

  val intervalExt: (Array[Double], Array[Double]) =
    ( subDown(x0, 0.0, radiusExt(0), radiusExt(1)),
       addUp(x0, 0.0, radiusExt(0), radiusExt(1)))

  val radius: Double = addU(radiusExt(0), radiusExt(1))

  /**
   * Computes the sound interval that this affine form describes.
   */
  val interval: Interval = {
    if(radius == 0.0) new NormalInterval(x0, x0)
    else {
      new NormalInterval(addD(intervalExt._1(0), intervalExt._1(1)), addU(intervalExt._2(0), intervalExt._2(1)))
    }
  }

  override def toString: String = {
    return doubleFormat.format(x0) + " " + formatQueueUncertain(xnoise)
  }

  /**
   * The central value is used for comparison with 0.
   * Note that is the central value is negative, all error terms also change sign.
   */
  def absValue: AffineForm = {
    if(x0 > 0.0) return this
    else return -this
  }

}


case object EmptyForm extends AffineForm {
  def +(rhs: AffineForm): AffineForm = EmptyForm
  def -(rhs: AffineForm): AffineForm = EmptyForm
  def unary_-(): AffineForm = EmptyForm
  def *(rhs: AffineForm): AffineForm = EmptyForm
  def /(rhs: AffineForm): AffineForm = EmptyForm
  def squareRoot: AffineForm = EmptyForm
  def sine: AffineForm = EmptyForm
  def cosine: AffineForm = EmptyForm
  def ln: AffineForm = EmptyForm
  def exponential: AffineForm = EmptyForm
  def tangent: AffineForm = EmptyForm
  def arccosine: AffineForm = EmptyForm
  def arcsine: AffineForm = EmptyForm
  def arctangent: AffineForm = EmptyForm
  def hypcosine: AffineForm = EmptyForm
  def hypsine: AffineForm = EmptyForm
  def hyptangent: AffineForm = EmptyForm
  def absValue: AffineForm = EmptyForm
  def ceiling: AffineForm = EmptyForm
  def floorFnc: AffineForm = EmptyForm
  val interval: Interval = EmptyInterval
  val intervalExt: (Array[Double], Array[Double]) =
    (Array(1.0, 0.0), Array(-1.0, 0.0))
  val radius: Double = -1.0
  val radiusExt: Array[Double] = Array(-1.0, 0.0)
  val x0: Double = scala.Double.NaN
  def isNonZero = false
}

case object FullForm extends AffineForm {
  def +(rhs: AffineForm): AffineForm = FullForm
  def -(rhs: AffineForm): AffineForm = FullForm
  def unary_-(): AffineForm = FullForm
  def *(rhs: AffineForm): AffineForm = FullForm
  def /(rhs: AffineForm): AffineForm = FullForm
  def squareRoot: AffineForm = FullForm
  def sine: AffineForm = FullForm
  def cosine: AffineForm = FullForm
  def ln: AffineForm = FullForm
  def exponential: AffineForm = FullForm
  def tangent: AffineForm = FullForm
  def arccosine: AffineForm = FullForm
  def arcsine: AffineForm = FullForm
  def arctangent: AffineForm = FullForm
  def hypcosine: AffineForm = FullForm
  def hypsine: AffineForm = FullForm
  def hyptangent: AffineForm = FullForm
  def absValue: AffineForm = FullForm
  def ceiling: AffineForm = FullForm
  def floorFnc: AffineForm = FullForm
  val interval: Interval = new NormalInterval(MinusInf, PlusInf)
  val intervalExt: (Array[Double], Array[Double]) =
    (Array(MinusInf, MinusInf), Array(PlusInf, PlusInf))
  val radius: Double = PlusInf
  val radiusExt: Array[Double] = Array(PlusInf, PlusInf)
  val x0: Double = PlusInf
  def isNonZero = false
}


