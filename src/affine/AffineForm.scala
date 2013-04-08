package ceres.affine 

import collection.mutable.Queue
import java.lang.Math.{nextUp}
import scala.Double.{PositiveInfinity => PlusInf}
import scala.Double.{NegativeInfinity => MinusInf}
import scala.Double.{MaxValue, MinValue}

import ceres.common._
import ceres.common.{DirectedRounding => DirRound}

import DDouble._
import math.{min => mmin, max => mmax, abs => mabs}
//So they don't get confused...
import ceres.common.DirectedRounding.{multDown => multD, multUp => multU, divUp => divU,
      divDown => divD, subDown => subD, subUp => subU, addUp => addU, addDown => addD,
      sqrtDown => sqrtD, sqrtUp => sqrtU}
import ceres.common.DirectedRounding.{down1 => d1, up1 => u1, nextDown}

//import scala.language.implicitConversions


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

  def apply(i: Interval): AffineForm = i match {
    case EmptyInterval => EmptyForm
    case NormalInterval(xlo, xhi) =>
      if(mabs(xlo) == PlusInf || mabs(xhi) == PlusInf) FullForm
      else {
        val x0_new = DirRound.divUp(xlo, 2.0) + DirRound.divDown(xhi, 2.0)
        val s =  subUp(xlo, 0.0, xhi, 0.0)
        val xk =  divUp(s(0), s(1), 2.0, 0.0)
        return new AForm(x0_new, new Queue[NoiseTerm]() += NoiseTerm(newIndex, xk))
      }
  }
  def apply(d: Double): AffineForm = {
    if (d != d) return EmptyForm
    else if (d == PlusInf || d == MinusInf) return FullForm
    else return new AForm(d)
  }

  implicit def int2AffineForm(i: Int): AffineForm = new AForm(i.toDouble)
  implicit def double2AffineForm(d: Double): AffineForm = {
    if (d != d) EmptyForm
    if (d == PlusInf || d == MinusInf) FullForm
    else new AForm(d)
  }

  def pow(x: AffineForm, y: AffineForm): AffineForm = {
    if (y.radius == 0.0 && y.x0.isWhole) {
      val power = y.x0.toInt
      if (power == 0) return new AForm(1.0)
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

  def sqrt(x: AffineForm): AffineForm = x.squareRoot
  def log(x: AffineForm): AffineForm = x.ln
  def exp(x: AffineForm): AffineForm = x.exponential
  def cos(x: AffineForm): AffineForm = x.cosine
  def sin(x: AffineForm): AffineForm = x.sine
  def tan(x: AffineForm): AffineForm = x.tangent
  def acos(x: AffineForm): AffineForm = x.arccosine
  def asin(x: AffineForm): AffineForm = x.arcsine
  def atan(x: AffineForm): AffineForm = x.arctangent

  // TODO: maybe we want to produce a warning here
  def abs(x: AffineForm): AffineForm = if (x.x0 < 0.0) -x else x

  private var currIndex: Int = 0
  def newIndex: Int = {
    currIndex += 1
    assert(currIndex != Int.MaxValue, "Affine indices just ran out...")
    currIndex
  }

  var maxNoiseCount = 42
  var doubleFormat = "%1.4e"
  val packingThreshold = math.pow(10, -32) //smaller numbers should be internal errors
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


