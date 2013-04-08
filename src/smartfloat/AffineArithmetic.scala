package ceres.smartfloat

import scala.collection.immutable.HashMap
import java.lang.Math.{nextUp}
import scala.Double.{PositiveInfinity => PlusInf}
import scala.Double.{NegativeInfinity => MinusInf}
import scala.Double.{MaxValue, MinValue, NaN}
import ceres.common.{DirectedRounding => DirRound}

import ceres.common.DDouble
import DDouble._
import AffineUtils._
import math.{min => mmin, max => mmax, abs => mabs}
//So they don't get confused...
import ceres.common.DirectedRounding.{multDown => multD, multUp => multU, divUp => divU,
      divDown => divD, subDown => subD, subUp => subU, addUp => addU, addDown => addD,
      sqrtDown => sqrtD, sqrtUp => sqrtU}
import ceres.common.DirectedRounding.{down1 => d1, up1 => u1, nextDown}

object AffineArithmetic {
  import AffineForm._

  type DD = Array[Double]
  case class IncompatibleAffineFormException(smth:String)  extends Exception

  abstract class Result
  case object All extends Result
  case object Nothing extends Result
  case class Value(d: Double, q: Queue, r: Array[Double]) extends Result


  def negate(xnoise: Queue): Queue = {
    var deviation = new Queue
    val iter = xnoise.getIterator
    while(iter.hasNext) {
      val xi = iter.next
      deviation :+ - xi   //no round-off error
    }
    deviation
  }


  def plus(x0: Double, xnoise: Queue, y0: Double, ynoise: Queue): Result = {
    var z0 = x0 + y0
    if(z0 == PlusInf || z0 == MinusInf) return All
    var roundoff = if (addD(x0, y0) == addU(x0, y0)) zero
      else Array(DirRound.roundOff(z0), 0.0)

    var (deviation, delta) = addQueuesUncertain(xnoise, ynoise)
    delta = addUp(delta, roundoff)

    if(delta(0) == PlusInf) return All
    if (delta(0) != 0.0 || delta(1) != 0.0) deviation :+ new Noise(newIndex, delta)
    return Value(z0, deviation, roundoff)
  }

  def minus(x0: Double, xnoise: Queue, y0: Double, ynoise: Queue): Result = {
    var z0 = x0 - y0
    if(z0 == PlusInf || z0 == MinusInf) return All
    var roundoff = if (subD(x0, y0) == subU(x0, y0)) zero
      else Array(DirRound.roundOff(z0), 0.0)

    var (deviation, delta) = subtractQueuesUncertain(xnoise, ynoise)
    delta = addUp(delta, roundoff)
    if(delta(0) == PlusInf) return All
    if (delta(0) != 0.0 || delta(1) != 0.0) deviation :+ new Noise(newIndex, delta)
    return Value(z0, deviation, roundoff)
  }

  def multiply(x0: Double, xnoise: Queue, xRadius: DD,
            y0: Double, ynoise: Queue, yRadius: DD, hint: Double = NaN): Result = {
    // Hint is necesary for center form with division, as that one moves the center off
    var z0 = if (hint != hint) x0 * y0 else hint
    if(z0 == PlusInf || z0 == MinusInf) return All
    var roundoff = if (multD(x0, y0) == multU(x0, y0)) zero
      else Array(DirRound.roundOff(z0), 0.0)

    var delta =  multUp(xRadius, yRadius)
    delta = addUp(delta, roundoff)
    var (deviation, deltaAddition) = multiplyQueuesUncertain(x0, xnoise, y0, ynoise)
    delta = addUp(delta, deltaAddition)

    if(delta(0) == PlusInf) return All
    if (delta(0) != 0.0 || delta(1) != 0.0) deviation :+ new Noise(newIndex, delta)
    return Value(z0, deviation, roundoff)
  }

  //Note: the following functions assume that you don't feed them some rubbish,
  // i.e. values outside of the domain etc.

  def computeSqrtApproximation(atemp: DD, adtemp: Double, b: DD, bd: Double): (DD, DD, DD) = {
    // Soft policy
    val (a, ad) = if(adtemp < 0.0) (zero, 0.0) else (atemp, adtemp)

    val sqA = Array(sqrtD(ad), 0.0)
    val sqB = Array(sqrtU(bd), 0.0)
    val alpha = div(Array(0.5, 0.0),  sqrt(b))
    val dmin = subDown(sqA, mult(alpha, a))
    val dmax = subUp(sqB, mult(alpha, b))
    return (alpha, dmin, dmax)
  }

  def computeLnApproximation(a: DD, ad: Double, b: DD, bd: Double): (DD, DD, DD) = {
    val alpha =  div(one , b)
    val dmin =  subDown(Array(d1(math.log(ad)), 0.0),   multUp(alpha,a))
    val dmax =  subUp(Array(u1(math.log(bd)), 0.0),  multDown(alpha,b))
    return (alpha, dmin, dmax)
  }

  def computeExpApproximation(a: DD, ad: Double, b: DD, bd: Double): (DD, DD, DD) = {
    val expA = Array(d1(math.exp(ad)), 0.0)
    val expB = Array(u1(math.exp(bd)), 0.0)

    val alpha = expA
    val dmin =  multDown(expA, sub(one, a))
    val dmax =  subUp(expB, multDown(alpha,b))
    return (alpha, dmin, dmax)
  }

  /**
   * Computes a linear approximation for cosine, if the range specified allows this.
   * Return null otherwise.
   * @return (null, null, null) or (alpha, dmin, dmax)
   */
  def computeCosineApproximation(xlo: Double, xloExt: DD, xhi: Double, xhiExt: DD): (DD, DD, DD) = {
    // range reduction.
    val ka =  if(xlo > 0.0) divD(2.0*xlo, Pi_up) else divD(2.0*xlo, Pi_down)
    val kb =  if(xhi > 0.0) divU(2.0*xhi, Pi_down) else divU(2.0*xhi, Pi_up)
    val m = math.floor(ka).toLong
    val n = math.ceil(kb).toLong

    if(n-m < 2.0) { //within patch where linear approximation is applicable and without extremum
      val k = math.floor(xlo/twoP)
      val kD = Array(k, 0.0)
      val r = (m % 4).toLong

      val aExt = multDown(TWO_PI, subDown(divDown(xloExt, TWO_PI), kD))
      val bExt = multUp(TWO_PI, subUp(divUp(xhiExt, TWO_PI), kD))

      val alpha = Array(- math.sin(xlo), 0.0)
      val (dmin: DD, dmax: DD) =
        if(r == 0 || r == -1 || r == 3) { //y > 0
          (subDown(Array(d1(math.cos(xhi)), 0.0), multUp(alpha, bExt)),
            subUp(Array(u1(math.cos(xlo)), 0.0), multDown(alpha, aExt)))
        }
        else { //y < 0
          (subDown(Array(d1(math.cos(xlo)), 0.0), multUp(alpha, aExt)),
            subUp(Array(u1(math.cos(xhi)), 0.0), multDown(alpha, bExt)))
        }
      return (alpha, dmin, dmax)
    }
    else {
      return (null, null, null)
    }
  }

  /**
   * In order to compute the new affine form for cosine, we need to shift the
   * central value.
   * @return (x0_new, roundoff)
   */
  def computeRangeReducedX0(x0: Double, xlo: Double): (DD, DD) = {
    val kD = Array(math.floor(xlo/twoP), 0.0)
    val x0D = Array(x0, 0.0)
    val x0_new = mult(TWO_PI, sub(div(x0D, TWO_PI), kD))
    val x0_down = multDown(TWO_PI, subDown(divDown(x0D, TWO_PI), kD))
    val x0_up = multUp(TWO_PI, subUp(divUp(x0D, TWO_PI), kD))
    var delta = max(subUp(x0_up, x0_new), subUp(x0_new, x0_down))
    return (x0_new, delta)
  }

 /**
   * Computes a linear approximation for sine, if the range specified allows this.
   * Return null otherwise.
   * @return (null, null, null) or (alpha, dmin, dmax)
   */
  def computeSineApproximation(xlo: Double, xloExt: DD, xhi: Double, xhiExt: DD): (DD, DD, DD) = {
    // range reduction.
    val ka = if(xlo > 0.0) divD(2.0*xlo, Pi_up) else divD(2.0*xlo, Pi_down)
    val kb = if(xhi > 0.0) divU(2.0*xhi, Pi_down) else divU(2.0*xhi, Pi_up)
    val m = math.floor(ka).toLong
    val n = math.ceil(kb).toLong

    if(n-m < 2.0) { //within patch where linear approximation is applicable and without extremum
      val k = math.floor(xlo/twoP)
      val kD = Array(k, 0.0)
      val r = (m % 4).toLong
      val aExt = multDown(TWO_PI, subDown(divDown(xloExt, TWO_PI), kD))
      val bExt = multUp(TWO_PI, subUp(divUp(xhiExt, TWO_PI), kD))


      val alpha = Array(math.cos(xlo), 0.0)
      val (dmin: Array[Double], dmax: Array[Double]) =
        if(r == 0 || r == 1 || r == -3) { //y > 0
          (subDown(Array(d1(math.sin(xhi)), 0.0), multUp(alpha, bExt)),
            subUp(Array(u1(math.sin(xlo)), 0.0), multDown(alpha, aExt)))
        }
        else { //y < 0
          (subDown(Array(d1(math.sin(xlo)), 0.0), multUp(alpha, aExt)),
            subUp(Array(u1(math.sin(xhi)), 0.0), multDown(alpha, bExt)))
        }
      return (alpha, dmin, dmax)
    }
    else {
      return (null, null, null)
    }
  }

  /**
   * Computes a linear approximation for tan, if the range specified allows this.
   * Returns null otherwise, or NaN if the result is not even defined on the range.
   * @return (NaN, NaN, NaN) or (null, null, null) or (alpha, dmin, dmax)
   */
  def computeTanApproximation(xlo: Double, xloExt: DD, xhi: Double, xhiExt: DD):
    (DD, DD, DD, DD, DD) = {
    val ka = if(xlo > 0.0) divD(2.0*xlo, Pi_up) else divD(2.0*xlo, Pi_down)
    val kb = if(xhi > 0.0) divU(2.0*xhi, Pi_down) else divU(2.0*xhi, Pi_up)
    val m = math.floor(ka).toLong
    val n = math.ceil(kb).toLong

    var errorMultiplier = zero
    if(n-m < 2.0) {
      val (alpha:Array[Double], dmin: Array[Double], dmax: Array[Double], k:Array[Double]) =
        if(m % 2 == 0) { //y > 0
          val kx = math.floor(2.0 * xlo / math.Pi)
          val kD = Array(kx, 0.0)
          val aExt = multDown(PI_2, subDown(divDown(xloExt, PI_2), kD))
          val bExt = multUp(PI_2, subUp(divUp(xhiExt, PI_2), kD))

          val alphax = Array(1.0 + math.tan(xlo)*math.tan(xlo), 0.0)
          val alpha2 = Array(1.0 + math.tan(xhi)*math.tan(xhi), 0.0)
          errorMultiplier = if(greaterEq(abs(alphax), abs(alpha2))) alphax else alpha2
          (alphax,
            subDown(Array(d1(math.tan(xlo)), 0.0), multUp(alphax, aExt)),
            subUp(Array(u1(math.tan(xhi)), 0.0), multDown(alphax, bExt)), kD)
        }
        else { //y < 0
          val kx = math.floor(2.0 * xlo/math.Pi) + 1.0
          val kD = Array(kx, 0.0)
          val aExt = multDown(PI_2, subDown(divDown(xloExt, PI_2), kD))
          val bExt = multUp(PI_2, subUp(divUp(xhiExt, PI_2), kD))

          val alphax = Array(1.0 + math.tan(xhi)*math.tan(xhi), 0.0)
          val alpha2 = Array(1.0 + math.tan(xlo)*math.tan(xlo), 0.0)
          errorMultiplier = if(greaterEq(abs(alphax), abs(alpha2))) alphax else alpha2
          (alphax,
           subDown(Array(d1(math.tan(xlo)), 0.0), multUp(alphax, aExt)),
           subUp(Array(u1(math.tan(xhi)), 0.0), multDown(alphax, bExt)), kD)
        }
      return (alpha, dmin, dmax, k, errorMultiplier)
    }
    else if(n-m == 2.0) {
      return (null, null, null, null, null)
    }
    else
      return (DDouble.NaN, DDouble.NaN, DDouble.NaN, DDouble.NaN, DDouble.NaN)
  }

  def computeRangeReducedX0Tan(k: DD, x0: Double): (DD, DD) = {
    val x0D = Array(x0, 0.0)
    val x0_new = mult(PI_2, sub(div(x0D, PI_2), k))
    val x0_down = multDown(PI_2, subDown(divDown(x0D, PI_2), k))
    val x0_up = multUp(PI_2, subUp(divUp(x0D, PI_2), k))
    var delta = max(subUp(x0_up, x0_new), subUp(x0_new, x0_down))
    return (x0_new, delta)
  }

  def computeArcCosApproximation(aT: Double, aExtT: DD, bT: Double, bExtT: DD): (DD, DD, DD, DD) = {
    val (b, bExt) = if(bT > 1.0) (1.0, one) else (bT, bExtT)
    val (a, aExt) = if(aT < -1.0) (-1.0, _one) else (aT, aExtT)

    val alphaA = div(_one, sqrt(sub(one, mult(aExt,aExt))))
    val alphaB = div(_one, sqrt(sub(one, mult(bExt,bExt))))

    val alpha: DD = if(a > 0.0) alphaA else alphaB
    val errorMultiplier = if(greaterEq(abs(alphaA), abs(alphaB))) alphaA else alphaB

    val dmin = subDown(Array(d1(math.acos(b)), 0.0), multUp(alpha, bExt))
    val dmax = subUp(Array(u1(math.acos(a)), 0.0), multDown(alpha, aExt))
    return (alpha, dmin, dmax, errorMultiplier)
  }

  def computeArcSinApproximation(aT: Double, aExtT: DD, bT: Double, bExtT: DD): (DD, DD, DD, DD) = {
    val (b, bExt) = if(bT > 1.0) (1.0, one) else (bT, bExtT)
    val (a, aExt) = if(aT < -1.0) (-1.0, _one) else (aT, aExtT)

    val alphaA = div(one, sqrt(sub(one, mult(aExt,aExt))))
    val alphaB = div(one, sqrt(sub(one, mult(bExt,bExt))))

    val alpha: DD = if(a > 0.0) alphaA else alphaB
    val errorMultiplier = if(greaterEq(abs(alphaA), abs(alphaB))) alphaA else alphaB

    val dmin = subDown(Array(d1(math.asin(a)), 0.0), multUp(alpha, aExt))
    val dmax = subUp(Array(u1(math.asin(b)), 0.0), multDown(alpha, bExt))
    return (alpha, dmin, dmax, errorMultiplier)
  }

  def computeArcTanApproximation(a: Double, aExt: DD, b: Double, bExt: DD): (DD, DD, DD, DD) = {
    val alphaA = div(one , add(one , mult(aExt, aExt)))
    val alphaB = div(one , add(one , mult(bExt, bExt)))

    val alpha: Array[Double] = if(a > 0.0) alphaA  else alphaB
    val errorMultiplier = if(greaterEq(abs(alphaA), abs(alphaB))) alphaA else alphaB

    val dmin = subDown(Array(d1(math.atan(b)), 0.0), multUp(alpha, bExt))
    val dmax = subUp(Array(u1(math.atan(a)), 0.0), multDown(alpha, aExt))
    return (alpha, dmin, dmax, errorMultiplier)
  }
}
