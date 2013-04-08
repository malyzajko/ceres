package ceres.smartfloat

import scala.collection.immutable.HashMap
import java.lang.Math.{nextUp}
import scala.Double.{PositiveInfinity => PlusInf}
import scala.Double.{NegativeInfinity => MinusInf}
import scala.Double.{MaxValue, MinValue, NaN}
import ceres.common.{DirectedRounding => DirRound}
import ceres.common.{Interval, NormalInterval, EmptyInterval}

import AffineArithmetic._
import ceres.common.DDouble
import DDouble._
import math.{min => mmin, max => mmax, abs => mabs}
//So they don't get confused...
import ceres.common.DirectedRounding.{multDown => multD, multUp => multU, divUp => divU,
      divDown => divD, subDown => subD, subUp => subU, addUp => addU, addDown => addD,
      sqrtDown => sqrtD, sqrtUp => sqrtU}
import ceres.common.DirectedRounding.{down1 => d1, up1 => u1, nextDown}
import AffineUtils._
import AffineForm._

object GeneralForm {
 /*
    We want to add an error to the estimate affine form.
  */
  def addError(xForm:AffineForm, errorForm: AffineForm): AffineForm = {
    (xForm, errorForm) match {
      case (x: GeneralForm, e: GeneralForm) =>
        val radNoise: DD = sumQueue(e.xerror)
        val errorToAdd = addUp(math.abs(e.x0), 0.0, radNoise(0), radNoise(1))
        val rdOffToAdd = sumQueue(e.xerror)

        val newNoiseQueue = x.xnoise.copy
        newNoiseQueue :+ new Noise(newIndex, errorToAdd)
        val newErrorQueue = x.xerror.copy
        newErrorQueue :+ new Noise(newIndex, rdOffToAdd)

        return new GeneralForm(x.x0, newNoiseQueue, newErrorQueue)

      //FIXME: this may not be quite right
      case (FullForm, _) => return FullForm
      case (EmptyForm, _) => return EmptyForm
      case (_, FullForm) => return FullForm
      case (_, EmptyForm) => return EmptyForm
      case _=> throw IncompatibleAffineFormException(errorForm.getClass.toString)
    }
  }

  val verbose = false
}


/**
 * This affine form represents a range of floating-point numbers.
 */
case class GeneralForm(x0: Double, var xnoise: Queue, var xerror: Queue) extends AbstractAForm {
  import GeneralForm.verbose
  type DD = Array[Double]

  def this(f: Double) = {
    this(f, Queue.empty, Queue.empty)
    if(!DirRound.isExact(f)) {
      xnoise :+ new Noise(AffineForm.newIndex, DirRound.roundOff(f))
      xerror :+ new Noise(AffineForm.newIndex, DirRound.roundOff(f))
    }
  }

  def this(f: Double, un: Double) = {
    this(f, new Queue(new Uncertainty(AffineForm.newIndex, un)), Queue.empty)
    xerror :+ new Noise(AffineForm.newIndex, getRoundoff(f, xnoise))
  }

  if(xnoise.size > maxNoiseCount) {
    val before = xnoise.size
    xnoise = packingUncertain(xnoise)
    if(printPackingInfo) println("---------> EstAForm, packed from: " +before + " to: " + xnoise.size)
  }

  if(xerror.size > maxNoiseCount) {
    xerror = packingStandard(xerror)
  }



  if(printDebug) println("new EstForm created. no.xnoise:" + xnoise.size + " , no.xerror:" + xerror.size)

  assert(xnoise.size <= maxNoiseCount, "packing did not work for noise "+xnoise.size +
    " ! Try to increase the max number of noise symbols!")

  assert(xerror.size <= maxNoiseCount, "packing did not work for errors"+xerror.size +
    " ! Try to increase the max number of noise symbols!")

  assert(xnoise.isSorted, "noise terms not sorted!")

  def maxRoundoff: Double = {
    val sum = sumQueue(xerror)
    addU(sum(0), sum(1))
  }

  override def toString: String =
    return x0 + " " + formatQueue(xnoise) + " ~~ " + formatQueue(xerror)


  def +(other: AffineForm): AffineForm = other match {
    case GeneralForm(y0, ynoise, yerror) => {
      if(printDebug) println("add")
      var z0 = x0 + y0
      var (deviation, delta) = addQueuesUncertain(xnoise, ynoise)
      //println("delta " + delta(0))
      if(z0 == PlusInf || z0 == MinusInf || delta(0) == PlusInf) {
        if(printFailed) println("add failed");
        return FullForm
      }

      val roundoff = getRoundoff(z0, deviation)
      var (errDev, errDelta) = addQueues(xerror, yerror)

      delta = addUp(delta, roundoff)
      errDelta = addUp(errDelta, roundoff)

      if (notZero(delta)) deviation :+ new Noise(newIndex, delta)
      if (notZero(errDelta)) errDev :+ new Noise(newIndex, errDelta)

      if (verbose) println("\naddition, roundoff: " + (roundoff(0) + roundoff(1)))
      return GeneralForm(z0, deviation, errDev)
    }
    case EmptyForm => return other
    case FullForm => return other
    case _ => throw new IncompatibleAffineFormException("GeneralForm vs. " + other.getClass)
  }


  def -(other: AffineForm): AffineForm = other match {
    case GeneralForm(y0, ynoise, yerror) => {
      if(printDebug) println("sub")
      var z0 = x0 - y0
      var (deviation, delta) = subtractQueuesUncertain(xnoise, ynoise)
      if(z0 == PlusInf || z0 == MinusInf || delta(0) == PlusInf) {
        if(printFailed) println("sub failed");
        return FullForm
      }

      val roundoff = getRoundoff(z0, deviation)
      var (errDev, errDelta) = subtractQueues(xerror, yerror)

      delta = addUp(delta, roundoff)
      errDelta = addUp(errDelta, roundoff)

      if (notZero(delta)) deviation :+ new Noise(newIndex, delta)
      if (notZero(errDelta)) errDev :+ new Noise(newIndex, errDelta)
      if (verbose) println("\nsubtraction, roundoff: " + (roundoff(0) + roundoff(1)))
      return new GeneralForm(z0, deviation, errDev)
    }
    case EmptyForm => return other
    case FullForm => return other
    case _ => throw new IncompatibleAffineFormException("GeneralForm vs. " + other.getClass)
  }


  def unary_-(): AffineForm = {
    var deviation = negate(xnoise)
    var errDev = negate(xerror)
    GeneralForm(-x0, deviation, errDev)
  }


  def *(other: AffineForm): AffineForm = other match {
    case GeneralForm(y0, ynoise, yerror) => {
      var z0 = x0 * y0
      if(z0 == PlusInf || z0 == MinusInf) {
        return FullForm
      }
      var (deviation, delta) = multiplyQueuesUncertain(x0, xnoise, y0, ynoise)
      deviation = computeEtaSmart(deviation, xnoise, ynoise)

      val roundoff = getRoundoff(z0, deviation)
      delta = addUp(delta, roundoff)
      if (notZero(delta)) deviation :+ new Noise(newIndex, delta)

      //ROUNDOFF ERRORS
      var (errDev, errDelta) = multiplyQueues(x0, xerror, y0, yerror)  //linear part
      val ex = sumQueue(xerror)
      val ey = sumQueue(yerror)
      errDelta = addUp(errDelta, multUp(this.radiusExt, ey))
      errDelta = addUp(errDelta, multUp(other.radiusExt, ex))
      errDelta = addUp(errDelta, multUp(ex, ey))
      errDelta = addUp(errDelta, roundoff)
      if (notZero(errDelta)) errDev :+ new Noise(newIndex, errDelta)
      if (verbose) println("\nmultiplication, roundoff: " + (roundoff(0) + roundoff(1)))
      return GeneralForm(z0, deviation, errDev)
    }
    case EmptyForm => return other
    case FullForm => return other
    case _ => throw new IncompatibleAffineFormException("GeneralForm vs. " + other.getClass)
  }


  def /(other: AffineForm): AffineForm = other match {
    case y: GeneralForm =>
      val (yloD, yhiD) = (y.smartInterval.xlo, y.smartInterval.xhi)
      val (ylo: Array[Double], yhi: Array[Double]) = y.smartIntervalExt

      if(yloD <= 0.0 && yhiD >= 0.0) {
        return FullForm
      }  //division by zero

      if(y.xnoise.size == 0.0) { //exact
        val inv = 1.0/y.x0
        if(divD(1.0, y.x0) == divU(1.0, y.x0))   //exact division
          return this * GeneralForm(inv, Queue.empty, Queue.empty)
        else {
          val r = DirRound.roundOff(inv)
          return this * GeneralForm(inv, new Queue(new Noise(newIndex, r)),
            new Queue(new Noise(newIndex, r)))
        }
      }

      /* Calculate the inverse. */
      val a = min(abs(ylo), abs(yhi))
      val b = max(abs(ylo), abs(yhi))

      val ad = mmin(mabs(yloD), mabs(yhiD))
      val bd = mmax(mabs(yloD), mabs(yhiD))
      val alpha =  div(_one,  mult(b,b))  //no rounding, it's intentional

      val errorMultiplier = div(_one, mult(a,a))

      val dmax =  subUp(Array(divU(1.0, ad), 0.0) ,  multDown(alpha, a))
      val dmin =  subDown(Array(divD(1.0, bd), 0.0),  multUp(alpha, b))

      var zeta = computeZeta(dmin, dmax)
      if(yloD < 0.0) zeta = Array(-zeta(0), -zeta(1))
      val delta = computeDelta(zeta, dmin, dmax)

      val inverse = unaryOp(y.x0, y.xnoise, alpha, zeta, delta, y.xerror, true, errorMultiplier)
      return this * inverse

    case EmptyForm => return other
    case FullForm => return other
    case _ => assert(false, "incompatible affine forms"); return null;

  }


  def squareRoot: AffineForm = {
    var (ad, bd) = (smartInterval.xlo, smartInterval.xhi)
    if(bd < 0.0) return EmptyForm
    if(bd == PlusInf) return interval2affine(this.smartInterval.squareRoot, true)

    if(xnoise.size == 0) { //exact
      val sqrt = math.sqrt(x0)
      val r = DirRound.roundOff(sqrt)
      return GeneralForm(sqrt, new Queue(new Noise(newIndex, r)), new Queue(new Noise(newIndex, r)))
    }

    var (a: DD, b: DD) = smartIntervalExt
    val (alpha, dmin, dmax) = computeSqrtApproximation(a, ad, b, bd)
    val errorMultiplier = div(Array(0.5, 0.0),  sqrt(a))  //wir sind gea* wenn a == 0...
    val zeta = computeZeta(dmin, dmax)
    val delta = computeDelta(zeta, dmin, dmax)
    return unaryOp(x0, xnoise, alpha, zeta, delta, xerror, true, errorMultiplier)
  }


  def ln: AffineForm = {
    if(xnoise.size == 0) return computeExactResult(x0, math.log )

    val (ad, bd) = (smartInterval.xlo, smartInterval.xhi)
    if(ad <= 0.0 || bd < 0.0 || bd == PlusInf) return FullForm

    var (a: DD, b: DD) = smartIntervalExt
    val (alpha, dmin, dmax) = computeLnApproximation(a, ad, b, bd)
    val errorMultiplier = div(one , a)
    var zeta = computeZeta(dmin, dmax)
    val delta = computeDelta(zeta, dmin, dmax)
    return unaryOp(x0, xnoise, alpha, zeta, delta, xerror, false, errorMultiplier)
  }


  def exponential: AffineForm = {
    if(xnoise.size == 0) return computeExactResult(x0, math.exp )
    var (a: DD, b: DD) = smartIntervalExt

    if(b(0) == PlusInf) return FullForm

    var (ad: Double, bd: Double) = (smartInterval.xlo, smartInterval.xhi)
    val (alpha, dmin, dmax) = computeExpApproximation(a, ad, b, bd)
    val errorMultiplier = Array(u1(math.exp(bd)), 0.0)
    var zeta = computeZeta(dmin, dmax)
    val delta = computeDelta(zeta, dmin, dmax)
    return unaryOp(x0, xnoise, alpha, zeta, delta, xerror, false, errorMultiplier)
  }



  def cosine: AffineForm = {
    if((radiusExt(0) > TWO_PI(0)) ||
      (radiusExt(0) == TWO_PI(0) && radiusExt(1) >= TWO_PI(1))) {
      return GeneralForm(0.0,
        new Queue(new Noise(newIndex, 1.0)),
        new Queue(new Noise(newIndex, DirRound.roundOff1(1.0))))
    }

    if(xnoise.size == 0) return computeExactResult(x0, math.cos )

    var (xlo, xhi) = (smartInterval.xlo, smartInterval.xhi)
    var (xloExt, xhiExt) = smartIntervalExt

    //let's use the symmetry, there is something funny going on for [-2.8, -0.5]
    if(xhi < 0.0) return (-this).cosine

    val (alpha, dmin, dmax) = computeCosineApproximation(xlo, xloExt, xhi, xhiExt)
    if (alpha == null)
      return interval2affine(smartInterval.cosine, false)

    val alpha2 = Array(- math.sin(xhi), 0.0)
    val errorMultiplier = if(greaterEq(abs(alpha), abs(alpha2))) alpha else alpha2

    var zeta = computeZeta(dmin, dmax)
    var delta = computeDelta(zeta, dmin, dmax)

    val (x0_new, rdoff) = computeRangeReducedX0(x0, xlo)
    delta = addUp(delta, rdoff)
    return unaryOp(x0_new, xnoise, alpha, zeta, delta, xerror, false, errorMultiplier)
  }


  def sine: AffineForm = {
    if((radiusExt(0) > TWO_PI(0)) ||
      (radiusExt(0) == TWO_PI(0) && radiusExt(1) >= TWO_PI(1))) {
      return GeneralForm(0.0,
        new Queue(new Noise(AffineForm.newIndex, 1.0)),
        new Queue(new Noise(newIndex, DirRound.roundOff1(1.0))))
    }

    if(xnoise.size == 0) return computeExactResult(x0, math.sin )

    var (xlo, xhi) = (smartInterval.xlo, smartInterval.xhi)
    var (xloExt, xhiExt) = smartIntervalExt

    //use symmetry, cause there is something funny going on for [-2.9, -0.5]
    if(xhi < 0.0) return -((-this).sine)

    val (alpha, dmin, dmax) = computeSineApproximation(xlo, xloExt, xhi, xhiExt)
    if (alpha == null)
      return interval2affine(smartInterval.sine, false)

    val alpha2 = Array(math.cos(xhi), 0.0)
    val errorMultiplier = if(greaterEq(abs(alpha), abs(alpha2))) alpha else alpha2
    var zeta = computeZeta(dmin, dmax)
    var delta = computeDelta(zeta, dmin, dmax)

    val (x0_new, rdoff) = computeRangeReducedX0(x0, xlo)
    delta = addUp(delta, rdoff)
    return unaryOp(x0_new, xnoise, alpha, zeta, delta, xerror, false, errorMultiplier)
  }

  def tangent: AffineForm = {
    if((radiusExt(0) > PI(0)) || (radiusExt(0) == PI(0) && radiusExt(1) >= PI(1))) {
      return FullForm
    }
    if(xnoise.size == 0) return computeExactResult(x0, math.tan )

    var (xlo, xhi) = (smartInterval.xlo, smartInterval.xhi)
    var (xloExt, xhiExt) = smartIntervalExt

    val (alpha, dmin, dmax, k, errorMultiplier) =
        computeTanApproximation(xlo, xloExt, xhi, xhiExt)
    if (alpha(0) != alpha(0)) return FullForm
    else if (alpha == null) return interval2affine(smartInterval.tangent, false)

    var zeta = computeZeta(dmin, dmax)
    var delta = computeDelta(zeta, dmin, dmax)

    val (x0_new, rdoff) = computeRangeReducedX0Tan(k, x0)
    delta = addUp(delta, rdoff)
    return unaryOp(x0_new, xnoise, alpha, zeta, delta, xerror, false, errorMultiplier)
  }




  def arccosine: AffineForm = {
    var (a, b) = (smartInterval.xlo, smartInterval.xhi)
    var (aExt, bExt) = smartIntervalExt

    if(b < -1.0 || a > 1.0) return EmptyForm
    if(a == b) return computeExactResult(x0, math.acos )
    //straddling turning point
    if(a < 0.0 && b > 0.0) {
      return interval2affine(smartInterval.arccosine, false)
    }

    //again, something funny going on here
    if(a > 0.8) return -(-this).arccosine + new GeneralForm(math.Pi)

    val (alpha, dmin, dmax, errorMultiplier) =
      computeArcCosApproximation(a, aExt, b, bExt)

    var zeta = computeZeta(dmin, dmax)
    val delta = computeDelta(zeta, dmin, dmax)
    return unaryOp(x0, xnoise, alpha, zeta, delta, xerror, false, errorMultiplier)
  }


  def arcsine: AffineForm = {
    var (a, b) = (smartInterval.xlo, smartInterval.xhi)
    var (aExt, bExt) = smartIntervalExt

    if(b < -1.0 || a > 1.0) return EmptyForm
    if(a == b) return computeExactResult(x0, math.asin )
    //straddling turning point
    if(a < 0.0 && b > 0.0) {
      return interval2affine(smartInterval.arcsine, false)
    }

    val (alpha, dmin, dmax, errorMultiplier) =
      computeArcSinApproximation(a, aExt, b, bExt)

    var zeta = computeZeta(dmin, dmax)
    val delta = computeDelta(zeta, dmin, dmax)
    return unaryOp(x0, xnoise, alpha, zeta, delta, xerror, false, errorMultiplier)
  }


  def arctangent: AffineForm = {
    var (a, b) = (smartInterval.xlo, smartInterval.xhi)
    var (aExt, bExt) = smartIntervalExt

    if(a == b) return computeExactResult(x0, math.atan )
    //straddling turning point
    if(a < 0.0 && b > 0.0) {
      return interval2affine(smartInterval.arctangent, false)
    }

    val (alpha, dmin, dmax, errorMultiplier) =
      computeArcTanApproximation(a, aExt, b, bExt)

    var zeta = computeZeta(dmin, dmax)
    val delta = computeDelta(zeta, dmin, dmax)
    return unaryOp(x0, xnoise, alpha, zeta, delta, xerror, false, errorMultiplier)
  }

 private def computeExactResult(x: Double, f: (Double) => Double): AffineForm = {
    val acos = f(x)
    val r = DirRound.roundOff1(acos)
    return GeneralForm(acos,
      new Queue(new Noise(newIndex, r)), new Queue(new Noise(newIndex, r)))
  }


  /**
   * Computes a new affine form from alpha * x + zeta.
   * Note, that this computation uses extended precision throughout.
   * The simple double central value is being converted at the end.
   */
  private def unaryOp(x0: Double, xnoise: Queue, alpha: DD, zeta: DD, delta: DD,
    xerror:Queue, exact: Boolean, errorMultiplier: DD) : AffineForm =
      unaryOp(Array(x0, 0.0), xnoise, alpha, zeta, delta, xerror, exact, errorMultiplier)


  private def unaryOp(x0: DD, xnoise: Queue, alpha: DD, zeta: DD, delta: DD,
    xerror:Queue, exact: Boolean, errMultiplier: DD) : AffineForm = {

    val z0 = add(mult(alpha, x0), zeta)
    var d: DD = rdOffAtimesBplusC(alpha, x0, zeta)
    val z0d = z0(0) + z0(1)
    d = addUp(d, Array(DirRound.roundOff(z0d), 0.0))
    d = addUp(d, delta)

    var deviation: Queue = new Queue()

    val iter = xnoise.getIterator
    while(iter.hasNext) {
      val xi = iter.next
      val zi =  mult(alpha , xi.value)
      d =  addUp(d,  rdOff(zi))
      if(notZero(zi))
        xi match {
          case n:Noise => deviation += new Noise(xi.index, zi, xi.comesFrom)
          case u:Uncertainty => deviation += new Uncertainty(xi.index, zi, xi.comesFrom)
        }
    }
    if(d(0) == PlusInf) {
      if(printFailed) println("unaryOp failed");
      return FullForm
    }
    if(notZero(d)) deviation :+ new Noise(newIndex, d)

    val roundoff = if(exact) getRoundoff(z0d, deviation) else getRoundoff1(z0d, deviation)
    var (errDev, errDelta) = multiplyErrors(xerror, errMultiplier)
    errDelta = addUp(errDelta, roundoff)
    if (notZero(errDelta)) errDev :+ new Noise(newIndex, errDelta)
    return GeneralForm(z0d, deviation, errDev)
  }

  private def multiplyErrors(q: Queue, alpha: DD): (Queue, DD) = {
    var deviation = new Queue
    var d = zero
    val iter = q.getIterator
    while(iter.hasNext) {
      val xi = iter.next
      val zi =  mult(alpha , xi.value)
      d =  addUp(d,  rdOff(zi))
      if(notZero(zi)) deviation :+ new Noise(xi.index, zi, xi.comesFrom)
    }
    return (deviation, d)
  }


   private def interval2affine(x: Interval, exact: Boolean): AffineForm = x match {
    case EmptyInterval => EmptyForm
    case NormalInterval(xlo, xhi) =>
      if(mabs(xlo) == PlusInf || mabs(xhi) == PlusInf) {
        if(printFailed) println("int2aff failed");
        return FullForm
      }
      else {
        val x0_new = DirRound.divUp(xlo, 2.0) + DirRound.divDown(xhi, 2.0)
        val s =  subUp(xlo, 0.0, xhi, 0.0)
        val xk =  new Queue(new Noise(newIndex, divUp(s(0), s(1), 2.0, 0.0)))

        val r = if(exact) getRoundoff(x0_new, xk) else getRoundoff1(x0_new, xk)
        return new GeneralForm(x0_new, xk, new Queue(new Noise(newIndex, r)))
      }
  }



  //############# QUADRATIC OPTIMIZATION ####################
  lazy val smartMinMaxRadius: (DD, DD) = {
    sumQueueSmart(xnoise)
  }

  lazy val smartIntervalExt: (DD, DD) = {
    val (lo, hi) = smartMinMaxRadius
    (addDown(x0, 0.0, lo(0), lo(1)), addUp(x0, 0.0, hi(0), hi(1)))
  }

  lazy val smartInterval: Interval = {
    val lo = addD(smartIntervalExt._1(0), smartIntervalExt._1(1))
    val hi = addU(smartIntervalExt._2(0), smartIntervalExt._2(1))
    NormalInterval(lo, hi)
  }

  lazy val smartRadius: DD = {
    val (lo, hi) = smartMinMaxRadius
    max(abs(lo), abs(hi))
  }

}
























 
