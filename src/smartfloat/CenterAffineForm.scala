package ceres.smartfloat

import AffineArithmetic._
import ceres.common.DDouble
import DDouble._
import scala.Double.{PositiveInfinity => PlusInf}
import scala.Double.{NegativeInfinity => MinusInf}
import scala.Double.{MaxValue, MinValue, NaN}
import ceres.common.{DirectedRounding => DirRound}
import ceres.common.{Interval, NormalInterval, EmptyInterval}

import math.{min => mmin, max => mmax, abs => mabs}
//So they don't get confused...
import ceres.common.DirectedRounding.{multDown => multD, multUp => multU, divUp => divU,
      divDown => divD, subDown => subD, subUp => subU, addUp => addU, addDown => addD,
      sqrtDown => sqrtD, sqrtUp => sqrtU}
import ceres.common.DirectedRounding.{down1 => d1, up1 => u1, nextDown}


case class CenterForm(x0: Double, var xnoise: Queue) extends AbstractAForm {
  import AffineForm._
  import AffineUtils._
  type DD = Array[Double]

  def this(f: Double) = {
     this(f, if(DirRound.isExact(f)) Queue.empty
            else new Queue(new Noise(AffineForm.newIndex, DirRound.roundOff(f))))
  }


  if(xnoise.size > maxNoiseCount) {
    xnoise = packingStandard(xnoise)
  }

  assert(xnoise.size <= maxNoiseCount, "packing did not work "+xnoise.size+ " ! \n" + xnoise.toStringVertical)
  assert(xnoise.isSorted, "noise terms not sorted! " + xnoise)


  def unary_-(): AffineForm = new CenterForm(-x0, negate(xnoise))


  def +(y: AffineForm): AffineForm = y match {
    case CenterForm(y0, ynoise) => plus(x0, xnoise, y0, ynoise) match {
        case Value(z0, znoise, roundoff) => return new CenterForm(z0, znoise)
        case All => return FullForm
        case Nothing => return EmptyForm
      }
    case EmptyForm | FullForm => return y
    case _ => throw new IncompatibleAffineFormException("CenterForm vs. " + y.getClass)
  }

  def -(y: AffineForm): AffineForm = y match {
    case CenterForm(y0, ynoise) => minus(x0, xnoise, y0, ynoise) match {
        case Value(z0, znoise, roundoff) => return new CenterForm(z0, znoise)
        case All => return FullForm
        case Nothing => return EmptyForm
      }
    case EmptyForm | FullForm => return y
    case _ => throw new IncompatibleAffineFormException("CenterForm vs. " + y.getClass)
  }

  // This will need a 'hint' for centering...
  def *(y: AffineForm): AffineForm = y match {
    case CenterForm(y0, ynoise) =>
      multiply(x0, xnoise, this.radiusExt, y0, ynoise, y.radiusExt) match {
        case Value(z0, znoise, roundoff) => return new CenterForm(z0, znoise)
        case All => return FullForm
        case Nothing => return EmptyForm
      }
    case EmptyForm | FullForm => return y
    case _ => throw new IncompatibleAffineFormException("CenterForm vs. " + y.getClass)
  }

  def /(y: AffineForm): AffineForm = y match {
    case CenterForm(y0, ynoise) =>
      val (yloD, yhiD) = (y.interval.xlo, y.interval.xhi)
      val (ylo: DD, yhi: DD) = y.intervalExt
      if(yloD <= 0.0 && yhiD >= 0.0) return FullForm  //division by zero

      val inverse =
        if(ynoise.size == 0.0) { //exact
          val inv = 1.0/y0
          if(divD(1.0, y0) == divU(1.0, y0)) CenterForm(inv, Queue.empty) //exact division
          else CenterForm(inv, new Queue(new Noise(newIndex, DirRound.roundOff(inv))))
        }
        else {
          /* Calculate the inverse. */
          val (alpha, dmin, dmax) = computeInverse(yloD, yhiD, ylo, yhi)
          var (zeta: Array[Double], rdoff: Array[Double]) = computeCentralZeta(1.0/y0, alpha, y0)
          if(yloD < 0.0) zeta = Array(-zeta(0), -zeta(1))
          val delta = addUp(computeDelta(zeta, dmin, dmax), rdoff)
          symmetricUnary(1.0/y0, ynoise, alpha, zeta, delta)
      }
      inverse match {
        case CenterForm(i0, inoise) =>
          multiply(x0, xnoise, this.radiusExt, i0, inoise, inverse.radiusExt, this.x0/y0) match {
            case Value(z0, znoise, roundoff) => return new CenterForm(z0, znoise)
            case All => return FullForm
            case Nothing => return EmptyForm
          }
        case EmptyForm | FullForm => return inverse
      }
    case EmptyForm | FullForm => return y
    case _ => throw new IncompatibleAffineFormException("CenterForm vs. " + y.getClass)
  }

  def squareRoot: AffineForm = {
    var (ad, bd) = (interval.xlo, interval.xhi)
    if(bd < 0.0) return EmptyForm
    if(bd == PlusInf) return FullForm

    if(xnoise.size == 0) { //exact
      val sqrt = math.sqrt(x0)
      if(sqrtD(x0) == sqrtU(x0)) return new CenterForm(sqrt, Queue.empty)
      else
        return new CenterForm(sqrt, new Queue(new Noise(newIndex, DirRound.roundOff(sqrt))))
    }

    var (a: DD, b: DD) = intervalExt
    val (alpha, dmin, dmax) = computeSqrtApproximation(a, ad, b, bd)
    var (zeta, rdoff: DD) = computeCentralZeta(math.sqrt(x0), alpha, x0)
    val delta = addUp(computeDelta(zeta, dmin, dmax), rdoff)
    return symmetricUnary(math.sqrt(x0), xnoise, alpha, zeta, delta)
  }

  def ln: AffineForm = {
    if(xnoise.size == 0) computeExactResult(x0, math.log)
    val (ad, bd) = (interval.xlo, interval.xhi)

    if(ad <= 0.0 || bd < 0.0 || bd == PlusInf) return FullForm
    var (a: DD, b: DD) = intervalExt
    val (alpha, dmin, dmax) = computeLnApproximation(a, ad, b, bd)
    var (zeta: DD, rdoff: DD) = computeCentralZeta(math.log(x0), alpha, x0)
    val delta = addUp(computeDelta(zeta, dmin, dmax), rdoff)
    return symmetricUnary(math.log(x0), xnoise, alpha, zeta, delta)
  }

  def exponential: AffineForm = {
    if(xnoise.size == 0) computeExactResult(x0, math.exp)
    if(interval.xhi == PlusInf) return FullForm

    var (a: DD, b: DD) = intervalExt
    val (alpha, dmin, dmax) = computeExpApproximation(a, interval.xlo, b, interval.xhi)
    var (zeta: DD, rdoff: DD) = computeCentralZeta(math.exp(x0), alpha, x0)
    val delta = addUp(computeDelta(zeta, dmin, dmax), rdoff)
    return symmetricUnary(math.exp(x0), xnoise, alpha, zeta, delta)
  }

  def cosine: AffineForm = {
    //trigCount += 1
    if((radiusExt(0) > TWO_PI(0)) ||
      (radiusExt(0) == TWO_PI(0) && radiusExt(1) >= TWO_PI(1))) {
      return new CenterForm(0.0, new Queue(new Noise(AffineForm.newIndex, 1.0)))
    }
    if(xnoise.size == 0) computeExactResult(x0, math.cos)

    //let's use the symmetry, there is something funny going on for [-2.8, -0.5]
    if(interval.xhi < 0.0) return (-this).cosine

    var (xlo, xhi) = (interval.xlo, interval.xhi)
    var (xloExt, xhiExt) = (intervalExt._1, intervalExt._2)
    val (alpha, dmin, dmax) = computeCosineApproximation(xlo, xloExt, xhi, xhiExt)
    if (alpha == null) //non-applicable range
      return interval2affine(interval.cosine, math.cos(x0))

    var (x0_new, delta) = computeRangeReducedX0(x0, xlo)
    var (zeta: DD, rdoff: DD) = computeCentralZeta(math.cos(x0), alpha, x0_new(0) + x0_new(1))
    delta = addUp(computeDelta(zeta, dmin, dmax), rdoff)
    return symmetricUnary(math.cos(x0), xnoise, alpha, zeta, delta)
  }


  def sine: AffineForm = {
    if((radiusExt(0) > TWO_PI(0)) ||
      (radiusExt(0) == TWO_PI(0) && radiusExt(1) >= TWO_PI(1))) {
      return new CenterForm(0.0, new Queue(new Noise(AffineForm.newIndex, 1.0)))
    }
    if(xnoise.size == 0) computeExactResult(x0, math.sin)
    val (xlo, xhi) = (interval.xlo, interval.xhi)
    val (xloExt, xhiExt) = (intervalExt._1, intervalExt._2)

    val (alpha, dmin, dmax) = computeSineApproximation(xlo, xloExt, xhi, xhiExt)
    if (alpha == null)//non-applicable range
      return interval2affine(interval.sine, math.sin(x0))

    var (x0_new, delta) = computeRangeReducedX0(x0, xlo)
    var (zeta: DD, rdoff: DD) = computeCentralZeta(math.sin(x0), alpha, x0_new(0) + x0_new(1))
    delta = addUp(computeDelta(zeta, dmin, dmax), rdoff)
    return symmetricUnary(math.sin(x0), xnoise, alpha, zeta, delta)
  }

  def tangent: AffineForm = {
    if((radiusExt(0) > PI(0)) || (radiusExt(0) == PI(0) && radiusExt(1) >= PI(1))) {
      return FullForm
    }
    if(xnoise.size == 0) computeExactResult(x0, math.tan)
    val (xlo, xhi) = (interval.xlo, interval.xhi)
    val (xloExt, xhiExt) = (intervalExt._1, intervalExt._2)

    val (alpha, dmin, dmax, k, xxx) = computeTanApproximation(xlo, xloExt, xhi, xhiExt)
    if (alpha(0) != alpha(0))
      return FullForm
    else if (alpha == null)
      return interval2affine(interval.tangent, math.tan(x0))

    var (x0_new, delta) = computeRangeReducedX0Tan(k, x0)
    var (zeta: DD, rdoff: DD) = computeCentralZeta(math.tan(x0), alpha, x0_new(0) + x0_new(1))
    delta = addUp(computeDelta(zeta, dmin, dmax), rdoff)
    return symmetricUnary(math.tan(x0), xnoise, alpha, zeta, delta)
  }

  def arccosine: AffineForm = {
    var (a, b) = (interval.xlo, interval.xhi)
    var (aExt, bExt) = (intervalExt._1, intervalExt._2)

    if(b < -1.0 || a > 1.0) return EmptyForm

    if(a == b) computeExactResult(x0, math.acos)
    if(a < 0.0 && b > 0.0)  //straddling turning point
      return interval2affine(interval.arccosine, math.acos(x0))
    val (alpha, dmin, dmax, xxx) = computeArcCosApproximation(a, aExt, b, bExt)

    var (zeta: DD, rdoff: DD) = computeCentralZeta(math.acos(x0), alpha, x0)
    val delta = addUp(computeDelta(zeta, dmin, dmax), rdoff)
    return symmetricUnary(math.acos(x0), xnoise, alpha, zeta, delta)
  }


  def arcsine: AffineForm = {
     var (a, b) = (interval.xlo, interval.xhi)
    var (aExt, bExt) = (intervalExt._1, intervalExt._2)

    if(b < -1.0 || a > 1.0) return EmptyForm

    if(a == b) computeExactResult(x0, math.asin)
    if(a < 0.0 && b > 0.0) //straddling turning point
      return interval2affine(interval.arcsine, math.asin(x0))

    val (alpha, dmin, dmax, xxx) = computeArcSinApproximation(a, aExt, b, bExt)
    var (zeta: DD, rdoff: DD) = computeCentralZeta(math.asin(x0), alpha, x0)
    var delta = addUp(computeDelta(zeta, dmin, dmax), rdoff)
    return symmetricUnary(math.asin(x0), xnoise, alpha, zeta, delta)
  }


  def arctangent: AffineForm = {
    var (a, b) = (interval.xlo, interval.xhi)
    var (aExt, bExt) = (intervalExt._1, intervalExt._2)

    if(a == b) computeExactResult(x0, math.atan)
    if(a < 0.0 && b > 0.0)  //straddling turning point
      return interval2affine(interval.arctangent, math.atan(x0))
    val (alpha, dmin, dmax, xxx) = computeArcTanApproximation(a, aExt, b, bExt)
    var (zeta: DD, rdoff: DD) = computeCentralZeta(math.atan(x0), alpha, x0)
    var delta = addUp(computeDelta(zeta, dmin, dmax), rdoff)
    return symmetricUnary(math.atan(x0), xnoise, alpha, zeta, delta)
  }


  private def symmetricUnary(z0: Double, xnoise: Queue, alpha: DD, zeta: DD,
    delta: DD) : AffineForm = {
    //unaryCount +=1
    var d = delta
    var deviation: Queue = new Queue()

    val iter = xnoise.getIterator
    while(iter.hasNext) {
      val xi = iter.next
      val xiValue = xi.value
      val zi =  mult(alpha(0), alpha(1) , xiValue(0), xiValue(1))
      d =  addUp(d,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0) deviation :+ new Noise(xi.index, zi)
    }
    if(d(0) == PlusInf) return FullForm
    if(d(0) != 0.0 || d(1) != 0.0) deviation :+ new Noise(newIndex, d)

    return new CenterForm(z0, deviation)
  }

  private def interval2affine(x: Interval, middle: Double): AffineForm =
  x match {
    case EmptyInterval => EmptyForm
    case NormalInterval(xloD, xhiD) =>
      val xlo = Array(xloD, 0.0)
      val xhi = Array(xhiD, 0.0)
      if(abs(xlo)(0) == PlusInf || abs(xhi)(0) == PlusInf) FullForm
      else {
        val low = subUp(Array(middle, 0.0), xlo)
        val high = subUp(xhi, Array(middle, 0.0))
        val xk = max(abs(low), abs(high))
        return new CenterForm(middle, new Queue(new Noise(newIndex, xk)))
      }
  }

  private def computeExactResult(x: Double, f: (Double) => Double): CenterForm = {
    val fncVal = f(x)
    return new CenterForm(fncVal,
        new Queue(new Noise(newIndex, DirRound.roundOff1(fncVal))))
  }


}
