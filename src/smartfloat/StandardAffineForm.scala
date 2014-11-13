package ceres.smartfloat

import ceres.smartfloat._
import java.lang.Math.{nextUp}
import scala.Double.{PositiveInfinity => PlusInf}
import scala.Double.{NegativeInfinity => MinusInf}
import scala.Double.{MaxValue, MinValue}
import ceres.{DirectedRounding => DirRound}
import ceres.{Interval, NormalInterval, EmptyInterval}

import ceres.DDouble
import DDouble._
import math.{min => mmin, max => mmax, abs => mabs}
//So they don't get confused...
import ceres.DirectedRounding.{multDown => multD, multUp => multU, divUp => divU,
      divDown => divD, subDown => subD, subUp => subU, addUp => addU, addDown => addD,
      sqrtDown => sqrtD, sqrtUp => sqrtU}
import ceres.DirectedRounding.{down1 => d1, up1 => u1, nextDown}

object AForm {
  import AffineForm._
  import AffineUtils._
  
  def computeExactResultStd(x: Double, f: (Double) => Double): AffineForm = {
    val fnc = f(x)
    return new AForm(fnc,
        new Queue(new Noise(newIndex, DirRound.roundOff1(fnc))))
  }
 
 /**
   * Computes a new affine form from alpha * x + zeta.
   */
  private def unaryOp(x0: Double, xnoise: Queue, alpha: Array[Double], zeta: Array[Double], 
    delta: Array[Double]) : AffineForm = 
      unaryOp(Array(x0, 0.0), xnoise, alpha, zeta, delta)
   
  
  private def unaryOp(x0: Array[Double], xnoise: Queue, alpha: Array[Double], zeta: Array[Double], 
    delta: Array[Double]) : AffineForm = {
    
    val z0 = add(mult(alpha, x0), zeta)
    var d: Array[Double] = rdOffAtimesBplusC(alpha, x0, zeta)
    val z0d = z0(0) + z0(1)
    d = addUp(d, Array(DirRound.roundOff(z0d), 0.0))
    d = addUp(d, delta)
    
    var deviation: Queue = new Queue
    
    val iter = xnoise.getIterator
    while(iter.hasNext) {
      val xi = iter.next
      val zi =  mult(alpha , xi.value)
      d =  addUp(d,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0) 
        xi match {
          case n:Noise => deviation += new Noise(xi.index, zi)
          case u:Uncertainty => deviation += new Uncertainty(xi.index, zi)
        }
    }
    if(d(0) == PlusInf) return FullForm
    if(d(0) != 0.0 || d(1) != 0.0)
      return new AForm(z0d, deviation + new Noise(newIndex, d))
    else return new AForm(z0d, deviation)
  }

  private def interval2affine(x: Interval): AffineForm = x match {
    case EmptyInterval => EmptyForm
    case NormalInterval(xlo, xhi) =>
      if(mabs(xlo) == PlusInf || mabs(xhi) == PlusInf) FullForm
      else {
        val x0_new = DirRound.divUp(xlo, 2.0) + DirRound.divDown(xhi, 2.0)
        val s =  subUp(xlo, 0.0, xhi, 0.0)
        val xk =  divUp(s(0), s(1), 2.0, 0.0)
        return new AForm(x0_new, new Queue(new Noise(newIndex, xk)))
      }
  }
}


/**
 * This affine form represents a range of real numbers,
 * computed with floating-points.
 */
case class AForm(x0: Double, var xnoise: Queue) extends AbstractAForm {
  import AffineForm._
  import AForm._
  import AffineUtils._


  //only central value
  def this(f: Double) = {
     this(f, if(DirRound.isExact(f)) Queue.empty
            else new Queue(new Noise(AffineForm.newIndex, DirRound.roundOff(f))))
  }

  //only new noise
  def this(ad: Array[Double]) = {
    this(0.0, new Queue(Noise(AffineForm.newIndex, ad)))
  }


  if(xnoise.size > maxNoiseCount) {
    xnoise = packingStandard(xnoise)
  }

  assert(xnoise.size <= maxNoiseCount, "packing did not work "+xnoise.size+ " ! \n" + xnoise.toStringVertical)
  assert(xnoise.isSorted, "noise terms not sorted!")


  def +(other: AffineForm): AffineForm = other match {

    case AForm(y0, ynoise) => {
      var z0 = x0 + y0
      if(z0 == PlusInf || z0 == MinusInf) return FullForm
      var delta = if (addD(x0, y0) == addU(x0, y0)) Array(0.0, 0.0)
        else Array(DirRound.roundOff(z0), 0.0)

      var (deviation, deltaAddition) = addQueuesUncertain(xnoise, ynoise)
      delta = addUp(delta, deltaAddition)

      if(delta(0) == PlusInf) return FullForm
      if (delta(0) != 0.0 || delta(1) != 0.0) deviation :+ new Noise(newIndex, delta)
      return AForm(z0, deviation)
    }
    case EmptyForm => return other
    case FullForm => return other
    case _ => assert(false, "incompatible affine forms"); return null;
  }


  def -(other: AffineForm): AffineForm = other match {

    case AForm(y0, ynoise) => {  //we're doing plain subtraction, a == 1, b == 1, c == 0
      var z0 = x0 - y0
      if(z0 == PlusInf || z0 == MinusInf) return FullForm
      var delta = if (subD(x0, y0) == subU(x0, y0)) Array(0.0, 0.0)
        else Array(DirRound.roundOff(z0), 0.0)

      var (deviation, deltaAddition) = subtractQueuesUncertain(xnoise, ynoise)
      delta = addUp(delta, deltaAddition)

      if(delta(0) == PlusInf) return FullForm
      if (delta(0) != 0.0 || delta(1) != 0.0) deviation :+ new Noise(newIndex, delta)
      return new AForm(z0, deviation)
    }
    case EmptyForm => return other
    case FullForm => return other
    case _ => assert(false, "incompatible affine forms"); return null;
  }


  def unary_-(): AffineForm = {
    var deviation = new Queue
    val iter = xnoise.getIterator
    while(iter.hasNext) {
      val xi = iter.next
      deviation :+ - xi   //no round-off error
    }
    new AForm(-x0, deviation)
  }


  def *(other: AffineForm): AffineForm = other match {
    case AForm(y0, ynoise) => {
      var delta =  multUp(this.radiusExt, other.radiusExt)
      var z0 = x0 * y0
      if(z0 == PlusInf || z0 == MinusInf) return FullForm
      if (multD(x0, y0) != multU(x0, y0)) {
        delta =  addUp(delta, Array(DirRound.roundOff(z0), 0.0))
      }

      var (deviation, deltaAddition) = multiplyQueuesUncertain(x0, xnoise, y0, ynoise)
      delta = addUp(delta, deltaAddition)

      if(delta(0) == PlusInf) return FullForm
      if (delta(0) != 0.0 || delta(1) != 0.0) deviation :+ new Noise(newIndex, delta)
      return new AForm(z0, deviation)
    }
    case EmptyForm => return other
    case FullForm => return other
    case _ => assert(false, "incompatible affine forms"); return null;
  }

  def /(other: AffineForm): AffineForm = other match {
    case y: AForm =>
      val (yloD, yhiD) = (y.interval.xlo, y.interval.xhi)

      val (ylo: Array[Double], yhi: Array[Double]) = y.intervalExt

      if(yloD <= 0.0 && yhiD >= 0.0) return FullForm  //division by zero

      if(y.xnoise.size == 0.0) { //exact
        val inv = 1.0/y.x0
        if(divD(1.0, y.x0) == divU(1.0, y.x0))   //exact division
          return this * AForm(inv, Queue.empty)
        else
          return this * AForm(inv, new Queue(new Noise(newIndex, DirRound.roundOff(inv))))
      }

      /* Calculate the inverse. */
      val (alpha, dmin, dmax) = computeInverse(yloD, yhiD, ylo, yhi)

      var zeta = computeZeta(dmin, dmax)
      if(yloD < 0.0) zeta = Array(-zeta(0), -zeta(1))
      val delta = computeDelta(zeta, dmin, dmax)

      val inverse = unaryOp(y.x0, y.xnoise, alpha, zeta, delta)
      return this * inverse

    case EmptyForm => return other
    case FullForm => return other
    case _ => assert(false, "incompatible affine forms"); return null;

  }

  def squareRoot: AffineForm = {
    var (ad, bd) = (interval.xlo, interval.xhi)

    if(bd < 0.0) return EmptyForm
    if(bd == PlusInf) return interval2affine(this.interval.squareRoot)

    if(xnoise.size == 0) { //exact
      val sqrt = math.sqrt(x0)
      return new AForm(sqrt, new Queue(new Noise(newIndex, DirRound.roundOff(sqrt))))
    }

    var (a: Array[Double], b: Array[Double]) = intervalExt

    if(ad < 0.0) {ad = 0.0; a = Array(0.0, 0.0)}

    val sqA = Array(sqrtD(ad), 0.0)
    val sqB = Array(sqrtU(bd), 0.0)
    val alpha = div(Array(0.5, 0.0),  sqrt(b))
    val dmin = subDown(sqA, mult(alpha, a))
    val dmax = subUp(sqB, mult(alpha, b))

    val zeta = computeZeta(dmin, dmax)
    val delta = computeDelta(zeta, dmin, dmax)
    return unaryOp(x0, xnoise, alpha, zeta, delta)
  }


  def ln: AffineForm = {
    if(xnoise.size == 0) return computeExactResultStd(x0, math.log)

    val ad = interval.xlo
    val bd = interval.xhi

    if(ad <= 0.0 || bd < 0.0 || bd == PlusInf)
      return FullForm

    var (a: Array[Double], b: Array[Double]) = intervalExt

    val alpha =  div(Array(1.0, 0.0) , b)
    val dmin =  subDown(Array(d1(math.log(ad)), 0.0),   multUp(alpha,a))
    val dmax =  subUp(Array(u1(math.log(bd)), 0.0),  multDown(alpha,b))
    var zeta = computeZeta(dmin, dmax)
    val delta = computeDelta(zeta, dmin, dmax)

    return unaryOp(x0, xnoise, alpha, zeta, delta)
  }


  def exponential: AffineForm = {
    if(interval.xhi == PlusInf) return FullForm
    if(xnoise.size == 0) return computeExactResultStd(x0, math.exp)

    var (a: Array[Double], b: Array[Double]) = intervalExt
    val expA = Array(d1(math.exp(interval.xlo)), 0.0)
    val expB = Array(u1(math.exp(interval.xhi)), 0.0)

    val alpha = expA
    val dmin =  multDown(expA ,  sub(Array(1.0, 0.0), a))
    val dmax =  subUp(expB,  multDown(alpha,b))

    var zeta = computeZeta(dmin, dmax)
    val delta = computeDelta(zeta, dmin, dmax)
    return unaryOp(x0, xnoise, alpha, zeta, delta)
  }

  def cosine: AffineForm = {
    if((radiusExt(0) > TWO_PI(0)) ||
      (radiusExt(0) == TWO_PI(0) && radiusExt(1) >= TWO_PI(1))) {
      return new AForm(0.0, new Queue(new Noise(AffineForm.newIndex, 1.0)))
    }

    if(xnoise.size == 0) return computeExactResultStd(x0, math.cos)

    var (xlo, xhi) = (interval.xlo, interval.xhi)
    var (xloExt, xhiExt) = intervalExt

    //let's use the symmetry, there is something funny going on for [-2.8, -0.5]
    if(xhi < 0.0) return (-this).cosine

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
      val (dmin: Array[Double], dmax: Array[Double]) =
        if(r == 0 || r == -1 || r == 3) { //y > 0
          (subDown(Array(d1(math.cos(xhi)), 0.0), multUp(alpha, bExt)),
            subUp(Array(u1(math.cos(xlo)), 0.0), multDown(alpha, aExt)))
        }
        else { //y < 0
          (subDown(Array(d1(math.cos(xlo)), 0.0), multUp(alpha, aExt)),
            subUp(Array(u1(math.cos(xhi)), 0.0), multDown(alpha, bExt)))
        }
      val x0D = Array(x0, 0.0)
      val x0_new = mult(TWO_PI, sub(div(x0D, TWO_PI), kD))
      val x0_down = multDown(TWO_PI, subDown(divDown(x0D, TWO_PI), kD))
      val x0_up = multUp(TWO_PI, subUp(divUp(x0D, TWO_PI), kD))


      var zeta = computeZeta(dmin, dmax)
      var delta = computeDelta(zeta, dmin, dmax)

      delta = addUp(delta, max(subUp(x0_up, x0_new), subUp(x0_new, x0_down)))
      return unaryOp(x0_new, xnoise, alpha, zeta, delta)

    }
    else {
      return interval2affine(interval.cosine)
    }

  }

  def sine: AffineForm = {
    if((radiusExt(0) > TWO_PI(0)) ||
      (radiusExt(0) == TWO_PI(0) && radiusExt(1) >= TWO_PI(1))) {
      return new AForm(0.0, new Queue(new Noise(AffineForm.newIndex, 1.0)))
    }

    if(xnoise.size == 0) return computeExactResultStd(x0, math.sin)

    var (xlo, xhi) = (interval.xlo, interval.xhi)
    var (xloExt, xhiExt) = intervalExt

    //use symmetry, cause there is something funny going on for [-2.9, -0.5]
    if(xhi < 0.0) return -((-this).sine)

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
      var zeta = computeZeta(dmin, dmax)
      var delta = computeDelta(zeta, dmin, dmax)

      val x0D = Array(x0, 0.0)
      val x0_new = mult(TWO_PI, sub(div(x0D, TWO_PI), kD))
      val x0_down = multDown(TWO_PI, subDown(divDown(x0D, TWO_PI), kD))
      val x0_up = multUp(TWO_PI, subUp(divUp(x0D, TWO_PI), kD))
      delta = addUp(delta, max(subUp(x0_up, x0_new), subUp(x0_new, x0_down)))
      return unaryOp(x0_new, xnoise, alpha, zeta, delta)

    }
    else {
      return interval2affine(interval.sine)
    }


  }

  def tangent: AffineForm = {

    if((radiusExt(0) > PI(0)) || (radiusExt(0) == PI(0) && radiusExt(1) >= PI(1))) {
      return FullForm
    }

    if(xnoise.size == 0) return computeExactResultStd(x0, math.tan)

    var (xlo, xhi) = (interval.xlo, interval.xhi)
    var (xloExt, xhiExt) = intervalExt

    val ka = if(xlo > 0.0) divD(2.0*xlo, Pi_up) else divD(2.0*xlo, Pi_down)
    val kb = if(xhi > 0.0) divU(2.0*xhi, Pi_down) else divU(2.0*xhi, Pi_up)
    val m = math.floor(ka).toLong
    val n = math.ceil(kb).toLong

    if(n-m < 2.0) {
      val (alpha:Array[Double], dmin: Array[Double], dmax: Array[Double], k:Array[Double]) =
        if(m % 2 == 0) { //y > 0
          val kx = math.floor(2.0 * xlo / math.Pi)
          val kD = Array(kx, 0.0)
          val aExt = multDown(PI_2, subDown(divDown(xloExt, PI_2), kD))
          val bExt = multUp(PI_2, subUp(divUp(xhiExt, PI_2), kD))

          val alphax = Array(1.0 + math.tan(xlo)*math.tan(xlo), 0.0)
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
          (alphax,
           subDown(Array(d1(math.tan(xlo)), 0.0), multUp(alphax, aExt)),
           subUp(Array(u1(math.tan(xhi)), 0.0), multDown(alphax, bExt)), kD)
        }

      var zeta = computeZeta(dmin, dmax)
      var delta = computeDelta(zeta, dmin, dmax)

      val x0D = Array(x0, 0.0)
      val x0_new = mult(PI_2, sub(div(x0D, PI_2), k))
      val x0_down = multDown(PI_2, subDown(divDown(x0D, PI_2), k))
      val x0_up = multUp(PI_2, subUp(divUp(x0D, PI_2), k))
      delta = addUp(delta, max(subUp(x0_up, x0_new), subUp(x0_new, x0_down)))
      return unaryOp(x0_new, xnoise, alpha, zeta, delta)
    }
    else if(n-m == 2.0) {
      return interval2affine(interval.tangent)
    }
    else {
      FullForm
    }
  }


  def arccosine: AffineForm = {
    var (a, b) = (interval.xlo, interval.xhi)
    var (aExt, bExt) = intervalExt

    if(b < -1.0 || a > 1.0)
      return EmptyForm

    if(a == b) return computeExactResultStd(x0, math.acos)

    if(a < 0.0 && b > 0.0) {
      return interval2affine(interval.arccosine)
    }

    val one = Array(1.0, 0.0)
    val _one = Array(-1.0, 0.0)

    //again, something funny going on here
    if(a > 0.8) return -(-this).arccosine + new AForm(math.Pi)

    if(b > 1.0) {b = 1.0; bExt = one}
    if(a < -1.0) {a = -1.0; aExt = _one}

    val alpha: Array[Double] =
      if(a > 0.0) {
        div(_one, sqrt(sub(one, mult(aExt,aExt))))
      }
      else {
        div(_one, sqrt(sub(one, mult(bExt,bExt))))
      }

    val dmin = subDown(Array(d1(math.acos(b)), 0.0), multUp(alpha, bExt))
    val dmax = subUp(Array(u1(math.acos(a)), 0.0), multDown(alpha, aExt))
    var zeta = computeZeta(dmin, dmax)
    val delta = computeDelta(zeta, dmin, dmax)

    return unaryOp(x0, xnoise, alpha, zeta, delta)
  }


  def arcsine: AffineForm = {

    var (a, b) = (interval.xlo, interval.xhi)
    var (aExt, bExt) = intervalExt

    if(b < -1.0 || a > 1.0)
      return EmptyForm

    if(a == b) return computeExactResultStd(x0, math.asin)

    if(a < 0.0 && b > 0.0) {
      return interval2affine(interval.arcsine)
    }

    val one = Array(1.0, 0.0)
    val _one = Array(-1.0, 0.0)

    if(b > 1.0) {b = 1.0; bExt = one}
    if(a < -1.0) {a = -1.0; aExt = _one}

    val alpha: Array[Double] =
      if(a > 0.0) {
        div(one, sqrt(sub(one, mult(aExt,aExt))))
      }
      else {
        div(one, sqrt(sub(one, mult(bExt,bExt))))
      }

    val dmin = subDown(Array(d1(math.asin(a)), 0.0), multUp(alpha, aExt))
    val dmax = subUp(Array(u1(math.asin(b)), 0.0), multDown(alpha, bExt))
    var zeta = computeZeta(dmin, dmax)
    val delta = computeDelta(zeta, dmin, dmax)

    return unaryOp(x0, xnoise, alpha, zeta, delta)
  }


  def arctangent: AffineForm = {
    var (a, b) = (interval.xlo, interval.xhi)
    var (aExt, bExt) = intervalExt

    if(a == b) return computeExactResultStd(x0, math.atan)

    if(a < 0.0 && b > 0.0) {
      return interval2affine(interval.arctangent)
    }

    val one = Array(1.0, 0.0)

    val alpha: Array[Double] =
      if(a > 0.0) {
        div(one , add(one , mult(aExt, aExt)))
      }
      else {
        div(one , add(one , mult(bExt, bExt)))
      }

    val dmin = subDown(Array(d1(math.atan(b)), 0.0), multUp(alpha, bExt))
    val dmax = subUp(Array(u1(math.atan(a)), 0.0), multDown(alpha, aExt))
    var zeta = computeZeta(dmin, dmax)
    val delta = computeDelta(zeta, dmin, dmax)

    return unaryOp(x0, xnoise, alpha, zeta, delta)

  }


}



