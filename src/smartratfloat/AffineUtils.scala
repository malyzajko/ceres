package ceres.smartratfloat

import ceres.common.Rational
import Rational._

import ceres.common.DirectedRounding.{multDown => multD, multUp => multU, divUp => divU,
      divDown => divD, subDown => subD, subUp => subU, addUp => addU, addDown => addD,
      sqrtDown => sqrtD, sqrtUp => sqrtU}

object AffineUtils {
 import AffineForm._

  def formatQueueUncertain(q: Queue): String = {
    var s = ""
    val iter = q.getIterator
    while(iter.hasNext) {
      val xi = iter.next
      xi match {
        case n: Noise =>
          if(xi.doubleValue < 0.0) s += xi.toString   else s += "+" + xi.toString
        case u: Uncertainty =>
          if(xi.doubleValue < 0.0) s += xi.toString   else s += "+" + xi.toString
      }
    }
    s
  }

  def addQueuesUncertain(xn: Queue, yn: Queue): Queue = {
    //var delta = Rational.zero
    var deviation = new Queue
    val iterX = xn.getIterator
    val iterY = yn.getIterator

    val fx = (xi: Deviation) => { deviation :+ xi }
    val fy = (yi: Deviation) => { deviation :+ yi }

    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi =  xi.value + yi.value
      //delta =  addUp(delta,  rdOff(zi))
      if(zi != zero) {
        xi match {
          case n:Noise => deviation :+ new Noise(xi.index, zi)//, xi.comesFrom)
          case u:Uncertainty => deviation :+ new Uncertainty(xi.index, zi)//, xi.comesFrom)
        }
      }
    }
    DoubleQueueIterator.iterate[Rational, Deviation](iterX, iterY, new Noise(Int.MaxValue, 0.0),
      fx, fy, fCouple)
    return deviation
  }

  def subtractQueuesUncertain(xn: Queue, yn: Queue): Queue = {
    //var delta = Rational.zero
    var deviation = new Queue
    val iterX = xn.getIterator
    val iterY = yn.getIterator
    val fx = (xi: Deviation) => { deviation :+ xi }
    val fy = (yi: Deviation) => { deviation :+ -yi }

    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi =  xi.value - yi.value
      //delta =  addUp(delta,  rdOff(zi))
      if(zi != zero) {
        xi match {
          case n:Noise => deviation :+ new Noise(xi.index, zi)//, xi.comesFrom)
          case u:Uncertainty => deviation :+ new Uncertainty(xi.index, zi)//, xi.comesFrom)
        }
      }
    }
    DoubleQueueIterator.iterate[Rational, Deviation](iterX, iterY, new Noise(Int.MaxValue, 0.0), fx, fy, fCouple)
    return deviation
  }



  def multiplyQueuesUncertain(a: Double, xqueue: Queue, b: Double, yqueue: Queue): Queue = {
    //var delta = Array(0.0, 0.0)
    var deviation = new Queue
    val iterX = xqueue.getIterator
    val iterY = yqueue.getIterator

    val aRat = Rational(a)
    val bRat = Rational(b)

    val fx = (d: Deviation) => {
      val zi =  bRat * d.value
      //delta =  addUp(delta,  rdOff(zi))
      if(zi != zero)
        d match {
          case n:Noise => deviation :+ new Noise(d.index, zi)//, d.comesFrom)
          case u:Uncertainty => deviation :+ new Uncertainty(d.index, zi)//, d.comesFrom)
        }
    }
    val fy = (d: Deviation) => {
      val zi =  aRat * d.value
      //delta =  addUp(delta,  rdOff(zi))
      if(zi != zero)
        d match {
          case n:Noise => deviation :+ new Noise(d.index, zi)//, d.comesFrom)
          case u:Uncertainty => deviation :+ new Uncertainty(d.index, zi)//, d.comesFrom)
        }
    }
    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi = (bRat * xi.value) + (aRat * yi.value)
      //delta =  addUp(delta,  rdOff(zi))// Fixme: this should be rdOffABplusCD
      if(zi != zero)
        xi match {
          case n:Noise => deviation :+ new Noise(xi.index, zi)//, xi.comesFrom)
          case u:Uncertainty => deviation :+ new Uncertainty(xi.index, zi)//, xi.comesFrom)
        }
    }
    DoubleQueueIterator.iterate[Rational, Deviation](iterX, iterY, new Noise(Int.MaxValue, 0.0), fx, fy, fCouple)
    return deviation
  }

  def packingStandard(q: Queue): Queue = {
    var newDev = packNoiseSymbolsStandardDeviation(q)
    //repeat once
    if(newDev.size > maxNoiseCount) {
      newDev = packNoiseSymbolsStandardDeviation(newDev)
      if(newDev.size > maxNoiseCount) {
        newDev = packNoiseSymbolsBruteForce(newDev)
      }
    }
    newDev
  }

  def packNoiseSymbolsStandardDeviation(queue: Queue): Queue = {
    if(queue.size > maxNoise) maxNoise = queue.size
    var sum = 0.0
    var counter = 0
    var iter = queue.getIterator
    while(iter.hasNext) {
      val xi = iter.next
      val v =  math.abs(xi.doubleValue)
      if(v > packingThreshold) { sum += v;  counter += 1; }
    }
    val avrg = sum/counter

    //compute st. deviation
    var devSum = 0.0
    iter = queue.getIterator
    while(iter.hasNext) {
      val v =  math.abs(iter.next.doubleValue)
      if(v > packingThreshold) {
        val diff = v - avrg
        devSum += diff * diff
      }
    }
    val stdDev = math.sqrt(devSum/counter)
    val threshold = avrg + stdDev

    //Now compute the new queue
    var newNoise = zero
    var newDev = new Queue

    val iter2 = queue.getIterator
    while(iter2.hasNext) {
      val xi = iter2.next
      val v =  abs(xi.value)
      if(v.toDouble < threshold) newNoise = newNoise + v
      else newDev :+ xi
    }
    newDev :+ new Noise(newIndex, newNoise)
    return newDev
  }

  def packNoiseSymbolsBruteForce(queue: Queue): Queue = {
    var newNoise = zero
    var newDev = new Queue

    val iter2 = queue.getIterator
    while(iter2.hasNext) { newNoise = newNoise + abs(iter2.next.value)}
    newDev :+ new Noise(newIndex, newNoise)
    return newDev
  }

  def computeInverse(yloD: Double, yhiD: Double, ylo: Rational, yhi: Rational):
    (Rational, Rational, Rational) = {

    val a = min(abs(ylo), abs(yhi))
    val b = max(abs(ylo), abs(yhi))

    val ad = math.min(math.abs(yloD), math.abs(yhiD))
    val bd = math.max(math.abs(yloD), math.abs(yhiD))

    val alpha =  - one / (b *b)  //no rounding, it's intentional

    val dmax = Rational(divU(1.0, ad)) - (alpha * a)
    val dmin = Rational(divD(1.0, bd)) - (alpha * b)

    return (alpha, dmin, dmax)
  }

  def computeCentralZeta(z0: Double, alpha: Rational, x0: Double): Rational = {
    val z0d = Rational(z0)
    val x0d = Rational(x0)

    val zeta = z0d - (alpha * x0d)
    val zetaDown = zeta //subDown(z0d -  alpha * x0d))
    val zetaUp = zeta//subUp(z0d, multUp(alpha, x0d))

    return zeta //, max( subUp(zeta, zetaDown), subUp(zetaUp, zeta)))
  }

  def computeDelta(zeta: Rational, dmin: Rational, dmax: Rational): Rational = {
    max(zeta -  dmin, dmax - zeta)
  }

}