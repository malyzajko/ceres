package ceres.smartfloat

import scala.collection.immutable.HashMap
import ceres.common.DDouble
import DDouble._
import math.{min => mmin, max => mmax, abs => mabs}
//So they don't get confused...
import ceres.common.DirectedRounding.{multDown => multD, multUp => multU, divUp => divU,
      divDown => divD, subDown => subD, subUp => subU, addUp => addU, addDown => addD,
      sqrtDown => sqrtD, sqrtUp => sqrtU}
import ceres.common.DirectedRounding.{down1 => d1, up1 => u1, nextDown}
//import ceres.smartfloat.special.AForm

/**
 * A collection of common functions for pur implementations of affine arithmetic.
 */
object AffineUtils {
  import AffineForm._

  /* ####################################################################
    #####################    Queue formatting     #########################
     ####################################################################*/
  def formatQueue(q: Queue): String = {
    var s = ""
    val iter = q.getIterator
    while(iter.hasNext) {
      val xi = iter.next
      if(xi.doubleValue < 0.0)  s += xi.toString  else s += "+" + xi.toString
    }
    s
  }
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


  /* ####################################################################
    #####################    Queue summing     #########################
     ####################################################################*/
  def sumQueue(q: Queue): Array[Double] = {
    var sum = zero
    if(q.size == 1)  sum = abs(q.head.value)
    else {
      val iter = q.getIterator
      while(iter.hasNext) {   sum = addUp(sum, abs(iter.next.value))  }
    }
    sum
  }

  /* ####################################################################
    #####################    Smart Intervals     #########################
     ####################################################################*/
  // TODO: we can still try the quadratic optimization, i.e. with ei * ei being positiv


     /* ####################################################################
    #####################    Eta computation     #########################
     ####################################################################*/

  def computeEta(xnoise: Queue, ynoise: Queue): Array[Double] = {
    var delta = zero

    val iterX = xnoise.getIterator
    while(iterX.hasNext) {
      val xi = iterX.next
      val i = xi.index

      val iterY = ynoise.getIterator
      while(iterY.hasNext) {
        val yj = iterY.next
        val j = yj.index
        if(i == j) {
          delta = addUp(delta , multUp(xi.value, yj.value))
        }
        else {
          delta = addUp(delta , abs(multUp(xi.value, yj.value)))
        }
       }
    }
    delta
  }


     /* ####################################################################
    #####################    Roundoff computation over ranges     #########################
     ####################################################################*/
  //for exactly rounded calculations
  def getRoundoff(xn: Double, xqueue:Queue): Array[Double] = {
    if(xqueue.size == 0) return zero

    var sum = sumQueue(xqueue)
    val zlo = abs(subDown(xn, 0.0, sum(0), sum(1)))
    val zhi = abs(addUp(xn, 0.0, sum(0), sum(1)))
    val m = max(zlo, zhi)
    return multUp(m, Array(ulp, 0.0))
    
  }

  def getRoundoff1(xn: Double, xqueue:Queue): Array[Double] = {
    
    var sum = sumQueue(xqueue)
    val zlo = abs(subDown(xn, 0.0, sum(0), sum(1)))
    val zhi = abs(addUp(xn, 0.0, sum(0), sum(1)))

    val m = max(zlo, zhi)
    return multUp(m, Array(2*ulp, 0.0))
    
  }



  /* ####################################################################
    #####################    Noise symbol packing     #########################
     ####################################################################*/
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

  def packingUncertain(q: Queue): Queue = {
    var newDev = packNoiseSymbolsStdDevWithUncertainty(q, packingFactor, packingAvrgScale)
    //repeat once
    if(newDev.size > maxNoiseCount) {
      newDev = packNoiseSymbolsStdDevWithUncertainty(newDev, packingFactor, packingAvrgScale)
      if(newDev.size > maxNoiseCount) {
        newDev = packNoiseSymbolsBruteForceWithUncertainty(newDev)
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
      val v =  mabs(xi.doubleValue)
      if(v > packingThreshold) { sum += v;  counter += 1; }
    }
    val avrg = sum/counter

    //compute st. deviation
    var devSum = 0.0
    iter = queue.getIterator
    while(iter.hasNext) {
      val v =  mabs(iter.next.doubleValue)
      if(v > packingThreshold) {
        val diff = v - avrg
        devSum += diff * diff
      }
    }
    val stdDev = math.sqrt(devSum/counter)
    val threshold = avrg + stdDev

    //Now compute the new queue
    var newNoise = Array(0.0, 0.0)
    var newDev = new Queue

    val iter2 = queue.getIterator
    while(iter2.hasNext) {
      val xi = iter2.next
      val v =  abs(xi.value)
      if(v(0) < threshold) newNoise =  addUp(newNoise, v)
      else newDev :+ xi
    }
    newDev :+ new Noise(newIndex, newNoise)
    return newDev
  }

  def packNoiseSymbolsStdDevWithUncertainty(queue: Queue, factor:Double, avrgScale: Double): Queue = {
    var sum = 0.0
    var counter = 0
    var iter = queue.getIterator
    while(iter.hasNext) { iter.next match {
        case n:Noise =>
          val v =  mabs(n.doubleValue)
          if(v > packingThreshold) {  sum += v;   counter += 1; }
        case u:Uncertainty => ;
    }}
    val avrg = sum/counter

    //compute st. deviation
    var devSum = 0.0
    iter = queue.getIterator
    while(iter.hasNext) {   iter.next match {
        case n:Noise =>
          val v = mabs(n.doubleValue)
          if(v > packingThreshold) {
            val diff = v - avrg
            devSum += diff * diff
          }
        case u:Uncertainty => ;
    }}
    val stdDev = math.sqrt(devSum/counter)

    val threshold = avrg*avrgScale + stdDev*factor

    //Now compute the new queue
    var newNoise = Array(0.0, 0.0)
    var newDev = new Queue

    val iter2 = queue.getIterator
    while(iter2.hasNext) {
      iter2.next match {
        case n:Noise =>
          val v =  abs(n.value)
          if(v(0) < threshold) newNoise =  addUp(newNoise, v)
          else newDev :+ n
        case u:Uncertainty =>
          newDev :+ u
    }}
    newDev :+ new Noise(newIndex, newNoise)
    return newDev
  }

  def packNoiseSymbolsBruteForce(queue: Queue): Queue = {
    var newNoise = Array(0.0, 0.0)
    var newDev = new Queue

    val iter2 = queue.getIterator
    while(iter2.hasNext) { newNoise =  addUp(newNoise, abs(iter2.next.value))}
    newDev :+ new Noise(newIndex, newNoise)
    return newDev
  }

  //if this still fails, then one needs to increase the number of noise symbols.
  def packNoiseSymbolsBruteForceWithUncertainty(queue: Queue): Queue = {
    var newNoise = Array(0.0, 0.0)
    var newDev = new Queue

    val iter2 = queue.getIterator
    while(iter2.hasNext) {
      iter2.next match {
        case n:Noise => newNoise = addUp(newNoise, abs(n.value))
        case u:Uncertainty => newDev :+ u
      }
    }
    newDev :+ new Noise(newIndex, newNoise)
    return newDev
  }

  /* ####################################################################
    #####################    Computing zeta and delta     #########################
     ####################################################################*/
  def computeZeta(dmin: Array[Double], dmax: Array[Double]): Array[Double] = {
     add( divUp(dmin(0), dmin(1), 2.0, 0.0),  divDown(dmax(0), dmax(1), 2.0, 0.0))
  }

  /**
   * Computes an adjusted zeta for the affine forms that require symmetric
   * unary functions.
   * @return (zeta, roundoff) new zeta plus the error commited in the computation
   */
  def computeCentralZeta(z0: Double, alpha: Array[Double], x0: Double):
    (Array[Double], Array[Double]) = {
    val z0d = Array(z0, 0.0)
    val x0d = Array(x0, 0.0)

    val zeta = sub(z0d, mult(alpha, x0d))
    val zetaDown = subDown(z0d, multDown(alpha, x0d))
    val zetaUp = subUp(z0d, multUp(alpha, x0d))

    return (zeta, max( subUp(zeta, zetaDown), subUp(zetaUp, zeta)))
  }


  def computeDelta(zeta: Array[Double], dmin: Array[Double], dmax: Array[Double]): Array[Double] = {
    max( subUp(zeta, dmin),  subUp(dmax, zeta))
  }

  /* ####################################################################
    #####################    Computing inverse     #########################
     ####################################################################*/
  //alpha, dmin, dmax
  def computeInverse(yloD: Double, yhiD: Double, ylo: Array[Double], yhi: Array[Double]):
    (Array[Double], Array[Double], Array[Double]) = {

    val a = min(abs(ylo), abs(yhi))
    val b = max(abs(ylo), abs(yhi))

    val ad = mmin(mabs(yloD), mabs(yhiD))
    val bd = mmax(mabs(yloD), mabs(yhiD))

    val alpha =  div(Array(-1.0, 0.0),  mult(b,b))  //no rounding, it's intentional

    val dmax =  subUp(Array(divU(1.0, ad), 0.0) ,  multDown(alpha, a))
    val dmin =  subDown(Array(divD(1.0, bd), 0.0),  multUp(alpha, b))

    return (alpha, dmin, dmax)
  }

  /* ####################################################################
    #####################    Queue add., subtr. mult.     #########################
     ####################################################################*/
  // returns the added Queue + the error committed in the computation
  def addQueues(xn: Queue, yn: Queue): (Queue, Array[Double]) = {
    var delta = Array(0.0, 0.0)
    var deviation = new Queue
    val iterX = xn.getIterator
    val iterY = yn.getIterator

    val fx = (xi: Deviation) => { deviation :+ xi }
    val fy = (yi: Deviation) => { deviation :+ yi }

    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi =  add(xi.value, yi.value)
      delta =  addUp(delta,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0) {
        deviation :+ new Noise(xi.index, zi)
      }
    }
    DoubleQueueIterator.iterate[Array[Double], Deviation](iterX, iterY, new Noise(Int.MaxValue, 0.0),
      fx, fy, fCouple)
    return (deviation, delta)
  }

  // returns the added Queue + the error committed in the computation
  def addQueuesUncertain(xn: Queue, yn: Queue): (Queue, Array[Double]) = {
    var delta = Array(0.0, 0.0)
    var deviation = new Queue
    val iterX = xn.getIterator
    val iterY = yn.getIterator

    val fx = (xi: Deviation) => { deviation :+ xi }
    val fy = (yi: Deviation) => { deviation :+ yi }

    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi =  add(xi.value, yi.value)
      delta =  addUp(delta,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0) {
        xi match {
          case n:Noise => deviation :+ new Noise(xi.index, zi)
          case u:Uncertainty => deviation :+ new Uncertainty(xi.index, zi)
        }
      }
    }
    DoubleQueueIterator.iterate[Array[Double], Deviation](iterX, iterY, new Noise(Int.MaxValue, 0.0),
      fx, fy, fCouple)
    return (deviation, delta)
  }

  // returns the added Queue + the error committed in the computation
  def subtractQueues(xn: Queue, yn: Queue): (Queue, Array[Double]) = {
    var delta = Array(0.0, 0.0)
    var deviation = new Queue
    val iterX = xn.getIterator
    val iterY = yn.getIterator

    val fx = (xi: Deviation) => { deviation :+ xi }
    val fy = (yi: Deviation) => { deviation :+ -yi }

    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi =  sub(xi.value, yi.value)
      delta =  addUp(delta,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0) {
        deviation :+ new Noise(xi.index, zi)
      }
    }
    DoubleQueueIterator.iterate[Array[Double], Deviation](iterX, iterY, new Noise(Int.MaxValue, 0.0),
     fx, fy, fCouple)
    return (deviation, delta)
  }

  // returns the added Queue + the error committed in the computation
  def subtractQueuesUncertain(xn: Queue, yn: Queue): (Queue, Array[Double]) = {
    var delta = Array(0.0, 0.0)
    var deviation = new Queue
    val iterX = xn.getIterator
    val iterY = yn.getIterator
    val fx = (xi: Deviation) => { deviation :+ xi }
    val fy = (yi: Deviation) => { deviation :+ -yi }

    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi =  sub(xi.value, yi.value)
      delta =  addUp(delta,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0) {
        xi match {
          case n:Noise => deviation :+ new Noise(xi.index, zi)
          case u:Uncertainty => deviation :+ new Uncertainty(xi.index, zi)
        }
      }
    }
    DoubleQueueIterator.iterate[Array[Double], Deviation](iterX, iterY, new Noise(Int.MaxValue, 0.0), fx, fy, fCouple)
    return (deviation, delta)
  }

  def multiplyQueues(a: Double, xqueue: Queue, b: Double, yqueue: Queue): (Queue, Array[Double]) = {
    var delta = Array(0.0, 0.0)
    var deviation = new Queue
    val iterX = xqueue.getIterator
    val iterY = yqueue.getIterator

    val fx = (d: Deviation) => {
      val zi =  mult(b, 0.0, d.value(0), d.value(1))
      delta =  addUp(delta,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0) deviation :+ new Noise(d.index, zi)
    }
    val fy = (d: Deviation) => {
      val zi =  mult(a, 0.0, d.value(0), d.value(1))
      delta =  addUp(delta,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0) deviation :+ new Noise(d.index, zi)
    }
    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi =  add( mult(b, 0.0, xi.value(0), xi.value(1)),
            mult(a, 0.0, yi.value(0), yi.value(1)))
      delta =  addUp(delta,  rdOff(zi))// Fixme: this should be rdOffABplusCD
      if(zi(0) != 0.0 || zi(1) != 0.0) deviation :+ new Noise(xi.index, zi)
    }
    DoubleQueueIterator.iterate[Array[Double], Deviation](iterX, iterY, new Noise(Int.MaxValue, 0.0), fx, fy, fCouple)
    return(deviation, delta)
  }

  def multiplyQueuesUncertain(a: Double, xqueue: Queue, b: Double, yqueue: Queue): (Queue, Array[Double]) = {
    var delta = Array(0.0, 0.0)
    var deviation = new Queue
    val iterX = xqueue.getIterator
    val iterY = yqueue.getIterator

    val fx = (d: Deviation) => {
      val zi =  mult(b, 0.0, d.value(0), d.value(1))
      delta =  addUp(delta,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0)
        d match {
          case n:Noise => deviation :+ new Noise(d.index, zi)
          case u:Uncertainty => deviation :+ new Uncertainty(d.index, zi)
        }
    }
    val fy = (d: Deviation) => {
      val zi =  mult(a, 0.0, d.value(0), d.value(1))
      delta =  addUp(delta,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0)
        d match {
          case n:Noise => deviation :+ new Noise(d.index, zi)
          case u:Uncertainty => deviation :+ new Uncertainty(d.index, zi)
        }
    }
    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi =  add( mult(b, 0.0, xi.value(0), xi.value(1)),
            mult(a, 0.0, yi.value(0), yi.value(1)))
      delta =  addUp(delta,  rdOff(zi))// Fixme: this should be rdOffABplusCD
      if(zi(0) != 0.0 || zi(1) != 0.0)
        xi match {
          case n:Noise => deviation :+ new Noise(xi.index, zi)
          case u:Uncertainty => deviation :+ new Uncertainty(xi.index, zi)
        }
    }
    DoubleQueueIterator.iterate[Array[Double], Deviation](iterX, iterY, new Noise(Int.MaxValue, 0.0), fx, fy, fCouple)
    return(deviation, delta)
  }

}




