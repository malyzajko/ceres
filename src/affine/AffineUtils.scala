package ceres.affine

import collection.mutable.Queue

import ceres.common._
import DDouble._
import math.{min => mmin, max => mmax, abs => mabs}
import ceres.common.DirectedRounding.{multDown => multD, multUp => multU, divUp => divU,
      divDown => divD, subDown => subD, subUp => subU, addUp => addU, addDown => addD,
      sqrtDown => sqrtD, sqrtUp => sqrtU}
import ceres.common.DirectedRounding.{down1 => d1, up1 => u1, nextDown}

case class NoiseTerm(index: Int, value: Array[Double]) {
  def unary_-(): NoiseTerm = new NoiseTerm(index, Array(-value(0), -value(1)))
  def doubleValue: Double = value(0) + value(1)
  override def toString: String = (value(0) + value(1)) + "e" + index
}

object DoubleQueueIterator {

  def iterate(iterX: Iterator[NoiseTerm], iterY: Iterator[NoiseTerm], dummy: NoiseTerm,
    fx: (NoiseTerm) => Unit, fy: (NoiseTerm) => Unit, fCouple: (NoiseTerm, NoiseTerm) => Unit): Unit = {

    var xi: NoiseTerm = if(iterX.hasNext) iterX.next else dummy
    var yi: NoiseTerm = if(iterY.hasNext) iterY.next else dummy

    while(iterX.hasNext || iterY.hasNext) {
      if(xi.index < yi.index) {
        fx(xi)
        xi = if(iterX.hasNext) iterX.next else dummy
      }
      else if(yi.index < xi.index) {
        fy(yi)
        yi = if(iterY.hasNext) iterY.next else dummy
      }
      else {
        fCouple(xi, yi)
        xi = if(iterX.hasNext) iterX.next else dummy
        yi = if(iterY.hasNext) iterY.next else dummy
      }
    }
    if(xi.index == yi.index) {
      if(xi != dummy) {
        fCouple(xi, yi)
        xi = dummy
        yi = dummy
      }
    }
    else if(xi.index < yi.index) {
      if(xi != dummy) {fx(xi); xi = dummy}
      if(yi != dummy) {fy(yi); yi = dummy}
    }
    else if(yi.index < xi.index) {
      if(yi != dummy) {fy(yi); yi = dummy}
      if(xi != dummy) {fx(xi); xi = dummy}
    }
  }
}


object AffineUtils {
  import AffineForm.{newIndex, maxNoiseCount, packingThreshold}
  val Pi_up = java.lang.Math.nextUp(math.Pi)
  val Pi_down = math.Pi
  val twoP = 2.0*math.Pi

  def packingStandard(q: Queue[NoiseTerm]): Queue[NoiseTerm] = {
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

  def packNoiseSymbolsStandardDeviation(q: Queue[NoiseTerm]): Queue[NoiseTerm] = {
    var sum = 0.0
    var counter = 0
    var iter = q.iterator
    while(iter.hasNext) {
      val xi = iter.next
      val v =  mabs(xi.doubleValue)
      if(v > packingThreshold) { sum += v;  counter += 1; }
    }
    val avrg = sum/counter

    //compute st. deviation
    var devSum = 0.0
    iter = q.iterator
    while(iter.hasNext) {
      val v =  mabs(iter.next.doubleValue)
      if(v > packingThreshold) {
        val diff = v - avrg
        devSum += diff * diff
      }
    }
    val stdDev = math.sqrt(devSum/counter)

    // For large numbers the std deviation may be huge, so if we don't want
    // to pack everything...
    val threshold =
      if (stdDev > 2 * avrg) {
        avrg
      } else {
        avrg + stdDev
      }

    //Now compute the new queue
    var newNoise = Array(0.0, 0.0)
    var newDev = new Queue[NoiseTerm]()

    val iter2 = q.iterator
    while(iter2.hasNext) {
      val xi = iter2.next
      val v =  abs(xi.value)
      if(v(0) < threshold) newNoise =  addUp(newNoise, v)
      else newDev += xi
    }
    newDev += NoiseTerm(newIndex, newNoise)
    return newDev
  }

  def packNoiseSymbolsBruteForce(q: Queue[NoiseTerm]): Queue[NoiseTerm] = {
    var newNoise = Array(0.0, 0.0)
    var newDev = new Queue[NoiseTerm]

    val iter2 = q.iterator
    while(iter2.hasNext) { newNoise =  addUp(newNoise, abs(iter2.next.value))}
    newDev += NoiseTerm(newIndex, newNoise)
    return newDev
  }

  def isSorted(q: Queue[NoiseTerm]): Boolean = {
    var sorted = true
    var i = 0
    while(i < q.size - 1) {
      if(q(i).index >= q(i+1).index) sorted = false
      i += 1
    }
    sorted
  }

  // TODO: fold...
  def sumQueue(q: Queue[NoiseTerm]): Array[Double] = {
    var sum = zero
    if(q.size == 1)  sum = abs(q.head.value)
    else {
      val iter = q.iterator
      while(iter.hasNext) { sum = addUp(sum, abs(iter.next.value))  }
    }
    sum
  }

  def addQueues(xn: Queue[NoiseTerm], yn: Queue[NoiseTerm]): (Queue[NoiseTerm], Array[Double]) = {
    var delta = Array(0.0, 0.0)
    var deviation = new Queue[NoiseTerm]()
    val iterX = xn.iterator
    val iterY = yn.iterator

    val fx = (xi: NoiseTerm) => { deviation += xi; val x = 0; }  // force Unit return type
    val fy = (yi: NoiseTerm) => { deviation += yi; val y = 0; }

    val fCouple = (xi: NoiseTerm, yi: NoiseTerm) => {
      val zi =  add(xi.value, yi.value)
      delta =  addUp(delta,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0) {
        deviation += NoiseTerm(xi.index, zi)
      }
      val x = 0;
    }
    DoubleQueueIterator.iterate(iterX, iterY, NoiseTerm(Int.MaxValue, Array(0.0, 0.0)),
      fx, fy, fCouple)
    return (deviation, delta)
  }

  def subtractQueues(xn: Queue[NoiseTerm], yn: Queue[NoiseTerm]): (Queue[NoiseTerm], Array[Double]) = {
    var delta = Array(0.0, 0.0)
    var deviation = new Queue[NoiseTerm]()
    val iterX = xn.iterator
    val iterY = yn.iterator

    val fx = (xi: NoiseTerm) => { deviation += xi; val x = 0; }
    val fy = (yi: NoiseTerm) => { deviation += -yi; val x = 0; }

    val fCouple = (xi: NoiseTerm, yi: NoiseTerm) => {
      val zi =  sub(xi.value, yi.value)
      delta =  addUp(delta,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0) {
        deviation += NoiseTerm(xi.index, zi)
      }
      val x = 0
    }
    DoubleQueueIterator.iterate(iterX, iterY, NoiseTerm(Int.MaxValue, Array(0.0, 0.0)),
     fx, fy, fCouple)
    return (deviation, delta)
  }

  def multiplyQueues(a: Double, xqueue: Queue[NoiseTerm], b: Double, yqueue: Queue[NoiseTerm])
    : (Queue[NoiseTerm], Array[Double]) = {
    var delta = Array(0.0, 0.0)
    var deviation = new Queue[NoiseTerm]()
    val iterX = xqueue.iterator
    val iterY = yqueue.iterator

    val fx = (d: NoiseTerm) => {
      val zi =  mult(b, 0.0, d.value(0), d.value(1))
      delta =  addUp(delta,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0) deviation += NoiseTerm(d.index, zi)
      val x = 0
    }
    val fy = (d: NoiseTerm) => {
      val zi =  mult(a, 0.0, d.value(0), d.value(1))
      delta =  addUp(delta,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0) deviation += NoiseTerm(d.index, zi)
      val x = 0
    }
    val fCouple = (xi: NoiseTerm, yi: NoiseTerm) => {
      val zi =  add( mult(b, 0.0, xi.value(0), xi.value(1)),
            mult(a, 0.0, yi.value(0), yi.value(1)))
      delta =  addUp(delta,  rdOff(zi))// Fixme: this should be rdOffABplusCD
      if(zi(0) != 0.0 || zi(1) != 0.0) deviation += NoiseTerm(xi.index, zi)
      val x = 0
    }
    DoubleQueueIterator.iterate(iterX, iterY,
      NoiseTerm(Int.MaxValue, Array(0.0, 0.0)), fx, fy, fCouple)
    return(deviation, delta)
  }

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

}
