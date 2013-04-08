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

  def formatQueue(q: AffineQueue): String = {
    var s = ""
    val iter = q.getIterator
    while(iter.hasNext) {
      val xi = iter.next
      xi match {
        case AffineNoise(i, v) => s += "{" + v.toString + "}p" + xi.index + "\n "
        case _ => s += "XXXX"
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

  def sumQueue(q: AffineQueue): Array[Double] = {
    //we're not just summing the radius', need to take into account the central
    //values as well, since these are also roundoff errors
    var sum = zero
    if(q.size == 1) sum = Array(q.head.value.radius + math.abs(q.head.value.x0), 0.0)
    else {
      val iter = q.getIterator
      while(iter.hasNext) {
        iter.next.value match {
          case a: AForm =>
            sum = addUp(sum, a.radiusExt)
            sum = addUp(sum, Array(math.abs(a.x0), 0.0))
          case _=> assert(false, "wrong type of affine form")
        }
      }
    }
    sum
  }

  /* ####################################################################
    #####################    Smart Intervals     #########################
     ####################################################################*/
  //@return returns the min and max values of this queue
  def sumQueueSmart(queue: Queue): (Array[Double], Array[Double]) = {

    var sumQuadratic = zero
    var sumNoise = zero
    var dev = Queue.empty

    val iter = queue.getIterator

    while(iter.hasNext) {
      val xi = iter.next
      if(xi.doubleValue < smartPackingThreshold) sumNoise = addUp(sumNoise, abs(xi.value))
      else dev :+ xi
    }


    if(dev.size > smartQueueLimit) {
      val before = dev.size
      var newQueue = packNoiseSymbolsStdDevWithUncertainty(dev, smartPackingFactor, smartPackingAvrgScale)
      if(newQueue.size > smartQueueLimit)
        newQueue = packNoiseSymbolsStdDevWithUncertainty(newQueue, 2.0*smartPackingFactor, smartPackingAvrgScale)
      if(printPackingInfo) println("--------> smartMinMaxradius packed from " + before + " to: " + newQueue.size)
      dev = newQueue
    }

    if(dev.size > smartQueueLimit) {
      val sum = sumQueue(queue) //worst case
      return (Array(-sum(0), -sum(1)), sum)
    }

    var map: HashMap[Int, Double] = HashMap.empty
    val devIter = dev.getIterator
    while (devIter.hasNext){
      map ++= devIter.next.getInitialMap
    }

    var (resLo, resHi) = computeRadius(map, dev)
    resLo = addDown(sumQuadratic, subDown(resLo, sumNoise))
    resHi = addUp(sumQuadratic, addUp(resHi, sumNoise))
    (resLo, resHi)
  }


  //@return (neg. radius, pos. radius)
  private def computeRadius(m: HashMap[Int, Double], dev: Queue): (Array[Double], Array[Double]) = {
    val nan = m.find((p:(Int, Double)) => p._2 != p._2);  //find not assigned yet

    nan match {
      case Some((key:Int, value:Double)) =>  //its convex, so try only max and min values
        val (aMin, aMax) = computeRadius(m + (key -> 1.0), dev)
        val (cMin, cMax) = computeRadius(m + (key -> 0.0), dev)
        val (bMin, bMax) = computeRadius(m + (key -> -1.0), dev)
        return (min(aMin, min(cMin, bMin)), max(aMax, max(cMax, bMax)))
      case None =>
        val value = evalRadius(m, dev: Queue)
        return (value, value)
    }
  }

  private def evalRadius(m: HashMap[Int, Double], dev: Queue): Array[Double] = {
    var sum = zero
    val elemX = dev.elements
    var i = 0
    while (i < dev.tail) {
      val xi = elemX(i)
      sum = add(sum, mult(Array(xi.computeFactor(m), 0.0), xi.value))
      i += 1
    }
    sum
  }


     /* ####################################################################
    #####################    Eta computation     #########################
     ####################################################################*/
  def computeEtaSmart(dev: Queue, xq: Queue, yq: Queue): Queue = {
    var sum = zero
    val elemX = xq.elements
    val elemY = yq.elements

    var i = 0
    while (i < xq.tail) {
      val xi = elemX(i)
      var j = 0
      while (j < yq.tail) {
        val yj = elemY(j)
        if((xi.filled + yj.filled) >= 8) {
          sum = add(sum, abs(mult(xi.value, yj.value)))
        }
        else if(xi.index == yj.index) {
          dev :<+ new Noise(newIndex, mult(xi.value, yj.value), xi, yj)
        }
        else {
          dev :<+ new Noise(newIndex, mult(xi.value, yj.value), xi, yj)
        }
        j += 1
      }
      i += 1
    }
    if(notZero(sum)) dev :+ new Noise(newIndex, sum)
    dev
  }

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

    if(smartRoundoffComputation) {
      //these are signed!
      var (minVal, maxVal) = sumQueueSmart(xqueue)
      val zlo = abs(addDown(xn, 0.0, minVal(0), minVal(1)))
      val zhi = abs(addUp(xn, 0.0, maxVal(0), maxVal(1)))
      val m = max(zlo, zhi)
      return multUp(m, Array(ulp, 0.0))
    }
    else {
      var sum = sumQueue(xqueue)
      val zlo = abs(subDown(xn, 0.0, sum(0), sum(1)))
      val zhi = abs(addUp(xn, 0.0, sum(0), sum(1)))
      val m = max(zlo, zhi)
      return multUp(m, Array(ulp, 0.0))
    }
  }

  def getRoundoff1(xn: Double, xqueue:Queue): Array[Double] = {
    if(smartRoundoffComputation) {
      var (minVal, maxVal) = sumQueueSmart(xqueue)
      val zlo = abs(addDown(xn, 0.0, minVal(0), minVal(1)))
      val zhi = abs(addUp(xn, 0.0, maxVal(0), maxVal(1)))

      val m = max(zlo, zhi)
      return multUp(m, Array(2*ulp, 0.0))
    }
    else {
      var sum = sumQueue(xqueue)
      val zlo = abs(subDown(xn, 0.0, sum(0), sum(1)))
      val zhi = abs(addUp(xn, 0.0, sum(0), sum(1)))

      val m = max(zlo, zhi)
      return multUp(m, Array(2*ulp, 0.0))
    }
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

  def packNoiseSymbolsStdDev(queue: AffineQueue): AffineQueue = {
    var sum = 0.0
    var counter = 0
    var iter = queue.getIterator
    while(iter.hasNext) {
      val ai = iter.next
      val v = math.abs(ai.value.x0) + ai.value.radius
      sum += v
      counter += 1
    }
    val avrg = sum/counter

    //compute st. deviation
    var devSum = 0.0
    iter = queue.getIterator
    while(iter.hasNext) {
      val ai = iter.next
      val v = math.abs(ai.value.x0) + ai.value.radius
      val diff = v - avrg
      devSum += diff * diff
    }
    val stdDev = math.sqrt(devSum/counter)
    val threshold = avrg + stdDev

    //Now compute the new queue
    var newNoise = Array(0.0, 0.0)
    var newDev = new AffineQueue

    val iter2 = queue.getIterator
    while(iter2.hasNext) {
      val ai = iter2.next
      val v =  addUp(Array(math.abs(ai.value.x0), 0.0), ai.value.radiusExt)
      if(v(0) < threshold) newNoise = addUp(newNoise, v)     //this way we keep the
      else newDev :+ ai
    }
    newDev :+ new AffineNoise(newIndex, newNoise)
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

  def packNoiseSymbolsBruteForce(queue: AffineQueue): AffineQueue = {
    var newNoise = Array(0.0, 0.0)
    var newDev = new AffineQueue

    val iter2 = queue.getIterator
    while(iter2.hasNext) {
      val ai = iter2.next
      newNoise = addUp(newNoise, addUp(Array(math.abs(ai.value.x0), 0.0), ai.value.radiusExt))

    }
    newDev :+ new AffineNoise(newIndex, newNoise)
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
        deviation :+ new Noise(xi.index, zi, xi.comesFrom)
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
          case n:Noise => deviation :+ new Noise(xi.index, zi, xi.comesFrom)
          case u:Uncertainty => deviation :+ new Uncertainty(xi.index, zi, xi.comesFrom)
        }
      }
    }
    DoubleQueueIterator.iterate[Array[Double], Deviation](iterX, iterY, new Noise(Int.MaxValue, 0.0),
      fx, fy, fCouple)
    return (deviation, delta)
  }

  def addAffineQueues(xn: AffineQueue, yn: AffineQueue): AffineQueue = {
    var deviation = new AffineQueue
    val iterX = xn.getIterator
    val iterY = yn.getIterator

    var xi: AffineNoise = if(iterX.hasNext) iterX.next
      else new AffineNoise(Int.MaxValue, 0.0)
    var yi: AffineNoise = if(iterY.hasNext) iterY.next
      else new AffineNoise(Int.MaxValue, 0.0)

    while (iterX.hasNext || iterY.hasNext) {
      if(xi.index == yi.index) {
        val zi: AffineForm =  xi.value + yi.value
        if(zi.isNonZero) deviation :+ new AffineNoise(xi.index, zi)
        if(iterX.hasNext) xi = iterX.next else xi = new AffineNoise(Int.MaxValue, 0.0)
        if(iterY.hasNext) yi = iterY.next else yi = new AffineNoise(Int.MaxValue, 0.0)
      }
      else if(xi.index < yi.index) {
        deviation :+ xi
        if(iterX.hasNext) xi = iterX.next else xi = new AffineNoise(Int.MaxValue, 0.0)
      }
      else {
        deviation :+ yi
        if(iterY.hasNext) yi = iterY.next else yi = new AffineNoise(Int.MaxValue, 0.0)
      }
    }//both
    if(xi.index == yi.index && xi.index != Int.MaxValue) {
      val zi =  xi.value + yi.value
      if(zi.isNonZero) deviation :+ new AffineNoise(xi.index, zi)
    }
    else {
      if(xi.index < yi.index){
        if(xi.index != Int.MaxValue) deviation :+ xi
        if(yi.index != Int.MaxValue) deviation :+ yi
      }
      else {
        if(yi.index != Int.MaxValue) deviation :+ yi
        if(xi.index != Int.MaxValue) deviation :+ xi
      }
    }

    return deviation
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
        deviation :+ new Noise(xi.index, zi, xi.comesFrom)
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
          case n:Noise => deviation :+ new Noise(xi.index, zi, xi.comesFrom)
          case u:Uncertainty => deviation :+ new Uncertainty(xi.index, zi, xi.comesFrom)
        }
      }
    }
    DoubleQueueIterator.iterate[Array[Double], Deviation](iterX, iterY, new Noise(Int.MaxValue, 0.0), fx, fy, fCouple)
    return (deviation, delta)
  }




  // returns the added Queue + the error committed in the computation
  def subtractAffineQueues(xn: AffineQueue, yn: AffineQueue): AffineQueue = {
    var deviation = new AffineQueue
    val iterX = xn.getIterator
    val iterY = yn.getIterator

    var xi: AffineNoise = if(iterX.hasNext) iterX.next
      else new AffineNoise(Int.MaxValue, 0.0)
    var yi: AffineNoise = if(iterY.hasNext) iterY.next
      else new AffineNoise(Int.MaxValue, 0.0)

    while (iterX.hasNext || iterY.hasNext) {
      if(xi.index == yi.index) {
        val zi =  xi.value - yi.value
        if(zi.isNonZero) deviation :+ new AffineNoise(xi.index, zi)
        if(iterX.hasNext) xi = iterX.next else xi = new AffineNoise(Int.MaxValue, 0.0)
        if(iterY.hasNext) yi = iterY.next else yi = new AffineNoise(Int.MaxValue, 0.0)
      }
      else if(xi.index < yi.index) {
        deviation :+ xi
        if(iterX.hasNext) xi = iterX.next else xi = new AffineNoise(Int.MaxValue, 0.0)
      }
      else {
        deviation :+ -yi
        if(iterY.hasNext) yi = iterY.next else yi = new AffineNoise(Int.MaxValue, 0.0)
      }
    }//both
    if(xi.index == yi.index && xi.index != Int.MaxValue) {
      val zi =  xi.value - yi.value
      if(zi.isNonZero) deviation :+ new AffineNoise(xi.index, zi)
    }
    else {
      if(xi.index < yi.index){
        if(xi.index != Int.MaxValue) deviation :+ xi
        if(yi.index != Int.MaxValue) deviation :+ -yi
      }
      else {
        if(yi.index != Int.MaxValue) deviation :+ -yi
        if(xi.index != Int.MaxValue) deviation :+ xi
      }
    }

    return deviation
  }


  def multiplyQueues(a: Double, xqueue: Queue, b: Double, yqueue: Queue): (Queue, Array[Double]) = {
    var delta = Array(0.0, 0.0)
    var deviation = new Queue
    val iterX = xqueue.getIterator
    val iterY = yqueue.getIterator

    val fx = (d: Deviation) => {
      val zi =  mult(b, 0.0, d.value(0), d.value(1))
      delta =  addUp(delta,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0) deviation :+ new Noise(d.index, zi, d.comesFrom)
    }
    val fy = (d: Deviation) => {
      val zi =  mult(a, 0.0, d.value(0), d.value(1))
      delta =  addUp(delta,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0) deviation :+ new Noise(d.index, zi, d.comesFrom)
    }
    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi =  add( mult(b, 0.0, xi.value(0), xi.value(1)),
            mult(a, 0.0, yi.value(0), yi.value(1)))
      delta =  addUp(delta,  rdOff(zi))// Fixme: this should be rdOffABplusCD
      if(zi(0) != 0.0 || zi(1) != 0.0) deviation :+ new Noise(xi.index, zi, xi.comesFrom)
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
          case n:Noise => deviation :+ new Noise(d.index, zi, d.comesFrom)
          case u:Uncertainty => deviation :+ new Uncertainty(d.index, zi, d.comesFrom)
        }
    }
    val fy = (d: Deviation) => {
      val zi =  mult(a, 0.0, d.value(0), d.value(1))
      delta =  addUp(delta,  rdOff(zi))
      if(zi(0) != 0.0 || zi(1) != 0.0)
        d match {
          case n:Noise => deviation :+ new Noise(d.index, zi, d.comesFrom)
          case u:Uncertainty => deviation :+ new Uncertainty(d.index, zi, d.comesFrom)
        }
    }
    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi =  add( mult(b, 0.0, xi.value(0), xi.value(1)),
            mult(a, 0.0, yi.value(0), yi.value(1)))
      delta =  addUp(delta,  rdOff(zi))// Fixme: this should be rdOffABplusCD
      if(zi(0) != 0.0 || zi(1) != 0.0)
        xi match {
          case n:Noise => deviation :+ new Noise(xi.index, zi, xi.comesFrom)
          case u:Uncertainty => deviation :+ new Uncertainty(xi.index, zi, xi.comesFrom)
        }
    }
    DoubleQueueIterator.iterate[Array[Double], Deviation](iterX, iterY, new Noise(Int.MaxValue, 0.0), fx, fy, fCouple)
    return(deviation, delta)
  }

  def multiplyAffineQueues(a: AffineForm, xqueue: AffineQueue,
    b: AffineForm, yqueue: AffineQueue): AffineQueue = {
    //println("Multiply queues: a:"+a + ", xqueue:"+xqueue+", b:"+b+", yqueue:"+yqueue)
    var deviation = new AffineQueue
    val iterX = xqueue.getIterator
    val iterY = yqueue.getIterator

    var xi: AffineNoise = if(iterX.hasNext) iterX.next
      else new AffineNoise(Int.MaxValue, 0.0)
    var yi: AffineNoise = if(iterY.hasNext) iterY.next
      else new AffineNoise(Int.MaxValue, 0.0)

    while (iterX.hasNext || iterY.hasNext) {
      if(xi.index == yi.index) {
        val zi =  b * xi.value + a * yi.value
        if(zi.isNonZero) deviation :+ new AffineNoise(xi.index, zi)
        if(iterX.hasNext) xi = iterX.next else xi = new AffineNoise(Int.MaxValue, 0.0)
        if(iterY.hasNext) yi = iterY.next else yi = new AffineNoise(Int.MaxValue, 0.0)
      }
      else if(xi.index < yi.index) {
        val zi =  b * xi.value
        if(zi.isNonZero) deviation :+ new AffineNoise(xi.index, zi)
        if(iterX.hasNext) xi = iterX.next else xi = new AffineNoise(Int.MaxValue, 0.0)
      }
      else {
        val zi =  a * yi.value
        if(zi.isNonZero) deviation :+ new AffineNoise(yi.index, zi)
        if(iterY.hasNext) yi = iterY.next else yi = new AffineNoise(Int.MaxValue, 0.0)
      }
    }//both
    if(xi.index == yi.index && xi.index != Int.MaxValue) {
      val zi =  b * xi.value + a * yi.value
      if(zi.isNonZero) deviation :+ new AffineNoise(xi.index, zi)
    }
    else {
      if(xi.index < yi.index){
        if(xi.index != Int.MaxValue) {
          val zi =   b * xi.value
          if(zi.isNonZero) deviation :+ new AffineNoise(xi.index, zi)
        }
        if(yi.index != Int.MaxValue) {
          val zi =  a * yi.value
          if(zi.isNonZero) deviation :+ new AffineNoise(yi.index, zi)
        }
      }
      else {
        if(yi.index != Int.MaxValue) {
          val zi =  a * yi.value
          if(zi.isNonZero) deviation :+ new AffineNoise(yi.index, zi)
        }
        if(xi.index != Int.MaxValue) {
          val zi =   b * xi.value
          if(zi.isNonZero) deviation :+ new AffineNoise(xi.index, zi)
        }
      }
    }
    return deviation
  }

}




