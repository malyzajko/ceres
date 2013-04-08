package ceres.affine

import ceres.common._


import collection.mutable.Queue

case class DivisionByZeroException(s: String) extends Exception

case class Deviation(index: Int, value: Rational) {
  def unary_-(): Deviation = new Deviation(index, -value)
  override def toString: String = value.toDouble.toString + "e" + index
  def toErrorString: String = value.toDouble.toString + "r" + index
}

object RationalAffineUtils {
  def sumQueue(q: Queue[Deviation]): Rational = {
    var sum = Rational(0)
    val iter = q.iterator
    while(iter.hasNext) {
      sum += Rational.abs(iter.next.value)
    }
    sum
  }

  def addQueues(xn: Queue[Deviation], yn: Queue[Deviation]): Queue[Deviation] = {
    var deviation = new Queue[Deviation]()
    val iterX = xn.iterator
    val iterY = yn.iterator

    val fx = (xi: Deviation) => { deviation += xi; val x = 0 }
    val fy = (yi: Deviation) => { deviation += yi; val x = 0 }

    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi =  xi.value + yi.value
      if(zi != 0) deviation += new Deviation(xi.index, zi)
      val x = 0
    }
    RationalDoubleQueueIterator.iterate(iterX, iterY,
      new Deviation(Int.MaxValue, Rational(0)), fx, fy, fCouple)
    return deviation
  }

  def subtractQueues(xn: Queue[Deviation], yn: Queue[Deviation]): Queue[Deviation] = {
    var deviation = new Queue[Deviation]()
    val iterX = xn.iterator
    val iterY = yn.iterator

    val fx = (xi: Deviation) => { deviation += xi; val x = 0 }
    val fy = (yi: Deviation) => { deviation += -yi; val x = 0 }

    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi =  xi.value - yi.value
      if(zi != 0) deviation += new Deviation(xi.index, zi)
      val x = 0
    }
    RationalDoubleQueueIterator.iterate(iterX, iterY,
      new Deviation(Int.MaxValue, Rational(0)), fx, fy, fCouple)
    return deviation
  }

  def multiplyQueues(a: Rational, xqueue: Queue[Deviation], b: Rational,
    yqueue: Queue[Deviation]): Queue[Deviation] = {
    var deviation = new Queue[Deviation]()
    val iterX = xqueue.iterator
    val iterY = yqueue.iterator

    val fx = (d: Deviation) => {
      val zi =  b * d.value
      if(zi != 0) deviation += new Deviation(d.index, zi)
      val x = 0
    }
    val fy = (d: Deviation) => {
      val zi =  a * d.value
      if(zi != 0) deviation += new Deviation(d.index, zi)
      val x = 0
    }
    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi = b * xi.value + a * yi.value
      if(zi != 0) deviation += new Deviation(xi.index, zi)
      val x = 0
    }
    RationalDoubleQueueIterator.iterate(iterX, iterY,
      new Deviation(Int.MaxValue, Rational(0)), fx, fy, fCouple)
    return deviation
  }

  def multiplyQueue(queue: Queue[Deviation], factor: Rational): Queue[Deviation] = {
    var deviation = new Queue[Deviation]()
    val iter = queue.iterator
    while(iter.hasNext) {
      val xi = iter.next
      val zi = factor * xi.value
      if(zi != 0.0) deviation += Deviation(xi.index, zi)
    }
    return deviation
  }

  def getIndices(q: Queue[Deviation]): collection.immutable.Set[Int] = {
    var i = 0
    var set = new collection.immutable.HashSet[Int]()
    while (i < q.size) {
      set += q(i).index
      i += 1
    }
    set
  }


}

object RationalDoubleQueueIterator {

  def iterate(iterX: Iterator[Deviation], iterY: Iterator[Deviation],
    dummy: Deviation, fx: (Deviation) => Unit, fy: (Deviation) => Unit,
    fCouple: (Deviation, Deviation) => Unit): Unit = {

    var xi: Deviation = if(iterX.hasNext) iterX.next else dummy
    var yi: Deviation = if(iterY.hasNext) iterY.next else dummy

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


