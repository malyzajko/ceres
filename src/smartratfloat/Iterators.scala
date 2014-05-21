package ceres.smartratfloat

/*
 A simple in order iterator.
 Separate (light-weight) implementation for efficiency.
*/
class Iterator[T](elements: Array[T]) {
  //import Queue._

  val max = elements.length
  var current = 0

  def hasNext: Boolean = (max > current && elements(current) != null)

  def next: T = {
    val tmp = elements(current)
    current += 1
    return tmp
  }
}

object DoubleQueueIterator {

  def iterate[A, T <: NoiseTerm[A]](iterX: Iterator[T], iterY: Iterator[T], dummy: T,
    fx: (T) => Unit, fy: (T) => Unit, fCouple: (T, T) => Unit): Unit = {

    var xi: T = if(iterX.hasNext) iterX.next else dummy
    var yi: T = if(iterY.hasNext) iterY.next else dummy

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



