package ceres.smartfloat

import ceres.common.DDouble

object BasicQueue {
  val MAX_SIZE = 90
}


abstract class BasicQueue[A, T <: NoiseTerm[A]] {
  import BasicQueue._

  def getNewArray(size: Int): Array[T]
  def getNewZeroValue: A

  def getIterator: Iterator[T]

  var elements =  getNewArray(MAX_SIZE)
  var tail = 0

  def size: Int = tail

  //will return null, if queue empty
  def head: T = {
    return elements(0)
  }

  /**
   * Checks if the noise terms are sorted by index.
   */
  def isSorted: Boolean = {
    var sorted = true
    var i = 0
    while(i < tail-1) {
      if(elements(i).index >= elements(i+1).index) sorted = false
      i += 1
    }
    sorted
  }

  /**
   * Adds (modifies) an element to the end of this queue.
   */
  def :+(d: T): Unit = {
    if(tail == elements.length) {
      val tmp = getNewArray(elements.length*2)
      var i = 0
      while(i < elements.length) {
        tmp(i) = elements(i)
        i += 1
      }
      elements = tmp
    }
    elements(tail) = d
    tail += 1
  }

  //find the deviation with index, return its value, or else return 0.0
  def find(indexSearched: Int): A = {
    var i = 0
    while(i < tail) {
      if(elements(i).index == indexSearched) {
        return elements(i).value
      }
      i += 1
    }
    return getNewZeroValue
  }

  override def toString: String = {
    var str = ""
    var i = 0
    while(elements(i) != null) {
      str += elements(i) + " "
      i += 1
    }
    str
  }

  def toStringVertical: String = {
    var str = ""
    var i = 0
    while(elements(i) != null) {
      str += elements(i) + "\n"
      i += 1
    }
    str
  }

}

/* ------------------------------------------------------
          Queues for SmartFloat and AffineFloat
  -------------------------------------------------------*/
object Queue {
  def empty: Queue = new Queue()
}

/**
 * A mutable queue.
 */
class Queue extends BasicQueue[Array[Double], Deviation] {
  import Queue._

  def getNewArray(size: Int): Array[Deviation] = new Array[Deviation](size)
  def getNewZeroValue: Array[Double] = Array(0.0, 0.0)

  def this(d: Deviation) = {
    this()
    elements(0) = d
    tail = 1
  }

  /**
   * Special add, it searches the queue first, if a deviation with the same index already exists.
   * If not, adds an element to the end of this queue.
   */
  def :<+(d: Deviation): Unit = {
    if(tail == elements.length) {
      val tmp = new Array[Deviation](elements.length*2)
      var i = 0
      while(i < elements.length) {
        tmp(i) = elements(i)
        i += 1
      }
      elements = tmp
    }

    var i = 0
    val dcomesFrom = d.comesFrom
    while(i < tail) {
      val currElem = elements(i)
      if(compareArray(dcomesFrom, currElem.comesFrom)) {
        currElem.value = DDouble.add(currElem.value, d.value)
        return
      }
      i += 1
    }
    //index not found
    elements(tail) = d
    tail += 1
  }

  private def compareArray(a1: Array[Int], a2: Array[Int]): Boolean = {
    var i = 0
    while(i < comesFromArraySize && a1(i) != 0) {
      if(a1(i) != a2(i)) return false
      i += 1
    }
    if(i > 0) return true else return false
  }

  /**
   * Creates a new (deep) copy of this queue and adds a new element.
   * Generally not recommended as this is very slow.
   */
  def +(d: Deviation): Queue = {
    val tmp = copy
    tmp :+ d
    return tmp
  }

  /**
   * Copy, but does NOT do a deep copy of the Deviations, since they
   * are assumed to be immutable anyway.
   */
  def copy: Queue = {
    val newCopy = new Queue
    var i = 0
    while(i < elements.length && elements(i) != null) {
      newCopy :+ elements(i)//.copy
      i += 1
    }
    newCopy
  }

  def getIterator: Iterator[Deviation] = new Iterator[Deviation](elements)
}
