package ceres.smartfloat

object AffineQueue {
  val MAX_SIZE = 90

  def empty: AffineQueue = new AffineQueue()

}

/**
 * A mutable queue.
 */
class AffineQueue {
  import AffineQueue._

  private var elements =  new Array[AffineNoise](MAX_SIZE)  //Array.fill(MAX_SIZE){null}
  private var tail = 0

  def this(d: AffineNoise) = {
    this()
    elements(0) = d
    tail = 1
  }

  //puts the coefficients into the form we want: functions of uncertainties
  def cleanUp: Unit = {
    var i = 0
    while(i < tail) {
      elements(i).cleanUp
      i += 1
    }
  }

  /**
   * Adds (modifies) an element to the end of this queue.
   */
  def :+(d: AffineNoise): Unit = {
    if(tail == elements.length) {
      val tmp = new Array[AffineNoise](elements.length*2)
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

  /**
   * Creates a new (deep) copy of this queue and adds a new element.
   * Generally not recommended as this is very slow.
   */
  def +(d: AffineNoise): AffineQueue = {
    val tmp = copy
    tmp :+ d
    return tmp
  }

  //find the deviation with index, return its value, or else return 0.0
  def find(indexSearched: Int): AffineForm = {
    var i = 0
    while(i < tail) {
      if(elements(i).index == indexSearched) {
        return elements(i).value
      }
      i += 1
    }
    return new AForm(0.0)
  }

  /**
   * Copy, but does NOT do a deep copy of the Deviations, since they
   * are assumed to be immutable anyway.
   */
  def copy: AffineQueue = {
    val newCopy = new AffineQueue
    var i = 0
    while(i < elements.length && elements(i) != null) {
      newCopy :+ elements(i)//.copy
      i += 1
    }
    newCopy
  }

  def getIterator: AffineIterator = new AffineIterator(elements)

  def size: Int = tail

  //will return null, if queue empty
  def head: AffineNoise = {
    return elements(0)
  }



  override def toString: String = {
    var str = ""
    var i = 0
    while(elements(i) != null) {
      str += elements(i) + "\n"
      i += 1
    }
    str
  }
}

class AffineIterator(elements: Array[AffineNoise]) {
  import AffineQueue._

  val max = elements.length
  var current = 0

  def hasNext: Boolean = (max > current && elements(current) != null)

  def next: AffineNoise = {
    val tmp = elements(current)
    current += 1
    return tmp
  }
}
