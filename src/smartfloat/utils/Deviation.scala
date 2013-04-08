package ceres.smartfloat

import ceres.common.DirectedRounding.{addUp}
import scala.Double.NaN
import scala.collection.immutable.HashMap

// Fixme: value should really be a val...
sealed abstract class NoiseTerm[T](val index: Int, var value: T) {
  def unary_-(): NoiseTerm[T];
}


/**
 * Represents one noise value.
 * Is supposed to be immutable, although we cannot guarantee that with a simple
 * implementation. Do not change the value array!!
 */
sealed abstract class Deviation(i: Int, v: Array[Double])
  extends NoiseTerm[Array[Double]](i, v) {
  val doubleValue = value(0) + value(1)

  def unary_-(): Deviation;
  //information from which noise symbols this one was computed.
  var comesFrom: Array[Int] = Array.fill(8){0}

  def getInitialMap: HashMap[Int, Double] = {
    var map: HashMap[Int, Double] = HashMap.empty
    if(comesFrom(0) == 0) { map += (index -> NaN); return map}

    var i = 0
    while(i < comesFromArraySize) {
      if(comesFrom(i) != 0) map += (comesFrom(i) -> NaN)  else i = comesFromArraySize
      i += 1
    }
    map
  }

  def computeFactor(m: HashMap[Int, Double]): Double = {
    if(comesFrom(0) == 0) return m(index)
    var factor = 1.0
    var i = 0
    while(i < comesFromArraySize) {
      if(comesFrom(i) != 0) factor = factor * m(comesFrom(i))   else i = comesFromArraySize
      i += 1
    }
    factor
  }

  //number of entries in the comesFrom array
  def filled: Int = {
    var i = 0
    while(i < comesFromArraySize) {
      if(comesFrom(i) != 0) i += 1  else return i
    }
    return i
  }

  def isQuadratic: Boolean = {
    if(comesFrom(0) == 0) return false

    var i = 0
    while(i < comesFromArraySize-1) {
      if(comesFrom(i) != comesFrom(i+1)) return false
      i += 2
    }
    return true
  }


  override def toString: String = {
    var str = doubleFormat.format(doubleValue) + "e" + index
    if(comesFrom(0) != 0) {
      str += "<"
      var i = 0
      while(i < comesFromArraySize) {
        if(comesFrom(i) != 0) str += comesFrom(i) +","    else i = comesFromArraySize
        i += 1
      }
      str += ">"
    }
    str += " "
    return str
  }

}

case class Noise(val i: Int, val v: Array[Double]) extends Deviation(i, v) {

  def this(ii: Int, vv: Array[Double], cf: Array[Int]) = {
    this(ii, vv)
    var i = 0
    while(i < comesFromArraySize) {
      comesFrom(i) = cf(i)
      i += 1
    }
  }

  //precondition: assumes we can actually merge these!
  def this(ii: Int, vv: Array[Double], xi:Deviation, yi:Deviation) = {
    this(ii, vv)

    val arr1 = if(xi.comesFrom(0) == 0) Array(xi.index, 0) else xi.comesFrom
    val arr2 = if(yi.comesFrom(0) == 0) Array(yi.index, 0) else yi.comesFrom

    var i = 0; var j = 0; var k = 0
    var curr1 = arr1(i)
    var curr2 = arr2(j)

    while(curr1 != 0 || curr2 != 0) {
      if(curr1 == curr2) {comesFrom(k) = curr1; comesFrom(k+1) = curr2; i+=1; j+=1; k+=2}
      else if(curr1 == 0) {comesFrom(k) = curr2; j+=1; k+=1}
      else if(curr2 == 0) {comesFrom(k) = curr1; i+=1; k+=1}
      else if(curr1 < curr2) {comesFrom(k) = curr1; i+=1; k+=1}
      else  {comesFrom(k) = curr2; j+=1; k+=1}
      curr1 = arr1(i)
      curr2 = arr2(j)
    }
  }

  def this(i: Int, d: Double) = this(i, Array(d, 0.0))
  def unary_-(): Noise = new Noise(index, Array(-value(0), -value(1)))
}

case class Uncertainty(val i: Int, val v: Array[Double]) extends Deviation(i, v) {

  def this(ii: Int, vv: Array[Double], cf: Array[Int]) = {
    this(ii, vv)
    var i = 0
    while(i < comesFromArraySize) {
      comesFrom(i) = cf(i)
      i += 1
    }
  }

  def this(ii: Int, vv: Array[Double], xi:Deviation, yi:Deviation) = {
    this(ii, vv)

    val arr1 = if(xi.comesFrom(0) == 0) Array(xi.index, 0) else xi.comesFrom
    val arr2 = if(yi.comesFrom(0) == 0) Array(yi.index, 0) else yi.comesFrom
    var i = 0; var j = 0; var k = 0
    var curr1 = arr1(i)
    var curr2 = arr2(j)

    while(curr1 != 0 || curr2 != 0) {
      if(curr1 == curr2) {comesFrom(k) = curr1; comesFrom(k+1) = curr2; i+=1; j+=1; k+=2}
      else if(curr1 == 0) {comesFrom(k) = curr2; j+=1; k+=1}
      else if(curr2 == 0) {comesFrom(k) = curr1; i+=1; k+=1}
      else if(curr1 < curr2) {comesFrom(k) = curr1; i+=1; k+=1}
      else  {comesFrom(k) = curr2; j+=1; k+=1}
      curr1 = arr1(i)
      curr2 = arr2(j)
    }
  }


  def this(i: Int, d: Double) = this(i, Array(d, 0.0))
  def unary_-(): Uncertainty = new Uncertainty(index, Array(-value(0), -value(1)))
}

//@invariant v instanceof StandardAffineForm
case class AffineNoise(val index: Int, var value: AffineForm) {
  def this(i: Int, d: Double) = this(i, new AForm(d)) //d is gonna be the x0
  def this(i: Int, ad: Array[Double]) = this(i, new AForm(ad))

  def unary_-(): AffineNoise = new AffineNoise(index, -value)

  def cleanUp = value match {
    case aa: AForm =>
      var x0new = aa.x0
      var newNoise = Array(0.0, 0.0)
      var deviation = Queue.empty
      val iter = aa.xnoise.getIterator

      while(iter.hasNext) {
        val xi = iter.next
        xi match {
          case n:Noise => newNoise = ceres.common.DDouble.addUp(newNoise, n.v)
          case u:Uncertainty =>
            deviation :+ xi
          case _=> assert(false, "error in cleanup, wrong type of Affine form")
        }
      }
      deviation :+ new Noise(AffineForm.newIndex, newNoise)
      value = AForm(x0new, deviation)

    case EmptyForm => ;
    case FullForm => ;
    case _ => assert(false, "error in cleanup, the affine form is not an AForm")
  }
}
