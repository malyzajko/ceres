package ceres.smartfloat

import ceres.DirectedRounding.{addUp}
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


  override def toString: String = {
    doubleFormat.format(doubleValue) + "e" + index + " "
  }

}

case class Noise(val i: Int, val v: Array[Double]) extends Deviation(i, v) {

  def this(i: Int, d: Double) = this(i, Array(d, 0.0))
  def unary_-(): Noise = new Noise(index, Array(-value(0), -value(1)))
}

case class Uncertainty(val i: Int, val v: Array[Double]) extends Deviation(i, v) {

  def this(i: Int, d: Double) = this(i, Array(d, 0.0))
  def unary_-(): Uncertainty = new Uncertainty(index, Array(-value(0), -value(1)))
}
