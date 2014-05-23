package ceres.smartratfloat

import ceres.common.Rational
import Rational._
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
sealed abstract class Deviation(i: Int, v: Rational) extends NoiseTerm[Rational](i, v) {
  val doubleValue = value.toDouble

  def unary_-(): Deviation;

  override def toString: String = {
    doubleFormat.format(doubleValue) + "e" + index + " "
  }

}

case class Noise(val i: Int, val v: Rational) extends Deviation(i, v) {

  def this(i: Int, d: Double) = this(i, Rational(d))
  def unary_-(): Noise = new Noise(index, -value)
}

case class Uncertainty(val i: Int, val v: Rational) extends Deviation(i, v) {

  def this(i: Int, d: Double) = this(i, Rational(d))
  def unary_-(): Uncertainty = new Uncertainty(index, -value)
}

//@invariant v instanceof StandardAffineForm
/*case class AffineNoise(val index: Int, var value: AffineForm) {
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
}*/
