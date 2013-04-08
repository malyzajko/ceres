package ceres.common


import Rational._


case class RationalInterval(xlo: Rational, xhi: Rational) {
  private val zero = Rational(0.0)

  case class DivisionByZeroException(s: String) extends Exception
  
  //def this(aa: RationalForm) = this(aa.interval._1, aa.interval._2)
  //def this(aa: FixedForm) = this(aa.qInterval._1, aa.qInterval._2)
  def this(i: Interval) = this(Rational(i.xlo), Rational(i.xhi))

  val mid: Rational = xlo/Rational(2.0) + xhi/Rational(2.0)
  val radius: Rational = abs(xhi - xlo) / Rational(2.0)

  // FIXME: not sound
  val toInterval: Interval = Interval(xlo.toDouble, xhi.toDouble)

  def unary_-(): RationalInterval = RationalInterval(-xhi, -xlo)

  def +(other: RationalInterval): RationalInterval = {
    RationalInterval(xlo + other.xlo, xhi + other.xhi)
  }

  def -(other: RationalInterval): RationalInterval = {
    RationalInterval(xlo - other.xhi, xhi - other.xlo)
  }

  def *(y: RationalInterval): RationalInterval = y match {
    case RationalInterval(ylo, yhi) =>
      if(xlo == 0.0 && xhi == 0.0) return RationalInterval(Rational(0.0), Rational(0.0))
      else if(xlo >= zero) {
        if(ylo >= zero) RationalInterval(xlo * ylo, xhi * yhi)
        else if(yhi <= zero) RationalInterval(xhi * ylo, xlo * yhi)
        else RationalInterval(xhi * ylo, xhi * yhi)
      }
      else if(xhi <= zero) {
        if(ylo >= zero) RationalInterval( xlo * yhi, xhi * ylo)
        else if(yhi <= zero) RationalInterval(xhi * yhi, xlo * ylo)
        else RationalInterval(xlo * yhi, xlo * ylo)
      }
      else {
        if(ylo >= zero) RationalInterval(xlo * yhi, xhi * yhi)
        else if(yhi <= zero) RationalInterval(xhi * ylo, xlo * ylo)
        else {
          val a = min(xlo * yhi, xhi * ylo)
          val b = max(xlo * ylo, xhi * yhi)
          RationalInterval(a, b)
        }
     }
  }

  def /(y: RationalInterval): RationalInterval = y match {
    case RationalInterval(ylo, yhi) =>

      if(xlo == 0.0 && ylo == 0.0) return RationalInterval(Rational(0.0), Rational(0.0))
      else if(ylo >= zero) {
        if(xlo >= zero) RationalInterval(xlo / yhi, xhi/ ylo)
        else if(xhi <= zero) RationalInterval(xlo / ylo, xhi / yhi)
        else RationalInterval( xlo / ylo, xhi / ylo)
      }
      else if(yhi <= zero) {
        if(xlo >= zero) RationalInterval(xhi / yhi, xlo / ylo)
        else if(xhi <= zero) RationalInterval(xhi / ylo, xlo / yhi)
        else RationalInterval( xhi / yhi, xlo / yhi)
      }
      else {
        throw DivisionByZeroException("trying to divide by interval containing 0")
        return RationalInterval(Rational(0.0), Rational(0.0))
      }
  }


  def union(y: RationalInterval) = {
    val temp = RationalInterval(min(this.xlo, y.xlo), max(this.xhi, y.xhi))
    temp
  }

  override def toString = "[" + xlo + ", " + xhi + "]"

}
