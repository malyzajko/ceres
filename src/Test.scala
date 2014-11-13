


object Test extends App {

  testRationals
  testQuadDoubles

  def testRationals = {
    import ceres.Rational._
    val u = fromReal(5.65)
    val v = fromReal(234.57)
    val T = fromReal(-13.78)
    println((- (331.4 + 0.6 * T) *v) / (((331.4 + 0.6 * T) + u)*((331.4 + 0.6 * T) + u)))
  }



  def testQuadDoubles = {
    import ceres.{QuadDouble => QD}
    import QD._

    val u = QD(5.65)
    val v = QD(234.57)
    val T = QD(-13.78)
    println((- (331.4 + 0.6 * T) *v) / (((331.4 + 0.6 * T) + u)*((331.4 + 0.6 * T) + u)))
  }
}