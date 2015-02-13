
import ceres.smartfloat.AffineFloat
import ceres.smartfloat.SmartFloat
import ceres.smartfloat.SmartFloat.{certainly, possibly}
import ceres.smartfloat.IntervalFloat
import scala.math.{min, max, abs}
import ceres.Interval


object Main extends App {

  smartfloatDemo
  //quaddoubleDemo

  cubeRoot
  triangles
  springSimulation(0.1, 5.0)
  quadraticEquation
  dopplerEffect

  def smartfloatDemo = {
    val x = new SmartFloat(5.0, 0.5)
    val y = new SmartFloat(6.0, 0.6)


    if (x - 2.0 < y) println("x - 2 < y")
    else println("xxx")

    if (certainly(x < y)) println("x < y")
    else println("---")

    if (possibly(x < y)) println("x < y")
    else println("---")

    // The following will throw an exception:
    //if (x < y) println("x < y")
    //else println("---")
  }

/*  def quaddoubleDemo = {
    import base.precision.QuadDouble
    import QuadDouble._

    val x1 = QuadDouble(3.46)
    val x2 = QuadDouble(9.75)

    val f1 = 2*(x2 - x1) + sin(2*x2) - sin(2*x1) - 1.2
    println(f1)

    println(f1.toLongString())  //default is 60 digits
  }
  */



  /**
   * Computes the cube root of 10 by Halley's method.
   */
  def cubeRoot = {
    println("~~~ Cube root ~~~")
    val a: AffineFloat = 10
    var xn = AffineFloat(1.6)

    for(i <- 1 until 5) {
        xn = xn * ((xn*xn*xn + 2.0*a)/(2.0*xn*xn*xn + a))
    }
    println("final:" + xn.toStringWithAbsErrors)
    println("intervals: " + xn.interval)
  }


  /**
   * Computes the area of a triangle with Kahan's method.
   */
  def triangles = {
    println("SmartFloat: textbook")
    println(triangleTextbook(9.0, SmartFloat(4.8, 0.09), SmartFloat(4.8, 0.09)))
    println(triangleTextbook(9.0, SmartFloat(4.7, 0.09), SmartFloat(4.7, 0.09)))

    println("\nIntervalFloat: textbook")
    println(triangleTextbook(9.0, IntervalFloat(4.8, 0.09), IntervalFloat(4.8, 0.09)))
    println(triangleTextbook(9.0, IntervalFloat(4.7, 0.09), IntervalFloat(4.7, 0.09)))

    println("\nSmartFloat: Kahan")
    println(triangleKahan(9.0, SmartFloat(4.8, 0.09), SmartFloat(4.8, 0.09)))
    println(triangleKahan(9.0, SmartFloat(4.7, 0.09), SmartFloat(4.7, 0.09)))


    println("\nIntervalFloat: Kahan")
    println(triangleKahan(9.0, IntervalFloat(4.8, 0.09), IntervalFloat(4.8, 0.09)))
    println(triangleKahan(9.0, IntervalFloat(4.7, 0.09), IntervalFloat(4.7, 0.09)))


    println("\nAffineFloat for these values: a = 9.0, b = 4.53, c=4.53")
    println(triangleKahan(AffineFloat(9.0), AffineFloat(4.61), AffineFloat(4.61)))
  }

  def triangleKahan(aa: SmartFloat, bb: SmartFloat, cc: SmartFloat): SmartFloat = {
    import SmartFloat._
    var a = aa
    var b = bb
    var c = cc

    // Using possibly here instead of certainly corresponds to the semantics used
    // in the original OOPSLA paper.
    if(b < a) {
      val t = a
      if(possibly(c < b)) {
        a = c; c = t
      }
      else {
        if(possibly(c < a)) {
          a = b; b = c; c = t
        }
        else {
          a = b; b = t
        }
      }
    }
    else if(c < b) {
      val t = c; c = b;
      if(possibly(c < a)) {
        b = a; a = t
      }
      else {
        b = t
      }
    }
    sqrt((a+(b+c)) * (c-(a-b)) * (c+(a-b)) * (a+(b-c))) / 4.0
  }

  def triangleKahan(aa: AffineFloat, bb: AffineFloat, cc: AffineFloat) = {
    import AffineFloat._
    var a = aa
    var b = bb
    var c = cc

    if(b < a) {
      val t = a
      if(c < b) {
        a = c; c = t
      }
      else {
        if(c < a) {
          a = b; b = c; c = t
        }
        else {
          a = b; b = t
        }
      }
    }
    else if(c < b) {
      val t = c; c = b;
      if(c < a) {
        b = a; a = t
      }
      else {
        b = t
      }
    }
    sqrt((a+(b+c)) * (c-(a-b)) * (c+(a-b)) * (a+(b-c))) / 4.0
  }

  def triangleKahan(aa: IntervalFloat, bb: IntervalFloat, cc: IntervalFloat) = {
    import IntervalFloat._
    var a = aa
    var b = bb
    var c = cc

    if(b < a) {
      val t = a
      if(c < b) {
        a = c; c = t
      }
      else {
        if(c < a) {
          a = b; b = c; c = t
        }
        else {
          a = b; b = t
        }
      }
    }
    else if(c < b) {
      val t = c; c = b;
      if(c < a) {
        b = a; a = t
      }
      else {
        b = t
      }
    }

    sqrt((a+(b+c)) * (c-(a-b)) * (c+(a-b)) * (a+(b-c))) / 4.0
  }


  def triangleTextbook(a: IntervalFloat, b: IntervalFloat, c: IntervalFloat) = {
    import IntervalFloat._
    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))
  }

  def triangleTextbook(a: SmartFloat, b: SmartFloat, c: SmartFloat): SmartFloat = {
    import SmartFloat._
    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))
  }

  def triangleTextbook(a: AffineFloat, b: AffineFloat, c: AffineFloat): AffineFloat = {
    import AffineFloat._
    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))
  }


  /**
   * Simulates a undamped, unforced spring.
   */
  def springSimulation(h:SmartFloat, tmax: Double) = {
    import SmartFloat._
    println("\nSpring simulation for h = " + h)
    //setup
    val k: SmartFloat = 1.0
    val m: SmartFloat = 1.0

    val xmax: SmartFloat = 5.0

    var x: SmartFloat = xmax  //current horizontal position
    var vx: SmartFloat = 0.0  //current velocity
    var t: SmartFloat = 0.0  //current 'time'

    var methodError = k*m*xmax * (h*h)/2.0

    while(possibly(t < tmax)) {    //global flag fails if 1.0
      val x_next = x + h * vx
      val vx_next = vx - h * k/m * x
      x = x_next.addError(methodError)
      vx = vx_next
      t = t + h
    }

    println("t: " + t.toStringWithAbsErrors +
       ",\n x: " + x.toStringWithAbsErrors)
  }


  /**
   * Computes the roots of a quadratic equation in two different ways.
   */
  def quadraticEquation {
    import AffineFloat._
    //println("\nRoots of quadratic equation for a = 2.999, b = 56.0001, c = 1.00074")

    var a = AffineFloat(2.999)
    var b = AffineFloat(56.0001)
    var c = AffineFloat(1.00074)

		if(true) {
				// Tom stuff
				a = AffineFloat(1)
  			c = AffineFloat(1e10)
				b = -(AffineFloat(1)+c)
    }
    if(true) {
				// ceres gives a large interval for this case, but actual error is 2e-9
				a = AffineFloat(1)
				c = a
				b = AffineFloat(-1e8)        
    }
    println("\nRoots of quadratic equation for a = "+a+", b = "+b+", c = "+c)

    val discr = b*b - a * c * 4.0

    //classical way
    var r2 = (-b + sqrt(discr))/(a * 2.0)
    var r1 = (-b - sqrt(discr))/(a * 2.0)
    println("classic r1 = " + r1.toStringWithErrors +
                 " , r2 = " + r2.toStringWithErrors)

    //smarter way
    val (rk1: AffineFloat, rk2: AffineFloat) =
    if(b*b - a*c > 10.0) {
      if(b > 0.0) ((-b - sqrt(discr))/(a * 2.0), c * 2.0 /(-b - sqrt(discr)))
      else if(b < 0.0)  (c * 2.0 /(-b + sqrt(discr)), (-b + sqrt(discr))/(a * 2.0))
      else  ((-b - sqrt(discr))/(a * 2.0), (-b + sqrt(discr))/(a * 2.0))
    }
    else {
      ((-b - sqrt(discr))/(a * 2.0), (-b + sqrt(discr))/(a * 2.0))
    }

    println("smarter r1 = " + rk1.toStringWithErrors + " , r2 = " + rk2.toStringWithErrors)

		// lower bound on the error
		val true_r1 = Interval.intersect(rk1.interval, r1.interval)
		val true_r2 = Interval.intersect(rk2.interval, r2.interval)
		println("classic r1 error = " + Interval.distance(true_r1,r1.value) + 
            ", r2 error = " + Interval.distance(true_r2,r2.value))
  }

  /**
   * Computes the frequency shift from doppler effect.
   * Parameters need to be tweaked a bit to get the best result:
   * maxNoiseCount = 200, smartQueueLimit = 23, packingFactor = 0.0, packingAvrgScale = 0.50,
   * smartPackingFactor = 0.0, smartPackingAvrgScale = 1.0
   */
  def dopplerEffect = {
    import SmartFloat._
    println("\nDoppler effect")

    val T = SmartFloat(10.0, 40.0)
    val v = SmartFloat(10010.0, 9990.0)
    val u = SmartFloat(0.0, 100.0)
    val q1 = SmartFloat(331.4) + T*0.6
    val q2 = q1 * v
    val q3 = q1 + u
    val q4 = q3 * q3
    val z = q2/q4

    println("q1: " + q1.toStringWithAbsErrors)
    println("q2: " + q2.toStringWithAbsErrors)
    println("q3: " + q3.toStringWithAbsErrors)
    println("q4: " + q4.toStringWithAbsErrors)
    println("z: " + z.toStringWithAbsErrors)
  }


}
