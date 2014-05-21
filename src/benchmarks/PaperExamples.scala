package ceres
package benchmarks


import smartfloat._
import scala.collection.mutable.LinkedList

/**
 * These are examples from the Technical Report.
 */
object PaperExamples extends App {

  dopplerEffect
  
  /**
   * Computes the cube root of 10 by Halley's method.
   */
  def cubeRoot = {
    import AffineFloat._
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
  def triangleKahan(aa: SmartFloat, bb: SmartFloat, cc: SmartFloat): SmartFloat = {
    import SmartFloat._
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
  
  def triangleExampleSection = {
    println("SmartFloat: textbook")
    println(triangleTextbook(9.0, SmartFloat(4.8, 0.09), SmartFloat(4.8, 0.09)))
    println(triangleTextbook(9.0, SmartFloat(4.7, 0.09), SmartFloat(4.7, 0.09)))
    
    println("\nIntervalFloat: textbook")
    println(triangleTextbook(9.0, IntervalFloat(4.8, 0.09), IntervalFloat(4.8, 0.09)))
    println(triangleTextbook(9.0, IntervalFloat(4.7, 0.09), IntervalFloat(4.7, 0.09)))
    
    println("\nSmartFloat: Kahan")
    println(triangleKahan(9.0, SmartFloat(4.8, 0.09), SmartFloat(4.8, 0.09)))
    println(triangleKahan(9.0, SmartFloat(4.7, 0.09), SmartFloat(4.7, 0.09)))
    
    
    println("IntervalFloat: Kahan")
    println(triangleKahan(9.0, IntervalFloat(4.8, 0.09), IntervalFloat(4.8, 0.09)))
    println(triangleKahan(9.0, IntervalFloat(4.7, 0.09), IntervalFloat(4.7, 0.09)))
    
    
    println("\nAffineFloat for these values: a = 9.0, b = 4.53, c=4.53")
    println("according to Mathematica  A=2.34216246234115877566387422739")
    println(triangleKahan(AffineFloat(9.0), AffineFloat(4.61), AffineFloat(4.61)))
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
    
    while(t < tmax) {    //global flag fails if 1.0
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
        
    var a = AffineFloat(2.999)
    var b = AffineFloat(56.0001)
    var c = AffineFloat(1.00074)
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
  }
  
  
  /**
   * Computes the frequency shift from doppler effect. 
   * Parameters need to be tweaked a bit to get the best result:
   * maxNoiseCount = 200, smartQueueLimit = 23, packingFactor = 0.0, packingAvrgScale = 0.50,
   * smartPackingFactor = 0.0, smartPackingAvrgScale = 1.0
   */
  def dopplerEffect = {
    import SmartFloat._
    
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
  
  
  
  /**
   * Analyzer the roundoff from the triangle example.
   * For this to work, you need to change the second import statement in SmartFloat.scala from
   * import tools.{SmartAForm => CurrentAForm}
   * to import tools.{ParamAForm => CurrentAForm}
   */
  def analyzeTriangles = {
    triangleTextbook(SmartFloat(9.0), SmartFloat(4.7, 0.19), SmartFloat(4.7, 0.19))
    
    println("\n********* affine float **************")
    triangleTextbook(AffineFloat(9.0), 4.51, 4.51)  //-1, -1
    triangleTextbook(AffineFloat(9.0), AffineFloat(4.89), AffineFloat(4.89))  //1, 1
    triangleTextbook(AffineFloat(9.0), AffineFloat(4.51), AffineFloat(4.89))  //-1, 1
    triangleTextbook(AffineFloat(9.0), AffineFloat(4.600001), AffineFloat(4.59999999))  //0, 0    
  }

  
  /**
   * This is not in the paper, but fun. Just try...
   */
  def rumpsFunction = {
    import AffineFloat._
    
    val x = AffineFloat(77617.0)
    val y = AffineFloat(33096.0)
    
    val f = (333.75 - x*x) * (y*y*y*y*y*y) + x*x *(11.0*x*x*y*y - 121.0*y*y*y*y - 2.0) + 5.5 * y*y*y*y*y*y*y*y + x/(2.0*y)
    println("f : " + f.interval)
  
  }
  
 
   
}

























