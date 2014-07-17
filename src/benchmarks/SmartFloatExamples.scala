package ceres
package benchmarks


import smartfloat._
import scala.collection.mutable.LinkedList

import SmartFloat._

/**
 * These are examples from the Technical Report.
 */
object SmartFloatExamples extends App {

  triangleExampleSection

  springSimulation(0.1, 1.0)
  springSimulation(0.125, 1.0)
  springSimulation(0.01, 1.0)

  //quadraticEquation
  println()
  println(quadraticEquationSmart)

  
  //require(-100.0 <= u && u <= 100 && 20 <= v && v <= 20000 && -30 <= T && T <= 50)
  println()
  println("doppler: " + doppler(0 +/- 100, 10010 +/- 9990, 10 +/- 40))
  //cubeRoot
  
  
  println()
  bSplines(0.5 +/- 0.5)

  println()
  turbine
  
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
    println(triangleTextbook(9.0, 4.8 +/- 0.09, 4.8 +/- 0.09))
    println(triangleTextbook(9.0, 4.7 +/- 0.09, 4.7 +/- 0.09))
    println(triangleTextbook(9.0, 4.541 +/- 0.04, 4.541 +/- 0.04))
    
    println("\nSmartFloat: Kahan")
    println(triangleKahan(9.0, 4.8 +/- 0.09, 4.8 +/- 0.09))
    println(triangleKahan(9.0, 4.7 +/- 0.09, 4.7 +/- 0.09))
    println(triangleKahan(9.0, 4.541 +/- 0.04, 4.541 +/- 0.04))

    //println("\nAffineFloat for these values: a = 9.0, b = 4.53, c=4.53")
    //println("according to Mathematica  A=2.34216246234115877566387422739")
    //println(triangleKahan(AffineFloat(9.0), AffineFloat(4.61), AffineFloat(4.61)))
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

  def quadraticEquationSmart: (SmartFloat, SmartFloat, SmartFloat, SmartFloat) = {
    import SmartFloat._
        
    // var a = SmartFloat(2.999)
    // var b = SmartFloat(56.0001)
    // var c = SmartFloat(1.00074)

    var a = SmartFloat(2.999, 0.5)
    var b = SmartFloat(56.0001, 0.5)
    var c = SmartFloat(1.00074, 0.5)
    val discr = b*b - a * c * 4.0
      
    //classical way
    var r2 = (-b + sqrt(discr))/(a * 2.0)
    var r1 = (-b - sqrt(discr))/(a * 2.0)
    println("classic r1 = " + r1.toStringWithErrors + 
                 " , r2 = " + r2.toStringWithErrors)
    
    //smarter way
    val (rk1: SmartFloat, rk2: SmartFloat) = 
    if(b*b - a*c > 10.0) {  
      if(b > 0.0) ((-b - sqrt(discr))/(a * 2.0), c * 2.0 /(-b - sqrt(discr)))
      else if(b < 0.0)  (c * 2.0 /(-b + sqrt(discr)), (-b + sqrt(discr))/(a * 2.0))
      else  ((-b - sqrt(discr))/(a * 2.0), (-b + sqrt(discr))/(a * 2.0))
    }
    else {
      ((-b - sqrt(discr))/(a * 2.0), (-b + sqrt(discr))/(a * 2.0))
    }
  
    println("smarter r1 = " + rk1.toStringWithErrors + " , r2 = " + rk2.toStringWithErrors)
    (r1, r2, rk1, rk2)
  }
  
  
  def doppler(u: SmartFloat, v: SmartFloat, T: SmartFloat): SmartFloat = {

    val t1 = 331.4 + 0.6 * T
    (- (t1) *v) / ((t1 + u)*(t1 + u))

  }
  
   def bSplines(u: SmartFloat) = {
    
    val one = SmartFloat(1.0)
    
    val b0 = ((one - u) * (one - u) * (one  - u))/6.0
  
    val b1 = (u*u*u*3 - u*u*6 + 4.0)/6.0
  
    val b2 = (u*u*u*(-3.0) + u*u*3 + u*3 + 1)/6.0
    
    val b3 = (-u*u*u)/6.0

    println("b0: " + b0)
    println("b1: " + b1)
    println("b2: " + b2)
    println("b3: " + b3)  
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

  
  
  

  def turbine = {
    val v = -2.4 +/- 2.1
    val w = 0.65 +/- 0.25
    val r = 5.8 +/- 2.0

    val t1 = 3 + 2/(r*r) - 0.125*(3-2*v)*(w*w*r*r)/(1-v) - 4.5
    val t2 = 6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
    val t3 = 3 - 2/(r*r) - 0.125 * (1+2*v) * (w*w*r*r) / (1-v) - 0.5
  
    println("turbine1: " + t1)
    println("turbine2: " + t2)
    println("turbine3: " + t3)
  }

  /*def triangleExampleSection = {
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
  }*/
   
}

























