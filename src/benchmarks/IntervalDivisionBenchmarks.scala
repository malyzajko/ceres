package ceres
package benchmarks


import smartfloat._

object IntervalDivisionBenchmarks extends App {


  def dopplerDivision = {
    

  }


  def doppler(u: SmartFloat, v: SmartFloat, T: SmartFloat):
    (SmartFloat, SmartFloat, SmartFloat, SmartFloat, SmartFloat) = {

    val q1 = SmartFloat(331.4) + T*0.6
    val q2 = q1 * v
    val q3 = q1 + u
    val q4 = q3 * q3
    val z = q2/q4
    
    (q1, q2, q3, q4, z)
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


  //true ranges are:= for u \in [0, 1]
  def bSplines(u: SmartFloat) = {
    
    //val u = SmartFloat(0.75, 0.25)
    //val u = SmartFloat(0.25, 0.25)
    
    val oneMinusU = SmartFloat(1.0) - u
    
    val b0 = (oneMinusU * oneMinusU * oneMinusU)/6.0
  
    val b1 = (u*u*u*3 - u*u*6 + 4.0)/6.0
  
    val b2 = (u*u*u*(-3.0) + u*u*3 + u*3 + 1)/6.0
    
    val b3 = (-u*u*u)/6.0
    /*val b0 = u
    val b1 = u*u
    val b2 = u*u*u
    
    
    */
    println("b0: " + b0.toStringWithAbsErrors)
    println("b1: " + b1.toStringWithAbsErrors)
    println("b2: " + b2.toStringWithAbsErrors)
    println("b3: " + b3.toStringWithAbsErrors)
  }
}