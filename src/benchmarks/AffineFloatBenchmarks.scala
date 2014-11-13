package ceres
package benchmarks 

import smartfloat.{AffineFloat, SmartFloat, IntervalFloat}

/**
 * A (hopefully comprehensive) collection of benchmarks for our SmartFloat.
 */
object AffineFloatBenchmarks extends App {

  args(0) match {
    case "nbody" => runNBody
    case "spectral" => runSpectralNorm
    case "sor" => runSOR
    case "lu" => runDenseLU
    case "fft" => runFFT
    case "packing" => runPacking
    case "rump" => rumpsFunction
  }

  /**
   * This is not in the paper, but fun.
   */
  def rumpsFunction = {
    import AffineFloat._
    
    {
      val x = AffineFloat(77617.0)
      val y = AffineFloat(33096.0)
    
      val f = (333.75 - x*x) * (y*y*y*y*y*y) + x*x *(11.0*x*x*y*y - 121.0*y*y*y*y - 2.0) + 5.5 * y*y*y*y*y*y*y*y + x/(2.0*y)
      println("rumps function: \nf : " + f.toStringWithAbsErrors)
    }
    import ceres.{QuadDouble => QD}
    {
      val x = QD(77617.0)
      val y = QD(33096.0)
    
      val f = (333.75 - x*x) * (y*y*y*y*y*y) + x*x *(11.0*x*x*y*y - 121.0*y*y*y*y - 2.0) + 5.5 * y*y*y*y*y*y*y*y + x/(2.0*y)
      println("rumps function: \nf : " + f)
    }
  }

  def runPacking = {
    println("NBody: " + printCompleteAffineFloat( NBody.affineSimulate(1000, 0.01) ))

    println("Spectral: " + printCompleteAffineFloat( SpectralNorm.approximateAffine(20) ))

    println("SOR: " + SOR.compareAffineInterval(20, 4357)._2)

    println("LU: " + DenseLU.compareLUAffineInterval(15, 4357)._2)

    println("FFT: " + FFT.compareFFTAffineInterval(512, 4357)._2)
  }

  def runNBody = {
    import NBody._

    println("Running Nbody simulation, measuring energy\n")
    println("AffineFloat, 1s")
    println("dt=0.01     " + printCompleteAffineFloat( affineSimulate(100, 0.01) ))
    println("dt=0.015625 " + printCompleteAffineFloat( affineSimulate(64, 0.015625) ))

    println("AffineFloat, 5s")
    println("dt=0.01     " + printCompleteAffineFloat( affineSimulate(500, 0.01) ))
    println("dt=0.015625 " + printCompleteAffineFloat( affineSimulate(320, 0.015625) ))

    println("IntervalFloat, 1s")
    println("dt=0.01     " + printCompleteIntervalFloat( intervalSimulate(100, 0.01) ))
    println("dt=0.015625 " + printCompleteIntervalFloat( intervalSimulate(64, 0.015625) ))

    println("IntervalFloat, 5s")
    println("dt=0.01     " + printCompleteIntervalFloat( intervalSimulate(500, 0.01) ))
    println("dt=0.015625 " + printCompleteIntervalFloat( intervalSimulate(320, 0.015625) ))


    println("SmartFloat, 1s")
    println("dt=0.01     " + printCompleteSmartFloat( smartSimulate(100, 0.01) ))
    println("dt=0.015625 " + printCompleteSmartFloat( smartSimulate(64, 0.015625) ))

    println("SmartFloat, 5s")
    println("dt=0.01     " + printCompleteSmartFloat( smartSimulate(500, 0.01) ))
    println("dt=0.015625 " + printCompleteSmartFloat( smartSimulate(320, 0.015625) ))
    
  }

  def runSpectralNorm = {
    import SpectralNorm._

    println("Running Spectral norm")
    println("AffineFloat")
    println("2 iter:  " + printCompleteAffineFloat( approximateAffine(2) ))
    println("5 iter:  " + printCompleteAffineFloat( approximateAffine(5) ))
    println("10 ite:r " + printCompleteAffineFloat( approximateAffine(10) ))
    println("15 ite:r " + printCompleteAffineFloat( approximateAffine(15) ))
    println("20 ite:r " + printCompleteAffineFloat( approximateAffine(20) ))

    println("IntervalFloat")
    println("2 iter:  " + printCompleteIntervalFloat( approximateInterval(2) ))
    println("5 iter:  " + printCompleteIntervalFloat( approximateInterval(5) ))
    println("10 ite:r " + printCompleteIntervalFloat( approximateInterval(10) ))
    println("15 ite:r " + printCompleteIntervalFloat( approximateInterval(15) ))
    println("20 ite:r " + printCompleteIntervalFloat( approximateInterval(20) ))


    println("SmartFloat")
    println("2 iter:  " + printCompleteSmartFloat( approximateSmart(2) ))
    println("5 iter:  " + printCompleteSmartFloat( approximateSmart(5) ))
    println("10 ite:r " + printCompleteSmartFloat( approximateSmart(10) ))
    println("15 ite:r " + printCompleteSmartFloat( approximateSmart(15) ))
    println("20 ite:r " + printCompleteSmartFloat( approximateSmart(20) ))

  }


  def runSOR = {
    import SOR._
    println("Running SOR")

    var seeds = Seq(4357, 4999, 7817, 2447, 1013)

    seeds.foreach { seed =>
      println("\nseed: " + seed)
      println(" 5 iter: " + compareAffineInterval(5, seed) )
      println("10 iter: " + compareAffineInterval(10, seed))
      println("15 iter: " + compareAffineInterval(15, seed))
      println("20 iter: " + compareAffineInterval(20, seed))
    }
    //
  }

  def runDenseLU = {
    import DenseLU._

    var seeds = Seq(4357, 4999, 7817, 2447, 1013)
    //var seeds = Seq(7817)

    println("Running DenseLU with pivoting: " + DenseLU.pivoting)

    seeds.foreach { seed =>
      println("\nseed: " + seed)
      println("dim 2 : " + compareLUAffineInterval(5, seed))
      println("dim 10: " + compareLUAffineInterval(10, seed))
      println("dim 15: " + compareLUAffineInterval(15, seed))
    }
  }


  def runFFT = {
    import FFT._

    var seeds = Seq(4357, 4999, 7817, 2447, 1013)

    println(" Running FFT")

    seeds.foreach { seed =>
      println("\nseed: " + seed)
      println("dim 256: " + compareFFTAffineInterval(256, seed))
      println("dim 512: " + compareFFTAffineInterval(512, seed))
    }

  }

  private def printCompleteAffineFloat(af: AffineFloat): String = {
    af.d + " " + af.interval + " rel: " + af.relError + " abs: " + af.absError
  }

  private def printCompleteIntervalFloat(in: IntervalFloat): String = {
    in.d + " " + in.interval + " rel: " + in.relError + " abs: " + in.absError
  }

  private def printCompleteSmartFloat(in: SmartFloat): String = {
    in.d + " " + in.interval + " rel: " + in.relError + " abs: " + in.absError
  }

  //def runAffineFloatBenchmarks = {
    
    
    // TODO: spring

    // TODO: Fbench

    // TODO: Whetstone
/*    //returns the norm, 10 iterations
    
    
    //computes sum of 2x2 matrix of the marginal and paraxial ray
    println("\n~~~ Fbench ~~~")
    println(AffineFBench.runNoTiming(1)().toStringWithErrors)
    
    
    
    
    
    //returns the final positon (x)
    println("\n~~~ Spring simulation: final positon (x) ~~~")
    println("rel.error of x: " + affineSpring(0.001, 10.0).toStringWithErrors)
*/    
  
  //} 
 
  
  /**
   * Computes the cube root of 10 by Halley's method.
   */
  def cubeRoot = {
    import AffineFloat._
    println("~~~ Cube root ~~~")
    val a: AffineFloat = 10
    var xn = AffineFloat(1.6)

    for(i <- 1 until 3) {
        xn = xn * ((xn*xn*xn + 2.0*a)/(2.0*xn*xn*xn + a))
    }
    println("final:" + xn.toStringWithAbsErrors)
    println("intervals: " + xn.interval)
  }

  
  def affineSpring(h:AffineFloat, tmax: Double): AffineFloat = {
    val k: AffineFloat = 1.0
    val m: AffineFloat = 1.0
    val xmax: AffineFloat = 5.0
    var x: AffineFloat = xmax  //current horizontal position
    var vx: AffineFloat = 0.0  //current velocity
    var t: AffineFloat = 0.0  //current 'time'
    
    var methodError = k*m*xmax * (h*h)/2.0
    
    while(t < tmax) {    //global flag fails if 1.0
      val x_next = x + h * vx
      val vx_next = vx - h * k/m * x
      x = x_next
      vx = vx_next
      t = t + h
    }
    return x  
  }
  
  

}
