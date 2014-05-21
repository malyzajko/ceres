package ceres
package benchmarks 

import smartfloat.{AffineFloat, SmartFloat, AffineForm}

/**
 * A (hopefully comprehensive) collection of benchmarks for our SmartFloat.
 */
object PaperBenchmarks extends App {

  //run

  /**
   * Example way to run our benchmarks.
   * There are also versions of these benchmarks in Double and SmartFloat.
   */
  def runAffineFloatBenchmarks = {
    //computes the max rel. error
    println("\n~~~ SOR: max rel. error (double) ~~~")
    println(" 5 iter: " + SOR.affineSOR(5) + "  |  " + SOR.intervalSOR(5))
    println("10 iter: " + SOR.affineSOR(10) + "  |  " + SOR.intervalSOR(10))
    println("15 iter: " + SOR.affineSOR(15) + "  |  " + SOR.intervalSOR(15))
    println("20 iter: " + SOR.affineSOR(20) + "  |  " + SOR.intervalSOR(20))
    

    println("~~~ Nbody simulation: energy after 100 iterations: ~~~")
    println(NBody.affineSimulate(100, 0.01)().toStringWithErrors)
    println(NBody.affineSimulate(64, 0.015625)().toStringWithErrors)
    println(NBody.affineSimulate(1000, 0.01)().toStringWithErrors)
    println(NBody.affineSimulate(640, 0.015625)().toStringWithErrors)

    println(NBody.intervalSimulate(100, 0.01)().toStringWithErrors)
    println(NBody.intervalSimulate(64, 0.015625)().toStringWithErrors)
    println(NBody.intervalSimulate(1000, 0.01)().toStringWithErrors)
    println(NBody.intervalSimulate(640, 0.015625)().toStringWithErrors)


    println("\n~~~ Spectral norm ~~~")
    println("2 ite:r " + SpectralNorm.approximateAffine(2).toStringWithErrors)
    println("5 ite:r " + SpectralNorm.approximateAffine(5).toStringWithErrors)
    println("10 ite:r " + SpectralNorm.approximateAffine(10).toStringWithErrors)
    println("15 ite:r " + SpectralNorm.approximateAffine(15).toStringWithErrors)
    println("20 ite:r " + SpectralNorm.approximateAffine(20).toStringWithErrors)
    
    //computes root squared difference of fft and inverse
    {
      println("\n~~~ Fast Fourier (dim 512) ~~~")
      val (dblAbsDiff, maxAbsAffine, maxAbsInt) = FFT.compareFFTAffineInterval(512)
      println("max absolute difference (double): " + dblAbsDiff)
      println("max abs error (affinefloat): " + maxAbsAffine)
      println("max abs error (intervalfloat): " + maxAbsInt)
    }

    {
      println("\n~~~ Fast Fourier (dim 256) ~~~")
      val (dblAbsDiff, maxAbsAffine, maxAbsInt) = FFT.compareFFTAffineInterval(256)
      println("max absolute difference (double): " + dblAbsDiff)
      println("max abs error (affinefloat): " + maxAbsAffine)
      println("max abs error (intervalfloat): " + maxAbsInt)
    }

    //computes abs norm diff
    {
      println("\n~~~ LU factorisation: abs norm diff ~~~")
      val (verif, maxAbsAffine, maxAbsInt) = DenseLU.compareLUAffineInterval(5)
      println("double max abs difference computed: " +verif)
      println("max abs error (affinefloat): " + maxAbsAffine)
      println("max abs error (intervalfloat): " + maxAbsInt)
    }  

    {
      println("\n~~~ LU factorisation: abs norm diff ~~~")
      val (verif, maxAbsAffine, maxAbsInt) = DenseLU.compareLUAffineInterval(10)
      println("double max abs difference computed: " +verif)
      println("max abs error (affinefloat): " + maxAbsAffine)
      println("max abs error (intervalfloat): " + maxAbsInt)
    }  

    {
      println("\n~~~ LU factorisation: abs norm diff ~~~")
      val (verif, maxAbsAffine, maxAbsInt) = DenseLU.compareLUAffineInterval(15)
      println("double max abs difference computed: " +verif)
      println("max abs error (affinefloat): " + maxAbsAffine)
      println("max abs error (intervalfloat): " + maxAbsInt)
    }      

    // TODO: LU without pivoting

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
  
  } 
 
  
  /* ###################################
  #####################################*/
  
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
