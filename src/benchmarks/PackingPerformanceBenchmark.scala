package ceres
package benchmarks

import org.scalameter.api._
import RandomUtils._
import SmartRandomUtils._


// Benchmarks for investigating effects of packing
object PackingPerformanceBenchmark extends PerformanceTest {

  /* configuration */
  lazy val executor = SeparateJvmsExecutor(
    new Executor.Warmer.Default,
    Aggregator.average,
    new Measurer.Default
  )
  
  lazy val reporter = new LoggingReporter
  lazy val persistor = Persistor.None
  
  
  /* tests */

  val integrationSteps = Gen.range("size")(1000, 1000, 100)

  performance of "NBody" config(
      exec.jvmflags -> "-Xmx2048m -Xms2048m -XX:CompileThreshold=100 -Djava.library.path=lib/"
  ) in {
  
    measure method "affine" config(
      exec.minWarmupRuns -> 10,
      exec.maxWarmupRuns -> 30,
      exec.independentSamples -> 2
    ) in {
      using(integrationSteps) in {
        steps => {
          NBody.affineSimulate(steps, 0.01)
        }
      }
    }
  }

  val iterationSpectral = Gen.range("size")(20, 20, 5)

  performance of "Spectral" config(
    exec.jvmflags -> "-Xmx2048m -Xms2048m -XX:CompileThreshold=100 -Djava.library.path=lib/"
  ) in {

    measure method "affine" config(
      exec.minWarmupRuns -> 10,
      exec.maxWarmupRuns -> 30,
      exec.independentSamples -> 2
    ) in {
      using(iterationSpectral) in {
        iter => {
          SpectralNorm.approximateAffine(iter)
        }
      }
    }
    
  }


  val iterationsSOR = Gen.range("size")(20, 20, 5)
  
 performance of "SOR" config(
    exec.jvmflags -> "-Xmx2048m -Xms2048m -XX:CompileThreshold=100 -Djava.library.path=lib/"

  ) in {

    measure method "affine" config(
      exec.minWarmupRuns -> 5,
      exec.maxWarmupRuns -> 20,
      exec.independentSamples -> 2
    ) in {
      using(iterationsSOR) in {
        iter => {
          SOR.affineSOR(iter)
        }
      }
    } 
  }

  val dimLU = Gen.range("size")(15, 15, 5)

  performance of "LU-pivoting" config(
    exec.jvmflags -> "-Xmx2048m -Xms2048m -XX:CompileThreshold=100 -Djava.library.path=lib/"
  ) in {
    
    measure method "affine" config(
      exec.minWarmupRuns -> 30,
      exec.maxWarmupRuns -> 50,
      exec.independentSamples -> 2
    ) in {
      using(dimLU) in {
        dim => {
          DenseLU.affineLU(dim)
        }
      }
    }
  }
  

  // probably needs to be some power of 2 or some such
  val dimFFT = Gen.range("size")(512, 512, 256)

  performance of "FFT" config(
    exec.jvmflags -> "-Xmx2048m -Xms2048m -XX:CompileThreshold=100 -Djava.library.path=lib/"
  ) in {
    
    measure method "affine" config(
      exec.minWarmupRuns -> 10,
      exec.maxWarmupRuns -> 30,
      exec.independentSamples -> 2
    ) in {
      using(dimFFT) in {
        dim => {
          FFT.affineFFTFresh(dim)
        }
      }
    }
  }
  
  
}