package ceres
package benchmarks

import org.scalameter.api._


object PerformanceBenchmark extends PerformanceTest {

  /* configuration */
  lazy val executor = SeparateJvmsExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default
  )
  
  lazy val reporter = new LoggingReporter
  lazy val persistor = Persistor.None

  /* inputs */
  //Gen.range(axis: String)(from: Int, upto: Int, hop: Int) 

  
  /* tests */

  val integrationSteps = Gen.range("size")(1, 1000, 100)

  performance of "NBody" config(
    exec.jvmflags -> "-Xmx2048m -Xms2048m -XX:CompileThreshold=100 -Djava.library.path=lib/"
    ) in {
    measure method "affine" in {
      using(integrationSteps) in {
        steps => {
          NBody.affineSimulate(steps, 0.01)
        }
      }
    }

    measure method "interval" in {
      using(integrationSteps) in {
        steps => {
          NBody.intervalSimulate(steps, 0.01)
        }
      }
    }
  }

  val iterationSpectral = Gen.range("size")(5, 20, 5)

  performance of "Spectral" config(
    exec.jvmflags -> "-Xmx2048m -Xms2048m -XX:CompileThreshold=100 -Djava.library.path=lib/"
    ) in {
    measure method "affine" in {
      using(iterationSpectral) in {
        iter => {
          SpectralNorm.approximateAffine(iter)
        }
      }
    }

    measure method "interval" in {
      using(iterationSpectral) in {
        iter => {
          SpectralNorm.approximateInterval(iter)
        }
      }
    }
  }


  val iterationsSOR = Gen.range("size")(5, 20, 5)
  
  performance of "SOR" config(
    exec.jvmflags -> "-Xmx2048m -Xms2048m -XX:CompileThreshold=100 -Djava.library.path=lib/"
    ) in {
    measure method "affine" in {
      using(iterationsSOR) in {
        iter => {
          SOR.affineSOR(iter)
        }
      }
    }

    measure method "interval" in {
      using(iterationsSOR) in {
        iter => {
          SOR.intervalSOR(iter)
        }
      }
    }
  }

  val dimLU = Gen.range("size")(5, 15, 5)

  performance of "LU-pivoting" config(
    exec.jvmflags -> "-Xmx2048m -Xms2048m -XX:CompileThreshold=100 -Djava.library.path=lib/"
    ) in {
    measure method "affine" in {
      using(dimLU) in {
        dim => {
          DenseLU.affineLU(dim)
        }
      }
    }

    measure method "interval" in {
      using(dimLU) in {
        dim => {
          DenseLU.intervalLU(dim)
        }
      }
    }
  }

  // probably needs to be some power of 2 or some such
  val dimFFT = Gen.range("size")(256, 512, 256)

  performance of "FFT" config(
    exec.jvmflags -> "-Xmx2048m -Xms2048m -XX:CompileThreshold=100 -Djava.library.path=lib/"
    ) in {
    measure method "affine" in {
      using(dimFFT) in {
        dim => {
          FFT.affineFFTFresh(dim)
        }
      }
    }

    measure method "interval" in {
      using(dimFFT) in {
        dim => {
          FFT.affineFFTFresh(dim)
        }
      }
    }
  }

  
}