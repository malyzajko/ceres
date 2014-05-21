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

  val iterationsSOR = Gen.range("size")(1, 1, 1)
  
  /* tests */

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


  val integrationSteps = Gen.range("size")(1, 1, 1)

  performance of "NBody" config(
    exec.jvmflags -> "-Xmx2048m -Xms2048m -XX:CompileThreshold=100 -Djava.library.path=lib/"
    ) in {
    measure method "affine" in {
      using(integrationSteps) in {
        steps => {
          NBody.affineSimulate(steps, 0.01)()
        }
      }
    }

    measure method "interval" in {
      using(integrationSteps) in {
        steps => {
          NBody.intervalSimulate(steps, 0.01)()
        }
      }
    }
  }

  val iterationSpectral = Gen.range("size")(1, 1, 1)

  performance of "Spectral" config(
    exec.jvmflags -> "-Xmx2048m -Xms2048m -XX:CompileThreshold=100 -Djava.library.path=lib/"
    ) in {
    measure method "affine" in {
      using(integrationSteps) in {
        steps => {
          NBody.affineSimulate(steps, 0.01)()
        }
      }
    }

    measure method "interval" in {
      using(integrationSteps) in {
        steps => {
          NBody.intervalSimulate(steps, 0.01)()
        }
      }
    }
  }

  // probably needs to be some power of 2 or some such
  val dimFFT = Gen.range("size")(512, 512, 1)

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

  val dimLU = Gen.range("size")(15, 15, 1)

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
}