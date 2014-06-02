package ceres
package benchmarks

import RandomUtils._
import SmartRandomUtils._

//This is a translation of the SciMark2.0 FFT Test into Scala.
//The translation is performed as closely as possible to the original.
//We will however, use Scala Random number generator.

object FFT {
  //so that they are equal for comparison of errors 
  /*var doubleRandomVector = randomVector(2*512)
  var smartRandomVector = sconvertArray(doubleRandomVector)
  var affineRandomVector = aconvertArray(doubleRandomVector)
  var intervalRandomVector = iconvertArray(doubleRandomVector)
  */

  //@return (double difference, affinefloat abs error, intervalfloat abs error)
  def compareFFTAffineInterval(N: Int, seed: Long): (Double, Double, Double) = {
    var doubleRandomVector = randomVector(2*N, 10.0, seed)
    var affineRandomVector = aconvertArray(doubleRandomVector)
    var intervalRandomVector = iconvertArray(doubleRandomVector)
   
    val dblError = FFTDouble.test(doubleRandomVector)
    val resAffine = AffineFFT.test(affineRandomVector)
    val resInt = IntervalFFT.test(intervalRandomVector)
    (dblError, resAffine._1, resInt._1)
  }

  // @return abs error
  def affineFFTFresh(N: Int = 512) = {
    val x = arandomVector(2*N)
    AffineFFT.transform(x) // forward transform
    AffineFFT.inverse(x)
  }

  def intervalFFTFresh(N: Int = 512) = {
    val x = irandomVector(2*N)
    IntervalFFT.transform(x) // forward transform
    IntervalFFT.inverse(x)
  } 

  def doubleFFTFresh(N: Int = 512) = {
    val x = randomVector(2*N)
    FFTDouble.transform(x) // forward transform
    FFTDouble.inverse(x)
  } 
}


object FFTDouble {

  /** Compute Fast Fourier Transform of (complex) data, in place.*/
  def transform (data: Array[Double]) = {
    transform_internal(data, -1)
  }


  /** Compute Inverse Fast Fourier Transform of (complex) data, in place.*/
  def inverse (data: Array[Double]) {
    transform_internal(data, +1)  
    // Normalize
    val nd = data.length
    val n = nd/2
    val norm = 1.0 / n.toDouble;
    for(i <- 0 until nd)
      data(i) *= norm
  }

  def transform_internal (data: Array[Double], direction: Int): Unit = {
	  if (data.length == 0) return
	   
	  val n = data.length/2
    if (n == 1) return         // Identity operation!
    val logn = log2(n);

    /* bit reverse the input data for decimation in time algorithm */
    bitreverse(data) ;

    /* apply fft recursion */
	  /* this loop executed log2(N) times */
	  var bit = 0
	  var dual = 1
	  while(bit < logn) {
    //for (int bit = 0, dual = 1; bit < logn; bit++, dual *= 2) {
      var w_real = 1.0;
      var w_imag = 0.0;

      val theta = 2.0 * direction * math.Pi / (2.0 * dual);
      val s = math.sin(theta);
      val t = math.sin(theta / 2.0);
      val s2 = 2.0 * t * t;

      /* a = 0 */
      var b = 0
      while(b < n) {
      //for (int b = 0; b < n; b += 2 * dual) {
        val i: Int = 2*b ;
        val j: Int = 2*(b + dual);

        val wd_real = data(j) ;
        val wd_imag = data(j+1) ;
          
        data(j)   = data(i)   - wd_real;
        data(j+1) = data(i+1) - wd_imag;
        data(i)  += wd_real;
        data(i+1)+= wd_imag;

        b += 2 * dual
      }//inner loop 1
      
      /* a = 1 .. (dual-1) */
      for (a <- 1 until dual) {
        /* trignometric recurrence for w-> exp(i theta) w */
        {
          val tmp_real: Double = w_real - s * w_imag - s2 * w_real;
          val tmp_imag: Double = w_imag + s * w_real - s2 * w_imag;
          w_real = tmp_real;
          w_imag = tmp_imag;
        }

        var bb: Int = 0
        while (bb < n) {
        //for (int b = 0; b < n; b += 2 * dual) {
          val i: Int = 2*(bb + a);
          val j: Int = 2*(bb + a + dual);

          val z1_real = data(j);
          val z1_imag = data(j+1);
              
          val wd_real = w_real * z1_real - w_imag * z1_imag;
          val wd_imag = w_real * z1_imag + w_imag * z1_real;

          data(j)   = data(i)   - wd_real;
          data(j+1) = data(i+1) - wd_imag;
          data(i)  += wd_real;
          data(i+1)+= wd_imag;
          
          bb += 2 * dual
        }//inner inner loop
      }//inner loop 2

      bit += 1 
      dual *= 2
    }//outer loop
  }

  /** Accuracy check on FFT of data. Make a copy of data, Compute the FFT, then
    * the inverse and compare to the original.  Returns the rms difference.*/
  def test(data: Array[Double]): Double = {
    val len: Int = data.length;

    // Make duplicate for comparison
    val copy: Array[Double] = copyArray(data)
    
    // Transform & invert
    transform(data)
    inverse(data)
    
    // Compute RMS difference.
    //var diff = 0.0
    //for(i <- 0 until len) {
    //  val d = data(i) - copy(i)
    //  diff += d*d 
    //}
    //return math.sqrt(diff/len)
    
    var diffMax = 0.0
    var sum = 0.0
    var count = 0
    for(i <- 0 until len) {
      val d = math.abs(data(i) - copy(i))
      sum += d
      count += 1
      if(d > diffMax) diffMax = d 
    }
    //println("avrg. double difference: " + (sum/count))
    return diffMax
  }  


  /** Simple Test routine. */
  //each n in ns signifies one input to the test routine
  def runTests(ns: Array[Int]) = {
    for(i <- 0 until ns.length) {
      val n = ns(i)
      println("n="+n+" => RMS Error="+test(randomVector(2*n)))
    }
  }


  def log2 (n: Int): Int = {
    var log = 0;
    var k = 1

    while(k < n) {
      assert(n != (1 << log), "FFT: Data length is not a power of 2! " + n)
      log += 1
      k *= 2
    }
    log
  }

  

  //TODO: we cannot do bitreversal on our SmartFloat!
  def bitreverse(data: Array[Double]) {
    /* This is the Goldrader bit-reversal algorithm */
    val n = data.length/2;
	  val nm1 = n-1;
	  var i = 0; 
	  var j = 0;
    while(i < nm1) {

      //int ii = 2*i;
      val ii: Int = i << 1

      //int jj = 2*j;
      val jj: Int = j << 1

      //int k = n / 2;
      var k: Int = n >> 1

      if (i < j) {
        val tmp_real    = data(ii)
        val tmp_imag    = data(ii+1)
        data(ii)   = data(jj)
        data(ii+1) = data(jj+1)
        data(jj)   = tmp_real
        data(jj+1) = tmp_imag 
      }

      while (k <= j) {
        //j = j - k ;
		    j -= k

        //k = k / 2 ; 
        k >>= 1  
	    }
      j += k 

      i += 1
    }
  }


}

object AffineFFT {
  import smartfloat.AffineFloat 
  import AffineFloat._

  /** Compute Fast Fourier Transform of (complex) data, in place.*/
  def transform (data: Array[AffineFloat]) = {
    transform_internal(data, -1)
  }


  /** Compute Inverse Fast Fourier Transform of (complex) data, in place.*/
  def inverse (data: Array[AffineFloat]) {
    transform_internal(data, +1)  
    // Normalize
    val nd = data.length
    val n = nd/2
    val norm = 1.0 / AffineFloat(n)
    for(i <- 0 until nd)
      data(i) *= norm
  }

  def transform_internal (data: Array[AffineFloat], direction: Int): Unit = {
    if (data.length == 0) return
     
    val n = data.length/2
    if (n == 1) return         // Identity operation!
    val logn = log2(n);

    /* bit reverse the input data for decimation in time algorithm */
    bitreverse(data) ;

    /* apply fft recursion */
    /* this loop executed log2(N) times */
    var bit = 0
    var dual = 1
    while(bit < logn) {
    //for (int bit = 0, dual = 1; bit < logn; bit++, dual *= 2) {
      var w_real = AffineFloat(1.0)
      var w_imag = AffineFloat(0.0)

      val theta = 2.0 * direction * Pi / (2.0 * dual);
      val s = sin(theta);
      val t = sin(theta / 2.0);
      val s2 = 2.0 * t * t;

      /* a = 0 */
      var b = 0
      while(b < n) {
      //for (int b = 0; b < n; b += 2 * dual) {
        val i: Int = 2*b ;
        val j: Int = 2*(b + dual);

        val wd_real = data(j) ;
        val wd_imag = data(j+1) ;
          
        data(j)   = data(i)   - wd_real;
        data(j+1) = data(i+1) - wd_imag;
        data(i)  += wd_real;
        data(i+1)+= wd_imag;

        b += 2 * dual
      }//inner loop 1
      
      /* a = 1 .. (dual-1) */
      for (a <- 1 until dual) {
        /* trignometric recurrence for w-> exp(i theta) w */
        {
          val tmp_real: AffineFloat = w_real - s * w_imag - s2 * w_real;
          val tmp_imag: AffineFloat = w_imag + s * w_real - s2 * w_imag;
          w_real = tmp_real;
          w_imag = tmp_imag;
        }

        var bb: Int = 0
        while (bb < n) {
        //for (int b = 0; b < n; b += 2 * dual) {
          val i: Int = 2*(bb + a);
          val j: Int = 2*(bb + a + dual);

          val z1_real = data(j);
          val z1_imag = data(j+1);
              
          val wd_real = w_real * z1_real - w_imag * z1_imag;
          val wd_imag = w_real * z1_imag + w_imag * z1_real;

          data(j)   = data(i)   - wd_real;
          data(j+1) = data(i+1) - wd_imag;
          data(i)  += wd_real;
          data(i+1)+= wd_imag;
          
          bb += 2 * dual
        }//inner inner loop
      }//inner loop 2

      bit += 1 
      dual *= 2
    }//outer loop
  }

  /** Accuracy check on FFT of data. Make a copy of data, Compute the FFT, then
    * the inverse and compare to the original.  Returns the rms difference.*/
  def test(data: Array[AffineFloat]): (Double, Double) = {
    val len: Int = data.length;

    // Make duplicate for comparison
    val copy: Array[AffineFloat] = acopyArray(data)
    
    // Transform & invert
    transform(data)
    inverse(data)
    
    
    //println("average abs.error: " + SmartRandomUtils.computeAvrgAbsError(data))
    //println("average rel.error: " + SmartRandomUtils.computeAvrgError(data))
    return (SmartRandomUtils.computeMaxAbsError(data), SmartRandomUtils.computeMaxError(data))
    
    // Compute RMS difference.
    //var diff = AffineFloat(0.0)
    //for(i <- 0 until len) {
    //  val d = data(i) - copy(i)
    //  diff = diff + d*d 
   // }
    //return sqrt(diff/len) 
  }  


  /** Simple Test routine. */
  //each n in ns signifies one input to the test routine
  def runTests(ns: Array[Int]) = {
    for(i <- 0 until ns.length) {
      val n = ns(i)
      println("n="+n+" => RMS Error="+test(arandomVector(2*n)))
    }
  }


  def log2 (n: Int): Int = {
    var log = 0;
    var k = 1

    while(k < n) {
      assert(n != (1 << log), "FFT: Data length is not a power of 2! " + n)
      log += 1
      k *= 2
    }
    log
  }

  
  def bitreverse(data: Array[AffineFloat]) {
    /* This is the Goldrader bit-reversal algorithm */
    val n = data.length/2;
    val nm1 = n-1;
    var i = 0; 
    var j = 0;
    while(i < nm1) {

      //int ii = 2*i;
      val ii: Int = i << 1

      //int jj = 2*j;
      val jj: Int = j << 1

      //int k = n / 2;
      var k: Int = n >> 1

      if (i < j) {
        val tmp_real    = data(ii)
        val tmp_imag    = data(ii+1)
        data(ii)   = data(jj)
        data(ii+1) = data(jj+1)
        data(jj)   = tmp_real
        data(jj+1) = tmp_imag 
      }

      while (k <= j) {
        //j = j - k ;
        j -= k

        //k = k / 2 ; 
        k >>= 1  
      }
      j += k 

      i += 1
    }
  }


}

//###############################################################
object SmartFFT {
  import smartfloat.SmartFloat 
  import SmartFloat._

  /** Compute Fast Fourier Transform of (complex) data, in place.*/
  def transform (data: Array[SmartFloat]) = {
    transform_internal(data, -1)
  }


  /** Compute Inverse Fast Fourier Transform of (complex) data, in place.*/
  def inverse (data: Array[SmartFloat]) {
    transform_internal(data, +1)  
    // Normalize
    val nd = data.length
    val n = nd/2
    val norm = 1.0 / SmartFloat(n)
    for(i <- 0 until nd)
      data(i) *= norm
  }

  def transform_internal (data: Array[SmartFloat], direction: Int): Unit = {
    if (data.length == 0) return
     
    val n = data.length/2
    if (n == 1) return         // Identity operation!
    val logn = log2(n);

    /* bit reverse the input data for decimation in time algorithm */
    bitreverse(data) ;

    /* apply fft recursion */
    /* this loop executed log2(N) times */
    var bit = 0
    var dual = 1
    while(bit < logn) {
    //for (int bit = 0, dual = 1; bit < logn; bit++, dual *= 2) {
      var w_real = SmartFloat(1.0)
      var w_imag = SmartFloat(0.0)

      val theta = 2.0 * direction * Pi / (2.0 * dual);
      val s = sin(theta);
      val t = sin(theta / 2.0);
      val s2 = 2.0 * t * t;

      /* a = 0 */
      var b = 0
      while(b < n) {
      //for (int b = 0; b < n; b += 2 * dual) {
        val i: Int = 2*b ;
        val j: Int = 2*(b + dual);

        val wd_real = data(j) ;
        val wd_imag = data(j+1) ;
          
        data(j)   = data(i)   - wd_real;
        data(j+1) = data(i+1) - wd_imag;
        data(i)  += wd_real;
        data(i+1)+= wd_imag;

        b += 2 * dual
      }//inner loop 1
      
      /* a = 1 .. (dual-1) */
      for (a <- 1 until dual) {
        /* trignometric recurrence for w-> exp(i theta) w */
        {
          val tmp_real: SmartFloat = w_real - s * w_imag - s2 * w_real;
          val tmp_imag: SmartFloat = w_imag + s * w_real - s2 * w_imag;
          w_real = tmp_real;
          w_imag = tmp_imag;
        }

        var bb: Int = 0
        while (bb < n) {
        //for (int b = 0; b < n; b += 2 * dual) {
          val i: Int = 2*(bb + a);
          val j: Int = 2*(bb + a + dual);

          val z1_real = data(j);
          val z1_imag = data(j+1);
              
          val wd_real = w_real * z1_real - w_imag * z1_imag;
          val wd_imag = w_real * z1_imag + w_imag * z1_real;

          data(j)   = data(i)   - wd_real;
          data(j+1) = data(i+1) - wd_imag;
          data(i)  += wd_real;
          data(i+1)+= wd_imag;
          
          bb += 2 * dual
        }//inner inner loop
      }//inner loop 2

      bit += 1 
      dual *= 2
    }//outer loop
  }

  /** Accuracy check on FFT of data. Make a copy of data, Compute the FFT, then
    * the inverse and compare to the original.  Returns the rms difference.*/
  def test(data: Array[SmartFloat]): SmartFloat = {
    val len: Int = data.length;

    // Make duplicate for comparison
    val copy: Array[SmartFloat] = scopyArray(data)
    
    // Transform & invert
    transform(data)
    inverse(data)
    
    //println("SmartFloat max deviation: " + SmartRandomUtils.computeMaxAbsError(data))
    return SmartRandomUtils.computeMaxError(data)
    // Compute RMS difference.
    //var diff = SmartFloat(0.0)
    //for(i <- 0 until len) {
    //  val d = data(i) - copy(i)
    //  diff = diff + d*d 
    //}
    //return sqrt(diff/len) 
  }  


  /** Simple Test routine. */
  //each n in ns signifies one input to the test routine
  def runTests(ns: Array[Int]) = {
    for(i <- 0 until ns.length) {
      val n = ns(i)
      println("n="+n+" => RMS Error="+test(srandomVector(2*n)))
    }
  }


  def log2 (n: Int): Int = {
    var log = 0;
    var k = 1

    while(k < n) {
      assert(n != (1 << log), "FFT: Data length is not a power of 2! " + n)
      log += 1
      k *= 2
    }
    log
  }

  
  def bitreverse(data: Array[SmartFloat]) {
    /* This is the Goldrader bit-reversal algorithm */
    val n = data.length/2;
    val nm1 = n-1;
    var i = 0; 
    var j = 0;
    while(i < nm1) {

      //int ii = 2*i;
      val ii: Int = i << 1

      //int jj = 2*j;
      val jj: Int = j << 1

      //int k = n / 2;
      var k: Int = n >> 1

      if (i < j) {
        val tmp_real    = data(ii)
        val tmp_imag    = data(ii+1)
        data(ii)   = data(jj)
        data(ii+1) = data(jj+1)
        data(jj)   = tmp_real
        data(jj+1) = tmp_imag 
      }

      while (k <= j) {
        //j = j - k ;
        j -= k

        //k = k / 2 ; 
        k >>= 1  
      }
      j += k 

      i += 1
    }
  }


}


//###############################################################
object IntervalFFT {
  import smartfloat.IntervalFloat 
  import IntervalFloat._

  /** Compute Fast Fourier Transform of (complex) data, in place.*/
  def transform (data: Array[IntervalFloat]) = {
    transform_internal(data, -1)
  }


  /** Compute Inverse Fast Fourier Transform of (complex) data, in place.*/
  def inverse (data: Array[IntervalFloat]) {
    transform_internal(data, +1)  
    // Normalize
    val nd = data.length
    val n = nd/2
    val norm = 1.0 / IntervalFloat(n)
    for(i <- 0 until nd)
      data(i) *= norm
  }

  def transform_internal (data: Array[IntervalFloat], direction: Int): Unit = {
    if (data.length == 0) return
     
    val n = data.length/2
    if (n == 1) return         // Identity operation!
    val logn = log2(n);

    /* bit reverse the input data for decimation in time algorithm */
    bitreverse(data) ;

    /* apply fft recursion */
    /* this loop executed log2(N) times */
    var bit = 0
    var dual = 1
    while(bit < logn) {
    //for (int bit = 0, dual = 1; bit < logn; bit++, dual *= 2) {
      var w_real = IntervalFloat(1.0)
      var w_imag = IntervalFloat(0.0)

      val theta = 2.0 * direction * Pi / (2.0 * dual);
      val s = sin(theta);
      val t = sin(theta / 2.0);
      val s2 = 2.0 * t * t;

      /* a = 0 */
      var b = 0
      while(b < n) {
      //for (int b = 0; b < n; b += 2 * dual) {
        val i: Int = 2*b ;
        val j: Int = 2*(b + dual);

        val wd_real = data(j) ;
        val wd_imag = data(j+1) ;
          
        data(j)   = data(i)   - wd_real;
        data(j+1) = data(i+1) - wd_imag;
        data(i)  += wd_real;
        data(i+1)+= wd_imag;

        b += 2 * dual
      }//inner loop 1
      
      /* a = 1 .. (dual-1) */
      for (a <- 1 until dual) {
        /* trignometric recurrence for w-> exp(i theta) w */
        {
          val tmp_real: IntervalFloat = w_real - s * w_imag - s2 * w_real;
          val tmp_imag: IntervalFloat = w_imag + s * w_real - s2 * w_imag;
          w_real = tmp_real;
          w_imag = tmp_imag;
        }

        var bb: Int = 0
        while (bb < n) {
        //for (int b = 0; b < n; b += 2 * dual) {
          val i: Int = 2*(bb + a);
          val j: Int = 2*(bb + a + dual);

          val z1_real = data(j);
          val z1_imag = data(j+1);
              
          val wd_real = w_real * z1_real - w_imag * z1_imag;
          val wd_imag = w_real * z1_imag + w_imag * z1_real;

          data(j)   = data(i)   - wd_real;
          data(j+1) = data(i+1) - wd_imag;
          data(i)  += wd_real;
          data(i+1)+= wd_imag;
          
          bb += 2 * dual
        }//inner inner loop
      }//inner loop 2

      bit += 1 
      dual *= 2
    }//outer loop
  }

  /** Accuracy check on FFT of data. Make a copy of data, Compute the FFT, then
    * the inverse and compare to the original.  Returns the rms difference.*/
  def test(data: Array[IntervalFloat]): (Double, Double) = {
    val len: Int = data.length;

    // Make duplicate for comparison
    val copy: Array[IntervalFloat] = icopyArray(data)
    
    // Transform & invert
    transform(data)
    inverse(data)
    
    
    //println("average abs.error: " + SmartRandomUtils.computeAvrgAbsError(data))
    //println("average rel.error: " + SmartRandomUtils.computeAvrgError(data))
    return (SmartRandomUtils.computeMaxAbsError(data), SmartRandomUtils.computeMaxError(data))
    
    // Compute RMS difference.
    //var diff = AffineFloat(0.0)
    //for(i <- 0 until len) {
    //  val d = data(i) - copy(i)
    //  diff = diff + d*d 
   // }
    //return sqrt(diff/len) 
  }  


  /** Simple Test routine. */
  //each n in ns signifies one input to the test routine
  def runTests(ns: Array[Int]) = {
    for(i <- 0 until ns.length) {
      val n = ns(i)
      println("n="+n+" => RMS Error="+test(irandomVector(2*n)))
    }
  }


  def log2 (n: Int): Int = {
    var log = 0;
    var k = 1

    while(k < n) {
      assert(n != (1 << log), "FFT: Data length is not a power of 2! " + n)
      log += 1
      k *= 2
    }
    log
  }

  
  def bitreverse(data: Array[IntervalFloat]) {
    /* This is the Goldrader bit-reversal algorithm */
    val n = data.length/2;
    val nm1 = n-1;
    var i = 0; 
    var j = 0;
    while(i < nm1) {

      //int ii = 2*i;
      val ii: Int = i << 1

      //int jj = 2*j;
      val jj: Int = j << 1

      //int k = n / 2;
      var k: Int = n >> 1

      if (i < j) {
        val tmp_real    = data(ii)
        val tmp_imag    = data(ii+1)
        data(ii)   = data(jj)
        data(ii+1) = data(jj+1)
        data(jj)   = tmp_real
        data(jj+1) = tmp_imag 
      }

      while (k <= j) {
        //j = j - k ;
        j -= k

        //k = k / 2 ; 
        k >>= 1  
      }
      j += k 

      i += 1
    }
  }


}

