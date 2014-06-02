package ceres
package benchmarks

import RandomUtils._
import SmartRandomUtils._

/** Description from SciMark homepage.
Computes the LU factorization of a dense 100x100 matrix using partial pivoting. Exercises linear algebra kernels (BLAS) and dense matrix operations. The algorithm is the right-looking version of LU with rank-1 updates.
The data size for the LARGE version of the benchmark uses a 1,000 x 1,000 matrix. 
*/


object DenseLU {

  val pivoting: Boolean = true

  def doubleLU(dim: Int) = {
    val A = randomMatrix(dim, dim)
    val lu = copyMatrix(A)
    val x = randomVector(dim)
    val pivot: Array[Int] = Array.fill(dim){0}
    
    DoubleDenseLU.factor(lu, pivot)
    //println("pivot: " + pivot.toList)
    //DoubleDenseLU.solve(lu, pivot, x) 
    DoubleDenseLU.solve(lu, pivot, x)
  }
  
  def smartLU(dim: Int) = {
    val sA = srandomMatrix(dim, dim)
    val slu = scopyMatrix(sA)
    val sx = srandomVector(dim)
    val spivot: Array[Int] = Array.fill(dim){0}
    
    SmartDenseLU.factor(slu, spivot)
    SmartDenseLU.solve(slu, spivot, sx)    
  }
  
  def affineLU(dim: Int) = {
    val sA = arandomMatrix(dim, dim)
    val slu = acopyMatrix(sA)
    val sx = arandomVector(dim)
    val spivot: Array[Int] = Array.fill(dim){0}
    
    AffineDenseLU.factor(slu, spivot)
    AffineDenseLU.solve(slu, spivot, sx)
  }

  def intervalLU(dim: Int) = {
    val sA = irandomMatrix(dim, dim)
    val slu = icopyMatrix(sA)
    val sx = irandomVector(dim)
    val spivot: Array[Int] = Array.fill(dim){0}
    
    IntervalDenseLU.factor(slu, spivot)
    IntervalDenseLU.solve(slu, spivot, sx)
  }

  /*
    Compare results for computing FFT on a random matrix of dimension dim.

    @return (affinefloat rel error, affinefloat abs error, interval rel error, interval abs error)
  */
  def compareLUAffineInterval(dim: Int, seed: Long): (Double, Double, Double, Double) = {
    var doubleMatrix = randomSquareMatrix(dim, 10.0, seed)
    var affineMatrix = aconvertMatrix(doubleMatrix)
    var intervalMatrix = iconvertMatrix(doubleMatrix)
    
    val sA = affineMatrix
    val slu = acopyMatrix(sA)
    val spivot: Array[Int] = Array.fill(dim){0}
    AffineDenseLU.factor(slu, spivot)
    
    val sverif = AffineDenseLU.verify(sA, slu, spivot)
    
    val iA = intervalMatrix
    val ilu = icopyMatrix(iA)
    val ipivot: Array[Int] = Array.fill(dim){0}
    IntervalDenseLU.factor(ilu, ipivot)
    
    val iverif = IntervalDenseLU.verify(iA, ilu, ipivot)
    
    (sverif._1, sverif._2, iverif._1, iverif._1) 
  }
}

object DoubleDenseLU {
  
  import DenseLU.pivoting
  /**
    LU factorization (in place).

    @param A (in/out) On input, the matrix to be factored.
        On output, the compact LU factorization.

    @param pivit (out) The pivot vector records the
        reordering of the rows of A during factorization.
        
    @return 0, if OK, nozero value, othewise.
  */
  def factor(A: Array[Array[Double]],  pivot: Array[Int]): Int = {
    val N = A.length
    val M = A(0).length

    val minMN = math.min(M,N)

    for (j <- 0 until minMN) {
      // find pivot in column j and  test for singularity.
      
        var jp = j
        
        if (pivoting) {
          var t = math.abs(A(j)(j))
          for (i <- (j+1) until M) {
              val ab = math.abs(A(i)(j))
              if ( ab > t) {
                  jp = i
                  t = ab
              }
          }
        }
        
        pivot(j) = jp

        // jp now has the index of maximum element 
        // of column j, below the diagonal

        if ( A(jp)(j) == 0 )                 
            return 1       // factorization failed because of zero pivot

        if (jp != j) {
            // swap rows j and jp
            val tA = A(j)
            A(j) = A(jp)
            A(jp) = tA
        }
      
 
      if (j < M-1) {                // compute elements j+1:M of jth column
          // note A(j,j), was A(jp,p) previously which was
          // guarranteed not to be zero (Label #1)
          //
          val recp =  1.0 / A(j)(j)

          for (k <- (j+1) until M)
              A(k)(j) *= recp
      }


      if (j < minMN-1) {
        // rank-1 update to trailing submatrix:   E = E - x*y;
        //
        // E is the region A(j+1:M, j+1:N)
        // x is the column vector A(j+1:M,j)
        // y is row vector A(j,j+1:N)


        for (ii <- (j+1) until M) {
            val Aii = A(ii)
            val Aj = A(j)
            val AiiJ = Aii(j)
            for (jj <- (j+1) until N)
              Aii(jj) -= AiiJ * Aj(jj)
        }
      }
    }
    return 0
  }


    /**
        Solve a linear system, using a prefactored matrix
            in LU form.


        @param LU (in) the factored matrix in LU form. 
        @param pivot (in) the pivot vector which lists
            the reordering used during the factorization
            stage.
        @param b    (in/out) On input, the right-hand side.
                    On output, the solution vector.
    */
  def solve(LU: Array[Array[Double]], pvt: Array[Int], b: Array[Double]) = {
    val M = LU.length
    val N = LU(0).length
    var ii=0

    for (i <- 0 until M) {
      ///*
      val ip = pvt(i)
      var sum = b(ip)

      b(ip) = b(i)
     // */ var sum = b(i)
      if (ii==0) {
        for (j <- ii until i)
          sum -= LU(i)(j) * b(j)
      }
      else { 
        if (sum == 0.0)
          ii = i
      }
      b(i) = sum
    }

    var i = N-1
    while (i >= 0) {
      var sum = b(i)
      for (j <- (i+1) until N) {
        sum -= LU(i)(j) * b(j)
      }
      b(i) = sum / LU(i)(i)
      i -= 1
    }
  }               


  def verify(A: Array[Array[Double]], lu: Array[Array[Double]], pivot: Array[Int]): Double = {
    val N = A.length
    val b: Array[Double] = randomVector(N)
		val x = copyArray(b)

		solve(lu, pivot, x)
		
    return maxabs(b, matvec(A, x))
		//val EPS = 1.0e-12
		//if ( normabs(b, matvec(A,x)) / N > EPS )
	//		return false
  
    //return true
  }

  def maxabs(x: Array[Double], y: Array[Double]): Double = {
    val N = x.length
		var max = 0.0

		for (i <- 0 until N) {
		  val d =  math.abs(x(i)-y(i))
      if(d > max) max = d
    }
		return max
  }

  def normabs(x: Array[Double], y: Array[Double]): Double = {
		val N = x.length
		var sum = 0.0

		for (i <- 0 until N)
		  sum += math.abs(x(i)-y(i))

		return sum
  }

  def matvec(A: Array[Array[Double]], x: Array[Double]): Array[Double] = {
		val N = x.length
		var y: Array[Double] = Array.fill(N){0.0}

		matvec(A, x, y)

		return y
	}

	def matvec(A: Array[Array[Double]], x: Array[Double], y: Array[Double]) = {
		val M = A.length
		val N = A(0).length

		for (i <- 0 until M) {
			var sum = 0.0
			val Ai = A(i)
			for (j <- 0 until N)
				sum += Ai(j) * x(j)

			y(i) = sum
		}
	}

}// end DoubleDenseLU


object SmartDenseLU {
  import smartfloat.SmartFloat
  import SmartFloat._


  import DenseLU.pivoting

  /**
    LU factorization (in place).

    @param A (in/out) On input, the matrix to be factored.
        On output, the compact LU factorization.

    @param pivit (out) The pivot vector records the
        reordering of the rows of A during factorization.
        
    @return 0, if OK, nozero value, othewise.
  */
  def factor(A: Array[Array[SmartFloat]],  pivot: Array[Int]): Int = {
    val N = A.length
    val M = A(0).length

    val minMN = math.min(M,N)

    for (j <- 0 until minMN) {
      // find pivot in column j and  test for singularity.
       
        var jp = j
          
          if (pivoting) {
            var t = abs(A(j)(j))
            for (i <- (j+1) until M) {
                val ab = abs(A(i)(j))
                if ( ab > t) {
                    jp = i
                    t = ab
                }
            }
          }
          pivot(j) = jp

          // jp now has the index of maximum element 
          // of column j, below the diagonal

          if ( A(jp)(j) == 0 )                 
              return 1       // factorization failed because of zero pivot

          if (jp != j) {
              // swap rows j and jp
              val tA = A(j)
              A(j) = A(jp)
              A(jp) = tA
          }
      
      if (j < M-1) {                // compute elements j+1:M of jth column
          // note A(j,j), was A(jp,p) previously which was
          // guarranteed not to be zero (Label #1)
          //
          val recp =  1.0 / A(j)(j)

          for (k <- (j+1) until M)
              A(k)(j) *= recp
      }


      if (j < minMN-1) {
        // rank-1 update to trailing submatrix:   E = E - x*y;
        //
        // E is the region A(j+1:M, j+1:N)
        // x is the column vector A(j+1:M,j)
        // y is row vector A(j,j+1:N)


        for (ii <- (j+1) until M) {
            val Aii = A(ii)
            val Aj = A(j)
            val AiiJ = Aii(j)
            for (jj <- (j+1) until N)
              Aii(jj) -= AiiJ * Aj(jj)
        }
      }
    }
    return 0
  }


    /**
        Solve a linear system, using a prefactored matrix
            in LU form.


        @param LU (in) the factored matrix in LU form. 
        @param pivot (in) the pivot vector which lists
            the reordering used during the factorization
            stage.
        @param b    (in/out) On input, the right-hand side.
                    On output, the solution vector.
    */
  def solve(LU: Array[Array[SmartFloat]], pvt: Array[Int], b: Array[SmartFloat]) = {
    val M = LU.length
    val N = LU(0).length
    var ii=0

    for (i <- 0 until M) {
     //    /*
      val ip = pvt(i)
      var sum = b(ip)

      b(ip) = b(i)
     //   */ var sum = b(i)
      if (ii==0) {
        for (j <- ii until i)
          sum -= LU(i)(j) * b(j)
      }
      else { 
        if (sum == 0.0)
          ii = i
      }
      b(i) = sum
    }

    var i = N-1
    while (i >= 0) {
      var sum = b(i)
      for (j <- (i+1) until N) {
        sum -= LU(i)(j) * b(j)
      }
      b(i) = sum / LU(i)(i)
      i -= 1
    }
  }               


  def verify(A: Array[Array[SmartFloat]], lu: Array[Array[SmartFloat]], pivot: Array[Int]): 
    SmartFloat = {
    val N = A.length
    val b: Array[SmartFloat] = srandomVector(N)
    val x = scopyArray(b)

    solve(lu, pivot, x)
  
  
  
    //println("SmartFloat max absolute error: " + SmartRandomUtils.computeMaxAbsError(x))
    
    return SmartRandomUtils.computeMaxError(x)
    //return normabs(b, matvec(A,x))/N

    //val EPS = 1.0e-12
    //if ( normabs(b, matvec(A,x)) / N > EPS )
  //    return false
  
    //return true
  }

  def normabs(x: Array[SmartFloat], y: Array[SmartFloat]): SmartFloat = {
    val N = x.length
    var sum = SmartFloat(0.0)

    for (i <- 0 until N)
      sum += abs(x(i)-y(i))

    return sum
  }

  def matvec(A: Array[Array[SmartFloat]], x: Array[SmartFloat]): Array[SmartFloat] = {
    val N = x.length
    var y: Array[SmartFloat] = Array.fill(N){0.0}

    matvec(A, x, y)

    return y
  }

  def matvec(A: Array[Array[SmartFloat]], x: Array[SmartFloat], y: Array[SmartFloat]) = {
    val M = A.length
    val N = A(0).length

    for (i <- 0 until M) {
      var sum = SmartFloat(0.0)
      val Ai = A(i)
      for (j <- 0 until N)
        sum += Ai(j) * x(j)

      y(i) = sum
    }
  }
}

object AffineDenseLU {
  import smartfloat.AffineFloat
  import AffineFloat._


  import DenseLU.pivoting

  /**
    LU factorization (in place).

    @param A (in/out) On input, the matrix to be factored.
        On output, the compact LU factorization.

    @param pivit (out) The pivot vector records the
        reordering of the rows of A during factorization.
        
    @return 0, if OK, nozero value, othewise.
  */
  def factor(A: Array[Array[AffineFloat]],  pivot: Array[Int]): Int = {
    val N = A.length
    val M = A(0).length

    val minMN = math.min(M,N)

    for (j <- 0 until minMN) {
      // find pivot in column j and  test for singularity.
      
        var jp = j
        
        if (pivoting) {
          var t = abs(A(j)(j))
          for (i <- (j+1) until M) {
              val ab = abs(A(i)(j))
              if ( ab > t) {
                  jp = i
                  t = ab
              }
          }
        }
        pivot(j) = jp

        // jp now has the index of maximum element 
        // of column j, below the diagonal

        if ( A(jp)(j) == 0 )                 
            return 1       // factorization failed because of zero pivot

        if (jp != j) {
            // swap rows j and jp
            val tA = A(j)
            A(j) = A(jp)
            A(jp) = tA
        }
       
      if (j < M-1) {                // compute elements j+1:M of jth column
          // note A(j,j), was A(jp,p) previously which was
          // guarranteed not to be zero (Label #1)
          //
          val recp =  1.0 / A(j)(j)

          for (k <- (j+1) until M)
              A(k)(j) *= recp
      }


      if (j < minMN-1) {
        // rank-1 update to trailing submatrix:   E = E - x*y;
        //
        // E is the region A(j+1:M, j+1:N)
        // x is the column vector A(j+1:M,j)
        // y is row vector A(j,j+1:N)


        for (ii <- (j+1) until M) {
            val Aii = A(ii)
            val Aj = A(j)
            val AiiJ = Aii(j)
            for (jj <- (j+1) until N)
              Aii(jj) -= AiiJ * Aj(jj)
        }
      }
    }
    return 0
  }


    /**
        Solve a linear system, using a prefactored matrix
            in LU form.


        @param LU (in) the factored matrix in LU form. 
        @param pivot (in) the pivot vector which lists
            the reordering used during the factorization
            stage.
        @param b    (in/out) On input, the right-hand side.
                    On output, the solution vector.
    */
  def solve(LU: Array[Array[AffineFloat]], pvt: Array[Int], b: Array[AffineFloat]) = {
    val M = LU.length
    val N = LU(0).length
    var ii=0

    for (i <- 0 until M) {
    //   /*
      val ip = pvt(i)
      var sum = b(ip)

      b(ip) = b(i)
    //    */ var sum = b(i)
      if (ii==0) {
        for (j <- ii until i)
          sum -= LU(i)(j) * b(j)
      }
      else { 
        if (sum == 0.0)
          ii = i
      }
      b(i) = sum
    }

    var i = N-1
    while (i >= 0) {
      var sum = b(i)
      for (j <- (i+1) until N) {
        sum -= LU(i)(j) * b(j)
      }
      b(i) = sum / LU(i)(i)
      i -= 1
    }
  }               


  def verify(A: Array[Array[AffineFloat]], lu: Array[Array[AffineFloat]], pivot: Array[Int]): 
    (Double, Double) = {
    val N = A.length
    val b: Array[AffineFloat] = arandomVector(N)
    val x = acopyArray(b)

    solve(lu, pivot, x)
    
    
    //val verif = maxabs(b, matvec(A, acleanArray(x)))
    //println("AffineFloat max deviation of verification : " + verif.toStringWithAbsErrors)
    //aprintVector(x)
    //println("average abs.error: " + SmartRandomUtils.computeAvrgAbsError(x))
    //println("average rel.error: " + SmartRandomUtils.computeAvrgError(x))
    return (SmartRandomUtils.computeMaxError(x), SmartRandomUtils.computeMaxAbsError(x))
    //return normabs(b, matvec(A,x))/N

    //val EPS = 1.0e-12
    //if ( normabs(b, matvec(A,x)) / N > EPS )
  //    return false
  
    //return true
  }

  def maxabs(x: Array[AffineFloat], y: Array[AffineFloat]): AffineFloat = {
    val N = x.length
    var max = new AffineFloat(0.0)

    for (i <- 0 until N) {
      val d =  abs(x(i)-y(i))
      if(d > max) max = d
    }
    return max
  }

  def normabs(x: Array[AffineFloat], y: Array[AffineFloat]): AffineFloat = {
    val N = x.length
    var sum = AffineFloat(0.0)

    for (i <- 0 until N)
      sum += abs(x(i)-y(i))

    return sum
  }

  def matvec(A: Array[Array[AffineFloat]], x: Array[AffineFloat]): Array[AffineFloat] = {
    val N = x.length
    var y: Array[AffineFloat] = Array.fill(N){0.0}

    matvec(A, x, y)

    return y
  }

  def matvec(A: Array[Array[AffineFloat]], x: Array[AffineFloat], y: Array[AffineFloat]) = {
    val M = A.length
    val N = A(0).length

    for (i <- 0 until M) {
      var sum = AffineFloat(0.0)
      val Ai = A(i)
      for (j <- 0 until N)
        sum += Ai(j) * x(j)

      y(i) = sum
    }
  }

}



object IntervalDenseLU {
  import smartfloat.IntervalFloat
  import IntervalFloat._

  import DenseLU.pivoting

  /**
    LU factorization (in place).

    @param A (in/out) On input, the matrix to be factored.
        On output, the compact LU factorization.

    @param pivit (out) The pivot vector records the
        reordering of the rows of A during factorization.
        
    @return 0, if OK, nozero value, othewise.
  */
  def factor(A: Array[Array[IntervalFloat]],  pivot: Array[Int]): Int = {
    val N = A.length
    val M = A(0).length

    val minMN = math.min(M,N)

    for (j <- 0 until minMN) {
      // find pivot in column j and  test for singularity.
      
        var jp = j
        
        if (pivoting) {
          var t = abs(A(j)(j))
          for (i <- (j+1) until M) {
              val ab = abs(A(i)(j))
              if ( ab > t) {
                  jp = i
                  t = ab
              }
          }
        }
        pivot(j) = jp

        // jp now has the index of maximum element 
        // of column j, below the diagonal

        if ( A(jp)(j) == 0 )                 
            return 1       // factorization failed because of zero pivot

        if (jp != j) {
            // swap rows j and jp
            val tA = A(j)
            A(j) = A(jp)
            A(jp) = tA
        }
      
      if (j < M-1) {                // compute elements j+1:M of jth column
          // note A(j,j), was A(jp,p) previously which was
          // guarranteed not to be zero (Label #1)
          //
          val recp =  1.0 / A(j)(j)

          for (k <- (j+1) until M)
              A(k)(j) *= recp
      }


      if (j < minMN-1) {
        // rank-1 update to trailing submatrix:   E = E - x*y;
        //
        // E is the region A(j+1:M, j+1:N)
        // x is the column vector A(j+1:M,j)
        // y is row vector A(j,j+1:N)


        for (ii <- (j+1) until M) {
            val Aii = A(ii)
            val Aj = A(j)
            val AiiJ = Aii(j)
            for (jj <- (j+1) until N)
              Aii(jj) -= AiiJ * Aj(jj)
        }
      }
    }
    return 0
  }


    /**
        Solve a linear system, using a prefactored matrix
            in LU form.


        @param LU (in) the factored matrix in LU form. 
        @param pivot (in) the pivot vector which lists
            the reordering used during the factorization
            stage.
        @param b    (in/out) On input, the right-hand side.
                    On output, the solution vector.
    */
  def solve(LU: Array[Array[IntervalFloat]], pvt: Array[Int], b: Array[IntervalFloat]) = {
    val M = LU.length
    val N = LU(0).length
    var ii=0

    for (i <- 0 until M) {
       /*
      val ip = pvt(i)
      var sum = b(ip)

      b(ip) = b(i)
        */ var sum = b(i)
      if (ii==0) {
        for (j <- ii until i)
          sum -= LU(i)(j) * b(j)
      }
      else { 
        if (sum == 0.0)
          ii = i
      }
      b(i) = sum
    }

    var i = N-1
    while (i >= 0) {
      var sum = b(i)
      for (j <- (i+1) until N) {
        sum -= LU(i)(j) * b(j)
      }
      b(i) = sum / LU(i)(i)
      i -= 1
    }
  }               


  def verify(A: Array[Array[IntervalFloat]], lu: Array[Array[IntervalFloat]], pivot: Array[Int]): 
    (Double, Double) = {
    val N = A.length
    val b: Array[IntervalFloat] = irandomVector(N)
    val x = icopyArray(b)

    solve(lu, pivot, x)
    
    
    //val verif = maxabs(b, matvec(A, acleanArray(x)))
    //println("IntervalFloat max deviation of verification : " + verif.toStringWithAbsErrors)
    //aprintVector(x)
    //println("average abs.error: " + SmartRndomUtils.computeAvrgAbsError(x))
    //println("average rel.error: " + SmartRandomUtils.computeAvrgError(x))
    return (SmartRandomUtils.computeMaxError(x), SmartRandomUtils.computeMaxAbsError(x))
    //return normabs(b, matvec(A,x))/N

    //val EPS = 1.0e-12
    //if ( normabs(b, matvec(A,x)) / N > EPS )
  //    return false
  
    //return true
  }

  def maxabs(x: Array[IntervalFloat], y: Array[IntervalFloat]): IntervalFloat = {
    val N = x.length
    var max = new IntervalFloat(0.0)

    for (i <- 0 until N) {
      val d =  abs(x(i)-y(i))
      if(d > max) max = d
    }
    return max
  }

  def normabs(x: Array[IntervalFloat], y: Array[IntervalFloat]): IntervalFloat = {
    val N = x.length
    var sum = IntervalFloat(0.0)

    for (i <- 0 until N)
      sum += abs(x(i)-y(i))

    return sum
  }

  def matvec(A: Array[Array[IntervalFloat]], x: Array[IntervalFloat]): Array[IntervalFloat] = {
    val N = x.length
    var y: Array[IntervalFloat] = Array.fill(N){0.0}

    matvec(A, x, y)

    return y
  }

  def matvec(A: Array[Array[IntervalFloat]], x: Array[IntervalFloat], y: Array[IntervalFloat]) = {
    val M = A.length
    val N = A(0).length

    for (i <- 0 until M) {
      var sum = IntervalFloat(0.0)
      val Ai = A(i)
      for (j <- 0 until N)
        sum += Ai(j) * x(j)

      y(i) = sum
    }
  }

}


