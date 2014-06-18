package ceres
package benchmarks

import RandomUtils._
import SmartRandomUtils._

import smartfloat.{AffineFloat, SmartFloat, IntervalFloat}
import AffineFloat._
import SmartFloat._
import IntervalFloat._

/** Description from Scimark homepage:
Jacobi Successive Over-relaxation (SOR) on a 100x100 grid exercises typical access patterns in 
finite difference applications, for example, solving Laplace's equation in 2D with Drichlet boundary
conditions. The algorithm excercises basic "grid averaging" memory patterns, where each A(i,j) is 
assigned an average weighting of its four nearest neighbors. 
The data size for the LARGE version of the benchmark uses a 1,000x1,000 grid. 
*/
object SOR {
	// so they are equal for comparison of errors, this also means that the
	// starting point is always the same
	var doubleMatrix100 = randomSquareMatrix(100, 10.0, 4357)
  var affineMatrix100 = aconvertMatrix(doubleMatrix100)
  var smartMatrix100 = sconvertMatrix(doubleMatrix100)
  var intervalMatrix100 = iconvertMatrix(doubleMatrix100)

  //@return (max rel err affine, max abs error affine, max rel error interval, max abs error interval)
  def compareAffineInterval(numIter: Int, seed: Long): (Double, Double, Double, Double) = {
  	val doubleM = randomSquareMatrix(100, 10.0, seed)
  	val affineM = aconvertMatrix(doubleM)
  	val intervalM = iconvertMatrix(doubleM)

  	val (relAA, absAA) = executeAffine(1.25, affineM, numIter)
  	val (relInt, absInt) = executeInterval(1.25, intervalM, numIter)
  	(relAA, absAA, relInt, absInt)
  }


	def affineSOR(numIter: Int) = {
    executeAffine(1.25, affineMatrix100, numIter)
  }
  
  def intervalSOR(numIter: Int) = {
    executeInterval(1.25, intervalMatrix100, numIter)
  }

  def doubleSOR(numIter: Int) = {
    execute(1.25, doubleMatrix100, numIter)
  }

  def smartSOR(numIter: Int) = {
    executeSmart(1.25, smartMatrix100, numIter)
  }

	def execute(omega: Double, G: Array[Array[Double]], num_iterations: Int) = {
		val M = G.length
		val N = G(0).length;

		val omega_over_four = omega * 0.25
		val one_minus_omega = 1.0 - omega

		// update interior points
		//
		val Mm1 = M-1;
		val Nm1 = N-1; 
		for (p <- 0 until num_iterations) {
			for (i <- 1 until Mm1) {
				val Gi = G(i)
				val Gim1 = G(i-1)
				val Gip1 = G(i+1)
				for (j <- 1 until Nm1) {
				  Gi(j) = omega_over_four * (Gim1(j) + Gip1(j) + Gi(j-1)
								+ Gi(j+1)) + one_minus_omega * Gi(j)
				}
			}
		}
	}

	//@return (max relative error, max absolute error)
	def executeAffine(omega: AffineFloat, G: Array[Array[AffineFloat]], num_iterations: Int): (Double, Double) = {
		val M = G.length
		val N = G(0).length;

		val omega_over_four = omega * 0.25
		val one_minus_omega = 1.0 - omega

		// update interior points
		//
		val Mm1 = M-1;
		val Nm1 = N-1; 
		for (p <- 0 until num_iterations) {
			for (i <- 1 until Mm1) {
				val Gi = G(i)
				val Gim1 = G(i-1)
				val Gip1 = G(i+1)
				for (j <- 1 until Nm1) {
				  Gi(j) = omega_over_four * (Gim1(j) + Gip1(j) + Gi(j-1)
								+ Gi(j+1)) + one_minus_omega * Gi(j)
				}
			}
		}
		
		val maxRel = SmartRandomUtils.computeMaxRelError(G)
		val maxAbs = SmartRandomUtils.computeMaxAbsError(G)
		(maxRel, maxAbs)
	}

	def executeSmart(omega: SmartFloat, G: Array[Array[SmartFloat]], num_iterations: Int): Double = {
		val M = G.length
		val N = G(0).length;

		val omega_over_four = omega * 0.25
		val one_minus_omega = 1.0 - omega

		// update interior points
		//
		val Mm1 = M-1;
		val Nm1 = N-1; 
		for (p <- 0 until num_iterations) {
			for (i <- 1 until Mm1) {
				val Gi = G(i)
				val Gim1 = G(i-1)
				val Gip1 = G(i+1)
				for (j <- 1 until Nm1) {
				  Gi(j) = omega_over_four * (Gim1(j) + Gip1(j) + Gi(j-1)
								+ Gi(j+1)) + one_minus_omega * Gi(j)
				}
			}
		}
		
		//if we want the computed error:
		//println("computed max error for " + smartfloats.tools.AffineForm.maxNoiseCount + ": " + 
		//  SmartRandomUtils.computeMaxError(G))
		return SmartRandomUtils.computeMaxError(G)
	}

	def executeInterval(omega: IntervalFloat, G: Array[Array[IntervalFloat]], num_iterations: Int): (Double,Double) = {
		val M = G.length
		val N = G(0).length;

		val omega_over_four = omega * 0.25
		val one_minus_omega = 1.0 - omega

		// update interior points
		//
		val Mm1 = M-1;
		val Nm1 = N-1; 
		for (p <- 0 until num_iterations) {
			for (i <- 1 until Mm1) {
				val Gi = G(i)
				val Gim1 = G(i-1)
				val Gip1 = G(i+1)
				for (j <- 1 until Nm1) {
				  Gi(j) = omega_over_four * (Gim1(j) + Gip1(j) + Gi(j-1)
								+ Gi(j+1)) + one_minus_omega * Gi(j)
				}
			}
		}
		
		val maxRel = SmartRandomUtils.computeMaxRelError(G)
		val maxAbs = SmartRandomUtils.computeMaxAbsError(G)
		(maxRel, maxAbs)
	}
}


