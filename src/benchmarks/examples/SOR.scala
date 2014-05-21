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
	var doubleMatrix100 = randomMatrix(100, 100)
  var affineMatrix100 = aconvertMatrix(doubleMatrix100)
  var smartMatrix100 = sconvertMatrix(doubleMatrix100)
  var intervalMatrix100 = iconvertMatrix(doubleMatrix100)

	def affineSOR(numIter: Int) = {
    val sm1 = affineMatrix100
    val res = executeAffine(1.25, sm1, numIter)
    res
  }
  
  def intervalSOR(numIter: Int) = {
    val sm1 = intervalMatrix100
    val res = executeInterval(1.25, sm1, numIter)
    res
  }

  def smartSOR(numIter: Int) = {
    val sm1 = smartMatrix100
    val res = executeSmart(1.25, sm1, numIter)
    res
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

	def executeAffine(omega: AffineFloat, G: Array[Array[AffineFloat]], num_iterations: Int): Double = {
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
		//println("average rel.error: " + SmartRandomUtils.computeAvrgError(G))
		return SmartRandomUtils.computeMaxError(G)
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

	def executeInterval(omega: IntervalFloat, G: Array[Array[IntervalFloat]], num_iterations: Int): Double = {
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
		//println("average rel.error: " + SmartRandomUtils.computeAvrgError(G))
		return SmartRandomUtils.computeMaxError(G)
	}
}


