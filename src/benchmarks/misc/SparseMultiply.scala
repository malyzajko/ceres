package ceres
package benchmarks
package scimark

import RandomUtils._

/**Description from SciMark homepage:
Sparse matrix multiply uses an unstructured sparse matrix stored in compressed-row format with a prescribed sparsity structure. This kernel exercises indirection addressing and non-regular memory references. A 1,000 x 1,000 sparse matrix with 5,000 nonzeros is used, with the following storage pattern:

**---------------.
***              |
* * *            |
** *  *          |
** **   *        |
** * *    *      |
* *  * *    *    |
*  *  *  *    *  |
*   *  *   *    *|
*---*---*----*---*

That is, each row has approximately 5 nonzeros, evenly spaced between the first column and the diagonal.
The data size for the LARGE version of the benchmark uses a 100,000 x 100,000 matrix with 1,000,000 nonzeros. 
*/
object SparseMultiply {

 
	/* computes  a matrix-vector multiply with a sparse matrix
		held in compress-row format.  If the size of the matrix
		in MxN with nz nonzeros, then the val[] is the nz nonzeros,
		with its ith entry in column col[i].  The integer vector row[]
		is of size M+1 and row[i] points to the begining of the
		ith row in col[].  
	*/

	def matmult(y: Array[Double], vall: Array[Double], row: Array[Int],
		col: Array[Int], x: Array[Double], NUM_ITERATIONS: Int) = {
		val M: Int = row.length - 1;

		for (reps <- 0 until NUM_ITERATIONS) 	{
		
			for (r <- 0 until M)	{
				var sum = 0.0 
				val rowR = row(r)
				val rowRp1 = row(r+1)
				for (i <- rowR until rowRp1) {
					sum += x( col(i) ) * vall(i)
				}
				y(r) = sum
			}
		}
	}
  
  
  def initialize(N: Int, nz: Int): (Array[Double], Array[Int], Array[Int])  = {
    // initialize vector multipliers and storage for result
		// y = A*y;

    val nr: Int = nz/N 		// average number of nonzeros per row
		val anz: Int = nr *N   // _actual_ number of nonzeros

			
		val vall: Array[Double] = randomVector(anz)
		val col: Array[Int] = Array.fill(anz){0}
		val row: Array[Int] = Array.fill(N+1){0}

		row(0) = 0	
		for (r <- 0 until N) {
			// initialize elements for row r

			val rowr: Int = row(r)
			row(r+1) = rowr + nr
			var step: Int = r/ nr
			if (step < 1) step = 1   // take at least unit steps


			for (i <- 0 until nr)
				col(rowr+i) = i*step
		}
		
		return (vall, row, col)
  }

  /*
  public static double measureSparseMatmult(int N, int nz, 
			double min_time, Random R)
	{
		// initialize vector multipliers and storage for result
		// y = A*y;

		double x[] = RandomVector(N, R);
		double y[] = new double[N];

		// initialize square sparse matrix
		//
		// for this test, we create a sparse matrix wit M/nz nonzeros
		// per row, with spaced-out evenly between the begining of the
		// row to the main diagonal.  Thus, the resulting pattern looks
		// like
		//             +-----------------+
		//             +*                +
		//             +***              +
		//             +* * *            +
		//             +** *  *          +
		//             +**  *   *        +
		//             +* *   *   *      +
		//             +*  *   *    *    +
		//             +*   *    *    *  + 
		//             +-----------------+
		//
		// (as best reproducible with integer artihmetic)
		// Note that the first nr rows will have elements past
		// the diagonal.

		int nr = nz/N; 		// average number of nonzeros per row
		int anz = nr *N;   // _actual_ number of nonzeros

			
		double val[] = RandomVector(anz, R);
		int col[] = new int[anz];
		int row[] = new int[N+1];

		row[0] = 0;	
		for (int r=0; r<N; r++)
		{
			// initialize elements for row r

			int rowr = row[r];
			row[r+1] = rowr + nr;
			int step = r/ nr;
			if (step < 1) step = 1;   // take at least unit steps


			for (int i=0; i<nr; i++)
				col[rowr+i] = i*step;
				
		}

		Stopwatch Q = new Stopwatch();

		int cycles=1;
		while(true)
		{
			Q.start();
			SparseCompRow.matmult(y, val, row, col, x, cycles);
			Q.stop();
			if (Q.read() >= min_time) break;

			cycles *= 2;
		}
		// approx Mflops
		return SparseCompRow.num_flops(N, nz, cycles) / Q.read() * 1.0e-6;
	}
	*/

}
