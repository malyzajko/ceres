package ceres
package benchmarks

import scala.util.Random

object RandomUtils {

  private val random = new Random(373)//(System.currentTimeMillis)
  
  private val maxNo = 1.0

  //FIXME: may need to have different range of random numbers
  def randomVector(n: Int): Array[Double] = {
	  val A = Array.fill(n){0.0}

		for (i <- 0 until n)
			A(i) = random.nextDouble * maxNo
		return A
	}

  def randomMatrix(m: Int, n: Int): Array[Array[Double]] = {
   val A: Array[Array[Double]] = Array.fill(n, m){0.0}

    for (i <- 0 until n)
		  for (j <- 0 until m)
        	A(i)(j) = random.nextDouble * maxNo
		return A
	}

	def copyArray(data: Array[Double]): Array[Double] = {
    val newData = Array.fill(data.length){0.0}
    for (i <- 0 until data.length)
      newData(i) = data(i)
	  newData
	}

	def copyArray(data: Array[Int]): Array[Int] = {
    val newData = Array.fill(data.length){0}
    for (i <- 0 until data.length)
      newData(i) = data(i)
	  newData
	}

  def copyMatrix(data: Array[Array[Double]]): Array[Array[Double]] = {
    val newData = Array.fill(data.length, data(0).length){0.0}
    for (i <- 0 until data.length)
      for (j <- 0 until data(0).length)
        newData(i)(j) = data(i)(j)
	  newData
	}
	
	def printMatrix(m: Array[Array[Double]]) = {
	  print("[")
    m.foreach { row =>
      row.foreach { e => 
        print(e + " ")
      } 
      print("\n")
    }
    print("]")	
	}
	
	def printArray(m: Array[Double]) = {
	  print("[")
    m.foreach { e => 
        print(e + " ")
    }
    print("]\n")	
	}
	
	def printArray(m: Array[Int]) = {
	  print("[")
    m.foreach { e => 
        print(e + " ")
    }
    print("]\n")	
	}
  
}
