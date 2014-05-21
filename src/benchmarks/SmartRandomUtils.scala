package ceres
package benchmarks

import scala.util.Random
import smartfloat.{AffineFloat,SmartFloat,IntervalFloat}

object SmartRandomUtils {

  private val random = new Random(373)  //that's a so-called sexy prime
  private val maxNo = 10.0

  //FIXME: may need to have different range of random numbers
  def srandomVector(n: Int): Array[SmartFloat] = {
	  val A = Array.fill(n){SmartFloat(0.0)}

		for (i <- 0 until n)
			A(i) = new SmartFloat(random.nextDouble*maxNo)
		return A
	}

  def srandomMatrix(m: Int, n: Int): Array[Array[SmartFloat]] = {
   val A: Array[Array[SmartFloat]] = Array.fill(n, m){0.0}

    for (i <- 0 until n)
		  for (j <- 0 until m)
        	A(i)(j) = SmartFloat(random.nextDouble*maxNo)
		return A
	}

	def scopyArray(data: Array[SmartFloat]): Array[SmartFloat] = {
    val newData = Array.fill(data.length){SmartFloat(0.0)}
    for (i <- 0 until data.length)
      newData(i) = data(i)
	  newData
	}
	
	def sconvertArray(data: Array[Double]): Array[SmartFloat] = {
    val newData = Array.fill(data.length){SmartFloat(0.0)}
    for (i <- 0 until data.length)
      newData(i) = new SmartFloat(data(i))
	  newData
	}

	def scopyArray(data: Array[Int]): Array[Int] = {
    val newData = Array.fill(data.length){0}
    for (i <- 0 until data.length)
      newData(i) = data(i)
	  newData
	}

  def scopyMatrix(data: Array[Array[SmartFloat]]): Array[Array[SmartFloat]] = {
    val newData = Array.fill(data.length, data(0).length){SmartFloat(0.0)}
    for (i <- 0 until data.length)
      for (j <- 0 until data(0).length)
        newData(i)(j) = data(i)(j)
	  newData
	}
	
	def sconvertMatrix(data: Array[Array[Double]]): Array[Array[SmartFloat]] = {
    val newData = Array.fill(data.length, data(0).length){SmartFloat(0.0)}
    for (i <- 0 until data.length)
      for (j <- 0 until data(0).length)
        newData(i)(j) = new SmartFloat(data(i)(j))
	  newData
	}
	
	def sprintMatrix(m: Array[Array[SmartFloat]]) = {
	  print("[")
    m.foreach { row =>
      row.foreach { e => 
        print(e + " ")
      } 
      print("\n")
    }
    print("]")	
	}

	def sprintArray(m: Array[SmartFloat]) = {
	  print("[")
    m.foreach { e => 
        print(e + " ")
    }
    print("]\n")
    println("with rel errors:")
    	print("[")
    m.foreach { e => 
        print(e.relError + " ")
    }
    print("]\n")
	}
  
  def computeMaxError(m: Array[Array[SmartFloat]]): Double = {
    var max = 0.0
    m.foreach{ row =>
      row.foreach {entry => 
        if(entry.relError > max) max = entry.relError
      }    
    }   
    return max
  }
  
  def computeMaxError(m: Array[SmartFloat]): Double = {
    var max = 0.0
    m.foreach{ entry =>
      if(entry.relError > max) max = entry.relError    
    }   
    return max
  }
  
  def computeMaxAbsError(m: Array[SmartFloat]): Double = {
    var max = 0.0
    m.foreach{ entry =>
      if(entry.relError > max) max = entry.absError    
    }   
    return max
  }
  
  
  //#################################################################
  def arandomVector(n: Int): Array[AffineFloat] = {
	  val A = Array.fill(n){AffineFloat(0.0)}

		for (i <- 0 until n)
			A(i) = new AffineFloat(random.nextDouble*maxNo)
		return A
	}

  def arandomMatrix(m: Int, n: Int): Array[Array[AffineFloat]] = {
   val A: Array[Array[AffineFloat]] = Array.fill(n, m){0.0}

    for (i <- 0 until n)
		  for (j <- 0 until m)
        	A(i)(j) = AffineFloat(random.nextDouble*maxNo)
		return A
	}

	def acopyArray(data: Array[AffineFloat]): Array[AffineFloat] = {
    val newData = Array.fill(data.length){AffineFloat(0.0)}
    for (i <- 0 until data.length)
      newData(i) = data(i)
	  newData
	}
	
	def acleanArray(data: Array[AffineFloat]): Array[AffineFloat] = {
    val newData = Array.fill(data.length){AffineFloat(0.0)}
    for (i <- 0 until data.length)
      newData(i) = new AffineFloat(data(i).aa.x0)   //removing accumulated roundoffs
	  newData
	}

	def acopyArray(data: Array[Int]): Array[Int] = {
    val newData = Array.fill(data.length){0}
    for (i <- 0 until data.length)
      newData(i) = data(i)
	  newData
	}
	
	def aconvertArray(data: Array[Double]): Array[AffineFloat] = {
    val newData = Array.fill(data.length){AffineFloat(0.0)}
    for (i <- 0 until data.length)
      newData(i) = new AffineFloat(data(i))
	  newData
	}

  def acopyMatrix(data: Array[Array[AffineFloat]]): Array[Array[AffineFloat]] = {
    val newData = Array.fill(data.length, data(0).length){AffineFloat(0.0)}
    for (i <- 0 until data.length)
      for (j <- 0 until data(0).length)
        newData(i)(j) = data(i)(j)
	  newData
	}
	
	def aconvertMatrix(data: Array[Array[Double]]): Array[Array[AffineFloat]] = {
    val newData = Array.fill(data.length, data(0).length){AffineFloat(0.0)}
    for (i <- 0 until data.length)
      for (j <- 0 until data(0).length)
        newData(i)(j) = new AffineFloat(data(i)(j))
	  newData
	}
	
	def aprintMatrix(m: Array[Array[AffineFloat]]) = {
	  print("[")
    m.foreach { row =>
      row.foreach { e => 
        print(e + " ")
      } 
      print("\n")
    }
    print("]\n")	
	}
	
	def aprintVector(m: Array[AffineFloat]) = {
	  print("[")
    m.foreach { e => 
        print(e + " ")
    }
    print("]\n")	
	}
  
  def computeMaxError(m: Array[Array[AffineFloat]]): Double = {
    var max = -1.0
    m.foreach{ row =>
      row.foreach {entry =>
       if(entry.relError > max) max = entry.relError
      }    
    }   
    return max
  }
  
  def computeAvrgError(m: Array[Array[AffineFloat]]): Double = {
    var sum = 0.0
    var count = 0
    m.foreach{ row =>
      row.foreach {entry =>
        sum += entry.relError
        count += 1
      }    
    }   
    return sum/count
  }
  
  def computeAvrgAbsError(m: Array[AffineFloat]): Double = {
    var sum = 0.0
    var count = 0
    m.foreach{ entry =>
        sum += entry.absError
        count += 1
      
    }   
    return sum/count
  }
  
  def computeAvrgError(m: Array[AffineFloat]): Double = {
    var sum = 0.0
    var count = 0
    m.foreach{ entry =>
        sum += entry.relError
        count += 1
    }   
    return sum/count
  }
  
  def computeMaxError(m: Array[AffineFloat]): Double = {
    var max = 0.0
    m.foreach{ entry =>
      if(entry.relError > max) max = entry.relError    
    }   
    return max
  }
  
  def computeMaxAbsError(m: Array[AffineFloat]): Double = {
    var max = 0.0
    m.foreach{ entry =>
      if(entry.relError > max) max = entry.absError    
    }   
    return max
  }
  
  
  //#################################################################
  def irandomVector(n: Int): Array[IntervalFloat] = {
	  val A = Array.fill(n){IntervalFloat(0.0)}

		for (i <- 0 until n)
			A(i) = new IntervalFloat(random.nextDouble*maxNo)
		return A
	}

  def irandomMatrix(m: Int, n: Int): Array[Array[IntervalFloat]] = {
   val A: Array[Array[IntervalFloat]] = Array.fill(n, m){IntervalFloat(0.0)}

    for (i <- 0 until n)
		  for (j <- 0 until m)
        	A(i)(j) = IntervalFloat(random.nextDouble*maxNo)
		return A
	}

	def icopyArray(data: Array[IntervalFloat]): Array[IntervalFloat] = {
    val newData = Array.fill(data.length){IntervalFloat(0.0)}
    for (i <- 0 until data.length)
      newData(i) = data(i)
	  newData
	}
	
	
	def icopyArray(data: Array[Int]): Array[Int] = {
    val newData = Array.fill(data.length){0}
    for (i <- 0 until data.length)
      newData(i) = data(i)
	  newData
	}
	
	def iconvertArray(data: Array[Double]): Array[IntervalFloat] = {
    val newData = Array.fill(data.length){IntervalFloat(0.0)}
    for (i <- 0 until data.length)
      newData(i) = new IntervalFloat(data(i))
	  newData
	}

  def icopyMatrix(data: Array[Array[IntervalFloat]]): Array[Array[IntervalFloat]] = {
    val newData = Array.fill(data.length, data(0).length){IntervalFloat(0.0)}
    for (i <- 0 until data.length)
      for (j <- 0 until data(0).length)
        newData(i)(j) = data(i)(j)
	  newData
	}
	
	def iconvertMatrix(data: Array[Array[Double]]): Array[Array[IntervalFloat]] = {
    val newData = Array.fill(data.length, data(0).length){IntervalFloat(0.0)}
    for (i <- 0 until data.length)
      for (j <- 0 until data(0).length)
        newData(i)(j) = new IntervalFloat(data(i)(j))
	  newData
	}
	
	def iprintMatrix(m: Array[Array[IntervalFloat]]) = {
	  print("[")
    m.foreach { row =>
      row.foreach { e => 
        print(e + " ")
      } 
      print("\n")
    }
    print("]\n")	
	}
	
	def iprintVector(m: Array[IntervalFloat]) = {
	  print("[")
    m.foreach { e => 
        print(e + " ")
    }
    print("]\n")	
	}
  
  def computeMaxError(m: Array[Array[IntervalFloat]]): Double = {
    var max = -1.0
    m.foreach{ row =>
      row.foreach {entry =>
       if(entry.relError > max) max = entry.relError
      }    
    }   
    return max
  }
  
  def computeAvrgError(m: Array[Array[IntervalFloat]]): Double = {
    var sum = 0.0
    var count = 0
    m.foreach{ row =>
      row.foreach {entry =>
        sum += entry.relError
        count += 1
      }    
    }   
    return sum/count
  }
  
  def computeAvrgAbsError(m: Array[IntervalFloat]): Double = {
    var sum = 0.0
    var count = 0
    m.foreach{ entry =>
        sum += entry.absError
        count += 1
      
    }   
    return sum/count
  }
  
  def computeAvrgError(m: Array[IntervalFloat]): Double = {
    var sum = 0.0
    var count = 0
    m.foreach{ entry =>
        sum += entry.relError
        count += 1
    }   
    return sum/count
  }
  
  def computeMaxError(m: Array[IntervalFloat]): Double = {
    var max = 0.0
    m.foreach{ entry =>
      if(entry.relError > max) max = entry.relError    
    }   
    return max
  }
  
  def computeMaxAbsError(m: Array[IntervalFloat]): Double = {
    var max = 0.0
    m.foreach{ entry =>
      if(entry.relError > max) max = entry.absError    
    }   
    return max
  }
}









