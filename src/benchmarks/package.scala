package ceres
//package benchmarks

import scala.util.Random
import smartfloat.{AffineFloat,SmartFloat,IntervalFloat}
import scala.language.reflectiveCalls

package object benchmarks {


  def computeMaxRelError(m: Array[Array[SmartFloat]]): Double = {
    var max = 0.0
    m.foreach{ row =>
      row.foreach {entry =>
        if(entry.relError != entry.relError) return Double.NaN
        if(entry.relError > max) max = entry.relError
      }    
    }   
    return max
  }
  
  def computeMaxRelError(m: Array[SmartFloat]): Double = {
    var max = 0.0
    m.foreach{ entry =>
      if(entry.relError != entry.relError) return Double.NaN
      if(entry.relError > max) max = entry.relError    
    }   
    return max
  }
  
  def computeMaxAbsError(m: Array[SmartFloat]): Double = {
    var max = -1.0
    m.foreach{ entry =>
      if(entry.absError != entry.absError) return Double.NaN
      if(entry.absError > max) max = entry.absError    
    }   
    return max
  }
  
  def computeMaxAbsError(m: Array[Array[SmartFloat]]): Double = {
    var max = -1.0
    m.foreach{ row =>
      row.foreach {entry =>
        if(entry.absError != entry.absError) return Double.NaN
        if(entry.absError > max) max = entry.absError
      }    
    }   
    return max
  }

  def computeMaxRelError(m: Array[AffineFloat]): Double = {
    var max = 0.0
    m.foreach{ entry =>
      if(entry.relError != entry.relError) return Double.NaN
      if(entry.relError > max) max = entry.relError    
    }   
    return max
  }
  
  def computeMaxAbsError(m: Array[AffineFloat]): Double = {
    var max = 0.0
    m.foreach{ entry =>
      if(entry.absError != entry.absError) return Double.NaN
      if(entry.absError > max) max = entry.absError    
    }   
    return max
  }
  
   def computeMaxRelError(m: Array[Array[AffineFloat]]): Double = {
    var max = -1.0
    m.foreach{ row =>
      row.foreach {entry =>
        if(entry.relError != entry.relError) return Double.NaN
        if(entry.relError > max) max = entry.relError
      }    
    }   
    return max
  }

  def computeMaxAbsError(m: Array[Array[AffineFloat]]): Double = {
    var max = -1.0
    m.foreach{ row =>
      row.foreach {entry =>
        if(entry.absError != entry.absError) return Double.NaN
       if(entry.absError > max) max = entry.absError
      }    
    }   
    return max
  }

  def computeMaxRelError(m: Array[Array[IntervalFloat]]): Double = {
    var max = -1.0
    m.foreach{ row =>
      row.foreach {entry =>
        if(entry.relError != entry.relError) return Double.NaN
       if(entry.relError > max) max = entry.relError
      }    
    }   
    return max
  }

  def computeMaxAbsError(m: Array[Array[IntervalFloat]]): Double = {
    var max = -1.0
    m.foreach{ row =>
      row.foreach {entry =>
        if(entry.absError != entry.absError) return Double.NaN
       if(entry.absError > max) max = entry.absError
      }    
    }   
    return max
  }
  
  def computeMaxRelError(m: Array[IntervalFloat]): Double = {
    var max = 0.0
    m.foreach{ entry =>
      if(entry.relError != entry.relError) return Double.NaN
      if(entry.relError > max) max = entry.relError    
    }   
    return max
  }
  
  def computeMaxAbsError(m: Array[IntervalFloat]): Double = {
    var max = 0.0
    m.foreach{ entry =>
      if(entry.absError != entry.absError) return Double.NaN
      if(entry.absError > max) max = entry.absError    
    }   
    return max
  }



   /*def computeMaxRelError(m: Array[{ def relError: Double }]): Double = {
    var max = 0.0
    m.foreach{ entry =>
      if(entry.relError != entry.relError) return Double.NaN
      if(entry.relError > max) max = entry.relError    
    }   
    return max
  }

  def computeMaxAbsError(m: Array[{ def absError: Double }]): Double = {
    var max = -1.0
    m.foreach{ entry =>
      if(entry.absError != entry.absError) return Double.NaN
      if(entry.absError > max) max = entry.absError    
    }   
    return max
  }

  def computeMaxRelError(m: Array[Array[{ def relError: Double }]]): Double = {
    var max = 0.0
    m.foreach{ row =>
      row.foreach {entry =>
        if(entry.relError != entry.relError) return Double.NaN
        if(entry.relError > max) max = entry.relError
      }    
    }   
    return max
  }


  def computeMaxAbsError(m: Array[Array[{ def absError: Double }]]): Double = {
    var max = 0.0
    m.foreach{ row =>
      row.foreach {entry =>
        if(entry.absError != entry.absError) return Double.NaN
        if(entry.absError > max) max = entry.absError
      }    
    }   
    return max
  }
  */

}