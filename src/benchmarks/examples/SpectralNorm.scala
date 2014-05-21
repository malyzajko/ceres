/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
   modified by Meiko Rachimow
   updated for 2.8 by Rex Kerr
*/
package ceres
package benchmarks
import smartfloat.{AffineFloat, SmartFloat, IntervalFloat}



object SpectralNorm{ //(n: Int) {

  
  def approximate(n: Int) = {

    // Ordinary and transposed versions of infinite matrix
    val A = (i: Int, j: Int) => 1.0/((i+j)*(i+j+1)/2 +i+1)
    val At = (j: Int, i: Int) => 1.0/((i+j)*(i+j+1)/2 +i+1)

    // Matrix multiplication w <- M*v
    def mult(v: Array[Double], w: Array[Double], M: (Int,Int)=> Double ) {
      var i = 0
      while (i < n) {
       var s = 0.0
       var j = 0
       while (j < n) { s += M(i,j)*v(j); j += 1 }
       w(i) =  s
       i += 1
      }
    }

    val u,v,w = Array.fill(n)(1.0)

    var i = 0
    while (i < 10) {
      // Multiply by matrix & transpose
      mult(u,w,A)
      mult(w,v,At)
      mult(v,w,A)
      mult(w,u,At)
      i += 1
    }

    var vbv,vv = 0.0
    i = 0
    while (i < n) {
      vbv += u(i)*v(i)
      vv += v(i)*v(i)
      i += 1
    }

    math.sqrt(vbv/vv)
  }

  def approximateAffine(n: Int): AffineFloat = {
    import AffineFloat._

    val A = (i: Int, j: Int) => AffineFloat(1.0)/((i+j)*(i+j+1)/2 +i+1)
    val At = (j: Int, i: Int) => AffineFloat(1.0)/((i+j)*(i+j+1)/2 +i+1)

    // Matrix multiplication w <- M*v
    def mult(v: Array[AffineFloat], w: Array[AffineFloat], M: (Int,Int)=> AffineFloat ) {
      var i = 0
      while (i < n) {
       var s = AffineFloat(0.0)
       var j = 0
       while (j < n) { s += M(i,j)*v(j); j += 1 }
       w(i) =  s
       i += 1
      }
    }


    val u,v,w = Array.fill(n)(AffineFloat(1.0))

    var i = 0
    while (i < 10) {
      // Multiply by matrix & transpose
      mult(u,w,A)
      mult(w,v,At)
      mult(v,w,A)
      mult(w,u,At)
      i += 1
    }

    var vbv,vv = AffineFloat(0.0)
    i = 0
    while (i < n) {
      vbv += u(i)*v(i)
      vv += v(i)*v(i)
      i += 1
    }

    sqrt(vbv/vv)
  }


  def approximateSmart(n: Int): SmartFloat = {
    import SmartFloat._

    // Ordinary and transposed versions of infinite matrix
    val A = (i: Int, j: Int) => SmartFloat(1.0)/((i+j)*(i+j+1)/2 +i+1)
    val At = (j: Int, i: Int) => SmartFloat(1.0)/((i+j)*(i+j+1)/2 +i+1)

    // Matrix multiplication w <- M*v
    def mult(v: Array[SmartFloat], w: Array[SmartFloat], M: (Int,Int)=> SmartFloat ) {
      var i = 0
      while (i < n) {
       var s = SmartFloat(0.0)
       var j = 0
       while (j < n) { s += M(i,j)*v(j); j += 1 }
       w(i) =  s
       i += 1
      }
    }

    val u,v,w = Array.fill(n)(SmartFloat(1.0))

    var i = 0
    while (i < 10) {
      // Multiply by matrix & transpose
      mult(u,w,A)
      mult(w,v,At)
      mult(v,w,A)
      mult(w,u,At)
      i += 1
    }

    var vbv,vv = SmartFloat(0.0)
    i = 0
    while (i < n) {
      vbv += u(i)*v(i)
      vv += v(i)*v(i)
      i += 1
    }

    sqrt(vbv/vv)
  }

  def approximateInterval(n: Int): IntervalFloat = {
    import IntervalFloat._

    // Ordinary and transposed versions of infinite matrix
    val A = (i: Int, j: Int) => IntervalFloat(1.0)/((i+j)*(i+j+1)/2 +i+1)
    val At = (j: Int, i: Int) => IntervalFloat(1.0)/((i+j)*(i+j+1)/2 +i+1)

    // Matrix multiplication w <- M*v
    def mult(v: Array[IntervalFloat], w: Array[IntervalFloat], M: (Int,Int)=> IntervalFloat ) {
      var i = 0
      while (i < n) {
       var s = IntervalFloat(0.0)
       var j = 0
       while (j < n) { s += M(i,j)*v(j); j += 1 }
       w(i) =  s
       i += 1
      }
    }

    val u,v,w = Array.fill(n)(IntervalFloat(1.0))

    var i = 0
    while (i < 10) {
      // Multiply by matrix & transpose
      mult(u,w,A)
      mult(w,v,At)
      mult(v,w,A)
      mult(w,u,At)
      i += 1
    }

    var vbv,vv = IntervalFloat(0.0)
    i = 0
    while (i < n) {
      vbv += u(i)*v(i)
      vv += v(i)*v(i)
      i += 1
    }

    sqrt(vbv/vv)
  }
}

