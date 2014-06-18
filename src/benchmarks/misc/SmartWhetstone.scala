/*
 *  Document:         Whets.java 
 *  File Group:       Classic Benchmarks
 *  Creation Date:    22 January 1997
 *  Revision Date:    
 *
 *  Title:            Whetstone Benchmark Java Version
 *  Keywords:         WHETSTONE BENCHMARK PERFORMANCE MIPS
 *                    MWIPS MFLOPS JAVA
 *
 *  Abstract:         Web/Java version of Whetstone one of the
 *                    Classic Numeric Benchmarks.        
 *
 *  Contributor:      Roy_Longbottom@compuserve.com
 *
 *           Copyright (C) 1997, Roy Longbottom
 *
 ************************************************************
 *
 *          Java Whetstone Benchmark Single Precision
 *
 *     Original concept        Brian Wichmann NPL      1960's
 *     Original author         Harold Curnow  CCTA     1972
 *     Self timing versions    Roy Longbottom CCTA     1978/87
 *     Optimisation control    Bangor University       1987/90
 *     C/C++ and PC Versions   Roy Longbottom          1996
 *     Java version            Roy Longbottom          1997
 *
 ************************************************************
 *
 *     The program normally runs for about 100 seconds
 *     (adjustable duration in initial parameters). This time
 *     is necessary because of poor PC clock resolution.
 *            
 ************************************************************
 *
 *     Descriptions of history and changes in the benchmark
 *     can be found in WHETS.C and WHETSOLD.TXT available
 *     in the Classics Benchmark Library at the CompuServe
 *     Benchmark and Standards Forum.
 *
 *     Changes for this version are described in WHETS.HTML.
 *     This is required for driving WHETS.CLASS as produced
 *     when this program is compiled.
 *
 *     Examples of numeric results and performance on various
 *     PCs are also given in WHETS.HTML. The latter can also
 *     be obtained from the Benchmark and Standards Forum.
 *
 *              Roy_Longbottom@compuserve.com
 *
 ************************************************************
 */
package ceres
package benchmarks

import smartfloat.SmartFloat 
import SmartFloat._

/**
The actual benchmark codes have not been changed, but we have adjusted
the variable management a little bit to the way Scala does it.
Some variables have been replaced by 'magic numbers'.
Also, we are only interested in the plain running times, hence all flops etc.
calculations have been removed.
*/
object SmartWhetstone {

  private val e1: Array[SmartFloat] = Array.fill(4){0.0}
  private var routine = 0
  private val x100 = 100
  
  private var t =  SmartFloat(0.49999975)
  private val t0 = t        
  private val t1 = SmartFloat(0.50000025)
	private val t2 = SmartFloat(2.0)
	
	/*private val n1 = 12*x100;
	private val	n2 = 14*x100;
	private val	n3 = 345*x100;
	private val	n4 = 210*x100;
	private val	n5 = 32*x100;
	private val	n6 = 899*x100;
	private val	n7 = 616*x100;
	private val	n8 = 93*x100;
	*/
	private val n1 = 1
	private val n2 = 1
	private val n3 = 1
	private val n4 = 1
	private val n5 = 1
	private val n6 = 1
	private val n7 = 1
	private val n8 = 1
	
  private var x = SmartFloat(0.0)
  private var y = SmartFloat(0.0)
  private var z = SmartFloat(0.0)
  
  /* Section 1, Array elements */
  def section1(xtra: Int) = {
    e1(0) =  1.0
		e1(1) = -1.0
		e1(2) = -1.0
		e1(3) = -1.0			 
		
		//val timea = System.currentTimeMillis
		
		for (ix <- 0 until xtra) {
			for(i <- 0 until (n1 * 10)) {
				e1(0) = (e1(0) + e1(1) + e1(2) - e1(3)) * t
				e1(1) = (e1(0) + e1(1) - e1(2) + e1(3)) * t
				e1(2) = (e1(0) - e1(1) + e1(2) + e1(3)) * t
				e1(3) = (-e1(0) + e1(1) + e1(2) + e1(3)) * t
			}
			t = 1.0 - t
		}
		t =  t0                   
		
		//val timeb = System.currentTimeMillis
		//println("Sectio 1 " + (timeb - timea) + " ms")
  }//end section1
  
  /* Section 2, Array as parameter */
  def section2(xtra: Int) = {
    //val timea = System.currentTimeMillis
			
		for (ix <- 0 until xtra) { 
			for(i <- 0 until n2)	{
				 pa(e1,t,t2);
			 }
			t = 1.0 - t;
		}
		t =  t0;
	
		//val timeb = System.currentTimeMillis
		//println("Sectio 2 " + (timeb - timea) + " ms")
  }//end section2
  
  private def pa(e: Array[SmartFloat], t: SmartFloat, t2: SmartFloat) {
    for(j <- 0 until 6) {
      e1(0) = (e1(0) + e1(1) + e1(2) - e1(3)) * t
		  e1(1) = (e1(0) + e1(1) - e1(2) + e1(3)) * t
			e1(2) = (e1(0) - e1(1) + e1(2) + e1(3)) * t
			e1(3) = (-e1(0) + e1(1) + e1(2) + e1(3))/t2;
    }
    return;
  }
  
  
  /* Section 3, Conditional jumps */
  def section3(xtra: Int) = {
    var j = 1
		//val timea = System.currentTimeMillis
		for (ix <- 0 until xtra) {
		  for(i <- 0 until n3) {
				if(j==1) j = 2;
				else j = 3;
				
				if(j>2) j = 0;
				else  j = 1;
        
        if(j<1) j = 1;
        else j = 0;
		  }
		}
		//val timeb = System.currentTimeMillis
    //println("Sectio 3 " + (timeb - timea) + " ms")
  }//def section3
  
  /* Section 4, Integer arithmetic */
  def section4(xtra: Int) = {
    var j = 1
	  var k = 2
		var l = 3
	  //val timea = System.currentTimeMillis
		for (ix <- 0 until xtra) {
		  for(i <- 0 until n4) {
				j = j *(k-j)*(l-k)
				k = l * k - (l-j) * k
        l = (l-k) * (k+j)
				e1(l-2) = j + k + l
				e1(k-2) = j * k * l
			}
		}
		//val timeb = System.currentTimeMillis
		//println("Section 4 " + (timeb - timea) + " ms")
  }//end section4
  
  
  /* Section 5, Trig functions */
  def section5(xtra: Int) = {
    x = 0.5
		y = 0.5
		//val timea = System.currentTimeMillis
		for (ix <- 0 until xtra) {
			for(i <- 1 until n5) {
			  x = t * atan(t2 * sin(x) * cos(x)/
					 (cos(x+y) + cos(x-y) - 1.0))
				y = t * atan(t2 * sin(y) * cos(y)/
					 (cos(x+y) + cos(x-y)-1.0))
			}
			t = 1.0 - t
		}
		t = t0
		//val timeb = System.currentTimeMillis
		//println("Section 5 " + (timeb - timea) + " ms")
  }//end section5
  
  /* Section 6, Procedure calls */
  def section6(xtra: Int) = {
    x = 1.0
		y = 1.0
		z = 1.0
	  //val timea = System.currentTimeMillis
		for (ix <- 0 until xtra) {
		  for(i <- 0 until n6)	{
		    p3(t,t1,t2)
			}
		}
		//val timeb = System.currentTimeMillis
		//println("Section 6 " + (timeb - timea) + " ms")
  }//end section6
  
  private def p3(t: SmartFloat, t1: SmartFloat, t2: SmartFloat) {
    x = y
    y = z
    x = t * (x + y)
    y = t1 * (x + y)
    z = (x + y)/t2
    return
  }
     
  /* Section 7, Array refrences */
  def section7(xtra: Int) = {
    var j = 0
	  var k = 1
		var l = 2
		e1(0) = 1.0
		e1(1) = 2.0
		e1(2) = 3.0
		//val timea = System.currentTimeMillis
		for (ix <- 0 until xtra) {
  		for(i <- 0 until n7) {
			  po(e1,j,k,l);
			}
		}
		//val timeb = System.currentTimeMillis
    //println("Section 7 " + (timeb - timea) + " ms")
  }//end section7
  
  def po(e1: Array[SmartFloat], j: Int, k: Int, l: Int) {
    e1(j) = e1(k)
    e1(k) = e1(l)
    e1(l) = e1(j)
    return
  }
 
  /* Section 8, Standard functions */
  def section8(xtra: Int) = {
    x = 0.75
	  //val timea = System.currentTimeMillis
		for (ix <- 0 until xtra) {
		  for(i <- 0 until n8)	{
				x = sqrt(exp(log(x)/t1))
			}
		}
		//val timeb = System.currentTimeMillis
	  //println("Section 8 " + (timeb - timea) + " ms")
  }//end section8
 
  def whetstonesNoTiming(xtra: Int)= () => {
    var sum = SmartFloat(0.0)
	  section1(xtra)
		section2(xtra)
		//section3(xtra)
		//section4(xtra)
		section5(xtra)
		section6(xtra)
		section7(xtra)
		section8(xtra)
		sum
	}
 
 
 
  def whetstones(xtra: Int): Long = {
    val timea = System.currentTimeMillis
	  section1(xtra)
		section2(xtra)
		//section3(xtra)
		//section4(xtra)
		section5(xtra)
		section6(xtra)
		section7(xtra)
		section8(xtra)
		val timeb = System.currentTimeMillis
		//println("whetstones " + (timeb - timea) + " ms")
		return (timeb - timea)
	}
	

}
