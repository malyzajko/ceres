/*===============================================================*/
/*                                                               */
/* fbench.java             John Walker's 'fbench' - Java version */
/*                                                               */
/*               Jim White                                       */
/*               u3280625@anu.edu.au                             */
/*               mathimagics@yahoo.co.uk                         */
/*               July 2005                                       */
/*                                                               */
/*===============================================================*/
/*                                                               */
/*  This program does exactly the same calculations as fbench.c, */
/*  so it allows you to measure the performance of the Java      */
/*  Runtime "virtual machine" relative to that of native-mode    */
/*  programs when performing similar work (ie, a computationally */
/*  intensive iterative procedure using simple data types).      */
/*                                                               */
/*  The program uses standard Java primitives and classes, and   */
/*  should require no modification. The number of iterations can */
/*  be given on the command line, otherwise the default value of */
/*  10,000 is used.                                              */
/*                                                               */
/*        e.g:    java fbench 5000                               */
/*                                                               */
/*  The program reports elapsed time, and the corresponding time */
/*  inferred for 1000 iterations, which is the standard unit     */
/*  used in the results table given at John's fbench web page.   */
/*                                                               */
/*===============================================================*/
/*  Note:  refer to fbench.c for John's original comments        */
/*         describing the benchmark algorithm and the nature of  */
/*         the task being performed.                             */
/*===============================================================*/
package ceres
package benchmarks

import scala.math._

/**
This is a Scala port of fbench.
*/
object FBench {
  val INTRIG: Int = 1
  val max_surfaces: Int = 10
  val twopi = Pi * 2.0
  val piover4    = Pi / 4.0
  val fouroverpi = 4.0 / Pi
  val piover2    = Pi / 2.0

  val current_surfaces: Int = 4
  var paraxial: Short = 0

  val clear_aperture = 4.0
  var aberr_lspher = 0.0
  var aberr_osc = 0.0
  var aberr_lchrom = 0.0
  var max_lspher = 0.0
  var max_osc = 0.0
  var max_lchrom = 0.0

  var radius_of_curvature = 0.0
  var object_distance = 0.0
  var ray_height = 0.0
  var axis_slope_angle = 0.0
  var from_index = 0.0
  var to_index = 0.0

  val spectral_line = Array.fill(9){0.0}
  val s = Array.fill(max_surfaces, 5){0.0}
  val testcase: Array[Array[Double]] = Array(
        Array(27.05, 1.5137, 63.6, 0.52),
        Array(-16.68, 1, 0, 0.138),
        Array(-16.68, 1.6164, 36.7, 0.38),
        Array(-78.1, 1, 0, 0));

  var od_sa = Array.fill(2, 2){0.0}
  
  var itercount: Int = 0    
  val ITERATIONS: Int = 10000
  var niter: Int = ITERATIONS

  /* Reference results.  
  "   Marginal ray          47.09479120920   0.04178472683",
  "   Paraxial ray          47.08372160249   0.04177864821",
  "Longitudinal spherical aberration:        -0.01106960671",
  "    (Maximum permissible):                 0.05306749907",
  "Offense against sine condition (coma):     0.00008954761",
  "    (Maximum permissible):                 0.00250000000",
  "Axial chromatic aberration:                0.00448229032",
  "    (Maximum permissible):                 0.05306749907" 
  */

  def cot(x: Double): Double = 1.0 / tan(x)
   
  def initialise() = {
      spectral_line(1) = 7621.0       // A 
      spectral_line(2) = 6869.955     // B 
      spectral_line(3) = 6562.816     // C 
      spectral_line(4) = 5895.944     // D 
      spectral_line(5) = 5269.557     // E 
      spectral_line(6) = 4861.344     // F 
      spectral_line(7) = 4340.477     // G
      spectral_line(8) = 3968.494     // H
  
      /* Load test case into working array */
      var i, j = 0
     
      for (i <- 0 until current_surfaces) {
         for (j <- 0 until 4) {
            s(i + 1)(j + 1) = testcase(i)(j)
         }
      }
   }//end initialize


  def trace_line(line: Int, ray_h: Double) = {
    var i = 0

    object_distance = 0.0
    ray_height = ray_h
    from_index = 1.0

    for (i <- 1 until (current_surfaces + 1)) {
      radius_of_curvature = s(i)(1)
      to_index = s(i)(2)
    
      if (to_index > 1.0) {
        to_index = to_index + ((spectral_line(4) - spectral_line(line)) / (spectral_line(3) - 
          spectral_line(6))) * ((s(i)(2) - 1.0) / s(i)(3))
      }
      
      transit_surface
      from_index = to_index

      if (i < current_surfaces) {
        object_distance = object_distance - s(i)(4)
      }
    }
  }//end trace line
  
  def transit_surface: Unit = {
    var iang,     /* Incidence angle */
        rang,     /* Refraction angle */
        iang_sin,    /* Incidence angle sin */
        rang_sin,    /* Refraction angle sin */
        old_axis_slope_angle, sagitta = 0.0;

    if (paraxial != 0) {
      if (radius_of_curvature != 0.0) {
        if (object_distance == 0.0) {
          axis_slope_angle = 0.0
          iang_sin = ray_height / radius_of_curvature
        } 
        else {
          iang_sin = ((object_distance - radius_of_curvature) / 
            radius_of_curvature) * axis_slope_angle
        }
        
        rang_sin = (from_index / to_index) * iang_sin
        old_axis_slope_angle = axis_slope_angle
        axis_slope_angle = axis_slope_angle +  iang_sin - rang_sin
        
        if (object_distance != 0.0) { 
          ray_height = object_distance * old_axis_slope_angle
        }
          
        object_distance = ray_height / axis_slope_angle
        return
      }
      
      object_distance = object_distance * (to_index / from_index)
      axis_slope_angle = axis_slope_angle * (from_index / to_index)
      return
    }

    if (radius_of_curvature != 0.0) {
      if (object_distance == 0.0) {
        axis_slope_angle = 0.0
        iang_sin = ray_height / radius_of_curvature
      }
      else {
          iang_sin = ((object_distance -  radius_of_curvature) / radius_of_curvature) * 
            sin(axis_slope_angle)
      }
      
      iang = asin(iang_sin)
      rang_sin = (from_index / to_index) *   iang_sin
      old_axis_slope_angle = axis_slope_angle
      axis_slope_angle = axis_slope_angle + iang - asin(rang_sin)
      sagitta = sin((old_axis_slope_angle + iang) / 2.0)
      sagitta = 2.0 * radius_of_curvature*sagitta*sagitta
      object_distance = ((radius_of_curvature * sin(old_axis_slope_angle + iang)) * 
        cot(axis_slope_angle)) + sagitta
      return
    }

    rang = -asin((from_index / to_index) * sin(axis_slope_angle))
    object_distance = object_distance * ((to_index * cos(-rang)) / 
        (from_index * cos(axis_slope_angle)))
    axis_slope_angle = -rang
  }//end transit_surface


  //@return average runtime per iteration
  def run(niter: Int): Long = {

    //Date    Now;
    var  time = 0L
    var  od_cline, od_fline = 0.0
    var runtime = 0.0
    
    initialise();
              
    //print("\nReady to begin John Walker's floating point accuracy\n")
    //printf("and performance benchmark.  %d iterations will be made.\n\n", niter)
    //printf("\nBegin ... ")

    //Now = new Date(); 
    //time = Now.getTime();
    time = System.currentTimeMillis

    for (itercount <- 0 until niter) {
      for (paraxial <- 0 until 2) {
        /* Do main trace in D light */
        trace_line(4, clear_aperture / 2.0)
        od_sa(paraxial)(0) = object_distance
        od_sa(paraxial)(1) = axis_slope_angle
      }
      paraxial = 0

      /* Trace marginal ray in C */

      trace_line(3, clear_aperture / 2.0)
      od_cline = object_distance

      /* Trace marginal ray in F */

      trace_line(6, clear_aperture / 2.0)
      od_fline = object_distance

      aberr_lspher = od_sa(1)(0) - od_sa(0)(0)
      aberr_osc = 1.0 - (od_sa(1)(0) * od_sa(1)(1)) / (sin(od_sa(0)(1)) * od_sa(0)(0))
      aberr_lchrom = od_fline - od_cline
      max_lspher = sin(od_sa(0)(1))

      /* D light */

      max_lspher = 0.0000926 / (max_lspher * max_lspher)
      max_osc = 0.0025
      max_lchrom = max_lspher
    }

    //Now = new Date();  time = Now.getTime() - time;
    time = System.currentTimeMillis - time;
    //printf("time taken %d ms\n", time)
    /* Now evaluate the accuracy of the results from the last ray trace */
    /*
    println("Results")
    println("Marginal ray      %21.11f  %14.11f ", od_sa(0)(0), od_sa(0)(1))
    println("Paraxial ray      %21.11f  %14.11f ", od_sa(1)(0), od_sa(1)(1))
    println("Longitudinal spherical aberration:      %16.11f ", aberr_lspher)
    println("    (Maximum permissible):              %16.11f ", max_lspher)
    println("Offense against sine condition (coma):  %16.11f ", aberr_osc)
    println("    (Maximum permissible):              %16.11f ", max_osc)
    println("Axial chromatic aberration:             %16.11f ", aberr_lchrom)
    println("    (Maximum permissible):              %16.11f ", max_lchrom)
    */
    //printf("Finished, clock =  %d\n", time)*/
    /* note that time unit is msecs, so the following calculation is correct */
    //printf("Time for 1000 iterations = %8.4f\n",(time / niter.toDouble)) 
    return time
    
    //runtime = (time / niter.toDouble)
    //return runtime
  }//end run


}
