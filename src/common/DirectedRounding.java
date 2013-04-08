package ceres.common;

import java.lang.Math;

public class DirectedRounding {

  private static final String LIB_NAME = "DirectedRounding";

  public static native double addUp(double a, double b);
  public static native double addDown(double a, double b);
  public static native double subUp(double a, double b);
  public static native double subDown(double a, double b);
  public static native double multUp(double a, double b);
  public static native double multDown(double a, double b);
  public static native double divUp(double a, double b);
  public static native double divDown(double a, double b);
  public static native double sqrtDown(double a);
  public static native double sqrtUp(double a);
  
  //convenience
  public static native double addRoundoff(double delta, double zi, double ai, double bi);
  public static native double computeZeta(double dmin, double dmax);
  public static native double computeDelta(double zeta, double dmin, double dmax);
  public static native double addRoundoffAtimesBplusC(double delta, double a, double b, double c);
  public static native double addRoundoffABplusCD(double delta, double a, double b, double c, double d);
  
  // this is just to force class loading, and therefore library loading.
  protected static void init() { }
  
  public final static double u = Math.pow(2.0, -53.0);

  public static double nextDown(double x) {
    return -Math.nextUp(-x);
  }
  
  public static double roundOff(double x) {
    return Math.nextUp(u*Math.abs(x));
  }
  
  public static double up1(double x) {
    if(x == Double.NEGATIVE_INFINITY) 
      return Double.NEGATIVE_INFINITY;
    else 
      return Math.nextUp(x + Math.ulp(x));
  }
  
 
  public static double down1(double x) {
    if(x == Double.POSITIVE_INFINITY)
      return Double.POSITIVE_INFINITY;
    else 
      return nextDown(x - Math.ulp(x));
  }

  public static double roundOff1(double x) {
    return Math.nextUp(2*(u*Math.abs(x)));
  }
  
  public static boolean isExact(double d) {
    if(d != d || d == Double.POSITIVE_INFINITY || d == Double.NEGATIVE_INFINITY || 
      d == Double.MAX_VALUE || d == Double.MIN_VALUE) return false;
    else {
      String s = Double.toString(d);  //has to be the first we do to avoid rounding
      if(!s.matches("-?[0-9.]*") || s.length() >= 9) 
        return false;
      
      s = s.substring(s.indexOf(".")+1);
      //exactly representable, if decimal parts denominator is a power of two
      if(Long.parseLong(s) % Math.pow(5, s.length()) == 0) return true;
      else return false;     
    }
  }
  
  public static String toBinary(double x) {
    return Long.toBinaryString(Double.doubleToLongBits(x));
  }
  
  static {
      try {
          System.loadLibrary(LIB_NAME);
      } catch (UnsatisfiedLinkError e) {
          System.out.println(
              "Sorry, linking error... Please check the JNI library is on the java.library.path:" +
              System.getProperty("java.library.path"));
          System.out.println(e);
      }
  }
  
    
 }
