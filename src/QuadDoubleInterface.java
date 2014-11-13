package ceres;

public class QuadDoubleInterface {

  // TODO:
  // To ensure that arithmetic is carried out with proper precision and accuracy, one
  // must call the function fpu_fix_start before performing any double-double or quad-double
  // arithmetic. This forces all arithmetic to be carried out in 64-bit double precision,
  // not the 80-bit precision that is found on certain compilers and interferes with the
  //existing library.
  // We should probably also return the settings back...
  // Do this in the main program...

  public static native long fpuFixStart();
  public static native void fpuFixEnd(long i);


  // FIXME: Need to make sure we don't to the sloppy one
  public static native double[] add(double x0, double x1, double x2, double x3,
    double y0, double y1, double y2, double y3);

  public static native double[] sub(double x0, double x1, double x2, double x3,
    double y0, double y1, double y2, double y3);

  public static native double[] mult(double x0, double x1, double x2, double x3,
    double y0, double y1, double y2, double y3);

  public static native double[] div(double x0, double x1, double x2, double x3,
    double y0, double y1, double y2, double y3);


  public static native double[] sqrt(double x0, double x1, double x2, double x3);
  public static native double[] log(double x0, double x1, double x2, double x3);
  public static native double[] exp(double x0, double x1, double x2, double x3);
  public static native double[] pow(double x0, double x1, double x2, double x3,
      double y0, double y1, double y2, double y3);
  public static native double[] cos(double x0, double x1, double x2, double x3);
  public static native double[] sin(double x0, double x1, double x2, double x3);
  public static native double[] tan(double x0, double x1, double x2, double x3);
  public static native double[] acos(double x0, double x1, double x2, double x3);
  public static native double[] asin(double x0, double x1, double x2, double x3);
  public static native double[] atan(double x0, double x1, double x2, double x3);


  public static native String toString(int digits, double x0, double x1, double x2, double x3);


  static {
    try {
      System.loadLibrary("QuadDouble");
      //System.out.println("JNI library successfully loaded");
    } catch (UnsatisfiedLinkError e) {
      System.out.println("Linking error, cannot find QuadDouble JNI library.");
      System.out.println(e);
      e.printStackTrace();
    }
  }
}
