package ceres

import scala.annotation.strictfp

/**
 * This is a translation from the Java version by M. Davis. DoubleDouble.java.
 * http://tsusiatsoftware.net/dd/main.html.
 * The algorithms used here are from D. M. Priest.
 * Algorithms for Arbitrary Precision Floating
 * Point Arithmetic. In Proceedings of the 10th Symposium on
 * Computer Arithmetic, 1991.
 * Implementation is according to M. Davis. DoubleDouble.java.
 * http://tsusiatsoftware.net/dd/main.html.
 */
@strictfp
object DDouble {
  val PI = Array(3.141592653589793116e+00, 1.224646799147353207e-16)
	val TWO_PI = Array(6.283185307179586232e+00, 2.449293598294706414e-16)
	val PI_2 = Array(1.570796326794896558e+00, 6.123233995736766036e-17)
	val E = Array(2.718281828459045091e+00, 1.445646891729250158e-16)

	val NaN = Array(Double.NaN, Double.NaN)

	//public static final double EPS = 1.23259516440783e-32;  /* = 2^-106 */
	val eps = scala.math.pow(2, -105);  //2^-105
	val ulp = scala.math.pow(2, -53.0);

	val zero = Array(0.0, 0.0);
	val one = Array(1.0, 0.0);
  val _one = Array(-1.0, 0.0);

	private val nan = Array(Double.NaN, Double.NaN);

	/**
	 * The value to split a double-precision value on during multiplication
	 */
	private val SPLIT = 134217729.0D; // 2^27+1, for IEEE double

	final def add(x: Array[Double], y: Array[Double]): Array[Double] = {
	  return add(x(0), x(1), y(0), y(1))
	}

  final def addUp(x: Array[Double], y: Array[Double]): Array[Double] = {
	  return addUp(x(0), x(1), y(0), y(1))
	}

	final def addDown(x: Array[Double], y: Array[Double]): Array[Double] = {
	  return addDown(x(0), x(1), y(0), y(1))
	}
	//----------------------------------

	final def sub(x: Array[Double], y: Array[Double]): Array[Double] = {
	  return sub(x(0), x(1), y(0), y(1))
	}

  final def subUp(x: Array[Double], y: Array[Double]): Array[Double] = {
	  return subUp(x(0), x(1), y(0), y(1))
	}

	final def subDown(x: Array[Double], y: Array[Double]): Array[Double] = {
	  return subDown(x(0), x(1), y(0), y(1))
	}


	//-----------------------------------

	final def mult(x: Array[Double], y: Array[Double]): Array[Double] = {
	  return mult(x(0), x(1), y(0), y(1));
	}

  final def multUp(x: Array[Double], y: Array[Double]): Array[Double] = {
	  return multUp(x(0), x(1), y(0), y(1));
	}

	final def multDown(x: Array[Double], y: Array[Double]): Array[Double] = {
	  return multDown(x(0), x(1), y(0), y(1));
	}

	//-----------------------------------

	final def div(x: Array[Double], y: Array[Double]): Array[Double] = {
	  return div(x(0), x(1), y(0), y(1));
	}

  final def divUp(x: Array[Double], y: Array[Double]): Array[Double] = {
	  return divUp(x(0), x(1), y(0), y(1));
	}

	final def divDown(x: Array[Double], y: Array[Double]): Array[Double] = {
	  return divDown(x(0), x(1), y(0), y(1));
	}


	//-----------------------------------

	final def sqrt(x: Array[Double]): Array[Double] = {
	  return sqrt(x(0), x(1))
	}

  final def sqrtUp(x: Array[Double]): Array[Double] = {
	  return sqrtUp(x(0), x(1))
	}

	final def sqrtDown(x: Array[Double]): Array[Double] = {
	  return sqrtDown(x(0), x(1))
	}

	final def abs(x: Array[Double]): Array[Double] = {
	  return abs(x(0), x(1))
	}


	//x + y   TODO: test for NaN
	final def add(xhi: Double, xlo: Double, yhi: Double, ylo: Double): Array[Double] = {
		var H, h, T, t, S, s, e, f = 0.0;
  	S = xhi + yhi;
  	T = xlo + ylo;
  	e = S - xhi;
  	f = T - xlo;
  	s = S-e;
  	t = T-f;
  	s = (yhi-e)+(xhi-s);
  	t = (ylo-f)+(xlo-t);
  	e = s+T; H = S+e; h = e+(S-H); e = t+h;

    val zhi = H + e;
  	val res = Array(H + e, e + (H - zhi));
  	return res;
	}

	final def addUp(xhi: Double, xlo: Double, yhi: Double, ylo: Double): Array[Double] = {
		var H, h, T, t, S, s, e, f = 0.0;
  	S = xhi + yhi;
  	T = xlo + ylo;
  	e = S - xhi;
  	f = T - xlo;
  	s = S-e;
  	t = T-f;
  	s = (yhi-e)+(xhi-s);
  	t = (ylo-f)+(xlo-t);
  	e = s+T; H = S+e; h = e+(S-H); e = t+h;

    val zhi = H + e;
    //zlo = e + (H - zhi)
    val zlo = DirectedRounding.addUp(e, DirectedRounding.subUp(H, zhi));
  	val res = Array(H + e, zlo);
  	return res;
	}

	final def addDown(xhi: Double, xlo: Double, yhi: Double, ylo: Double): Array[Double] = {
		var H, h, T, t, S, s, e, f = 0.0;
  	S = xhi + yhi;
  	T = xlo + ylo;
  	e = S - xhi;
  	f = T - xlo;
  	s = S-e;
  	t = T-f;
  	s = (yhi-e)+(xhi-s);
  	t = (ylo-f)+(xlo-t);
  	e = s+T; H = S+e; h = e+(S-H); e = t+h;

    val zhi = H + e;
    //zlo = e + (H - zhi)
    val zlo = DirectedRounding.addDown(e, DirectedRounding.subDown(H, zhi));
  	val res = Array(H + e, zlo);
  	return res;
	}

	//  x - y
	final def sub(xhi: Double, xlo: Double, yhi: Double, ylo: Double): Array[Double] = {
    return add(xhi, xlo, -yhi, -ylo);
  }

  final def subUp(xhi: Double, xlo: Double, yhi: Double, ylo: Double): Array[Double] = {
    return addUp(xhi, xlo, -yhi, -ylo);
  }

  final def subDown(xhi: Double, xlo: Double, yhi: Double, ylo: Double): Array[Double] = {
    return addDown(xhi, xlo, -yhi, -ylo);
  }


  // x * y TODO: test for NaN ( isNan(xhi) || isNan(yhi) )
  final def mult(xhi: Double, xlo: Double, yhi: Double, ylo: Double): Array[Double] = {
    var hx, tx, hy, ty, C, c = 0.0;
	  C = SPLIT * xhi;
	  hx = C-xhi;
	  c = SPLIT * yhi;
	  hx = C-hx;
	  tx = xhi-hx;
	  hy = c-yhi;
	  C = xhi*yhi;
	  hy = c-hy;
	  ty = yhi-hy;
	  c = ((((hx*hy-C) + hx * ty) + tx*hy) + tx*ty) + (xhi*ylo + xlo*yhi);
	  val zhi = C+c; hx = C-zhi;
	  //double zlo = c+hx;

	  val res = Array(zhi, c+hx);

	  //hi = zhi;
	  //lo = zlo;
	  return res;
  }

  final def multUp(xhi: Double, xlo: Double, yhi: Double, ylo: Double): Array[Double] = {
    var hx, tx, hy, ty, C, c = 0.0;
	  C = SPLIT * xhi;
	  hx = C-xhi;
	  c = SPLIT * yhi;
	  hx = C-hx;
	  tx = xhi-hx;
	  hy = c-yhi;
	  C = xhi*yhi;
	  hy = c-hy;
	  ty = yhi-hy;
	  c = ((((hx*hy-C) + hx * ty) + tx*hy) + tx*ty) + (xhi*ylo + xlo*yhi);
	  val zhi = C+c; hx = C-zhi;
	  //double zlo = c+hx;
	  val zlo = DirectedRounding.addUp(c, hx);
	  val res = Array(zhi, zlo);

	  //hi = zhi;
	  //lo = zlo;
	  return res;
  }

  final def multDown(xhi: Double, xlo: Double, yhi: Double, ylo: Double): Array[Double] = {
    var hx, tx, hy, ty, C, c = 0.0;
	  C = SPLIT * xhi;
	  hx = C-xhi;
	  c = SPLIT * yhi;
	  hx = C-hx;
	  tx = xhi-hx;
	  hy = c-yhi;
	  C = xhi*yhi;
	  hy = c-hy;
	  ty = yhi-hy;
	  c = ((((hx*hy-C) + hx * ty) + tx*hy) + tx*ty) + (xhi*ylo + xlo*yhi);
	  val zhi = C+c; hx = C-zhi;
	  //double zlo = c+hx;
	  val zlo = DirectedRounding.addDown(c, hx);
	  val res = Array(zhi, zlo);

	  //hi = zhi;
	  //lo = zlo;
	  return res;
  }

  // x / y  TODO: test for NaN
  final def div(xhi: Double, xlo: Double, yhi: Double, ylo: Double): Array[Double] = {
    var hc, tc, hy, ty, C, c, U, u = 0.0;
	  C = xhi/yhi;
	  c = SPLIT*C;
	  hc =c-C;
	  u = SPLIT * yhi;
	  hc = c-hc;
	  tc = C-hc;
	  hy = u-yhi;
	  U = C * yhi;
	  hy = u-hy;
	  ty = yhi-hy;
	  u = (((hc*hy-U) + hc*ty) + tc*hy) + tc*ty;
	  c = ((((xhi-U) - u) + xlo) - C * ylo) / yhi;
	  u = C+c;

	  val zhi = u;
	  val zlo = (C-u)+c;
	  val res = Array(u, (C-u)+c);
	  return res;
  }

  final def divUp(xhi: Double, xlo: Double, yhi: Double, ylo: Double): Array[Double] = {
    var hc, tc, hy, ty, C, c, U, u = 0.0;
	  C = xhi/yhi;
	  c = SPLIT*C;
	  hc =c-C;
	  u = SPLIT * yhi;
	  hc = c-hc;
	  tc = C-hc;
	  hy = u-yhi;
	  U = C * yhi;
	  hy = u-hy;
	  ty = yhi-hy;
	  u = (((hc*hy-U) + hc*ty) + tc*hy) + tc*ty;
	  c = ((((xhi-U) - u) + xlo) - C * ylo) / yhi;
	  u = C+c;

	  val zhi = u;
	  //zlo = (C-u)+c;
	  val zlo = DirectedRounding.addUp(DirectedRounding.subUp(C,u), c);
	  val res = Array(u, zlo);
	  return res;
  }

  final def divDown(xhi: Double, xlo: Double, yhi: Double, ylo: Double): Array[Double] = {
    var hc, tc, hy, ty, C, c, U, u = 0.0;
	  C = xhi/yhi;
	  c = SPLIT*C;
	  hc =c-C;
	  u = SPLIT * yhi;
	  hc = c-hc;
	  tc = C-hc;
	  hy = u-yhi;
	  U = C * yhi;
	  hy = u-hy;
	  ty = yhi-hy;
	  u = (((hc*hy-U) + hc*ty) + tc*hy) + tc*ty;
	  c = ((((xhi-U) - u) + xlo) - C * ylo) / yhi;
	  u = C+c;

	  val zhi = u;
	  //zlo = (C-u)+c;
	  val zlo = DirectedRounding.addDown(DirectedRounding.subDown(C,u), c);
	  val res = Array(u, zlo);
	  return res;
  }

  //sqrt(x)  TODO: test for NaN
  final def sqrt(xhi: Double, xlo: Double): Array[Double] = {
    /*[comment from DoubleDouble.java] Strategy:  Use Karp's trick:  if x is an approximation
    to sqrt(a), then sqrt(a) = a*x + [a - (a*x)^2] * x / 2   (approx)
    The approximation is accurate to twice the accuracy of x.
    Also, the multiplication (a*x) and [-]*x can be done with only half the precision.
    */

		if (xhi == 0.0 && xlo == 0.0) return zero;

	  if (xhi < 0.0 || (xhi == 0.0 && xlo < 0.0)) return nan;

	  val x = 1.0 / scala.math.sqrt(xhi);
	  val ax = xhi * x;

	  //double[] axdd = {ax, 0.0}; //new DoubleDouble(ax);
	  val axddSqr = mult(ax, 0.0, ax, 0.0);
	  val diffSq = sub(xhi, xlo, axddSqr(0), axddSqr(1));  //this.subtract(axdd.sqr());
	  val d2 = diffSq(0) * (x * 0.5);     //diffSq.hi * (x * 0.5);

	  val res = add(ax, 0.0, d2, 0.0);  //return axdd.add(new DoubleDouble(d2));
	  return res;
  }

  final def sqrtUp(xhi: Double, xlo: Double): Array[Double] = {
    if (xhi == 0.0 && xlo == 0.0) return zero;

	  if (xhi < 0.0 || (xhi == 0.0 && xlo < 0.0)) return nan;

	  val x = 1.0 / scala.math.sqrt(xhi);
	  val ax = xhi * x;

	  //double[] axdd = {ax, 0.0}; //new DoubleDouble(ax);
	  val axddSqr = mult(ax, 0.0, ax, 0.0);
	  val diffSq = sub(xhi, xlo, axddSqr(0), axddSqr(1));  //this.subtract(axdd.sqr());
	  val d2 = diffSq(0) * (x * 0.5);     //diffSq.hi * (x * 0.5);

	  val res = addUp(ax, 0.0, d2, 0.0);  //return axdd.add(new DoubleDouble(d2));
	  return res;
  }

  final def sqrtDown(xhi: Double, xlo: Double): Array[Double] = {
    if (xhi == 0.0 && xlo == 0.0) return zero;

	  if (xhi < 0.0 || (xhi == 0.0 && xlo < 0.0)) return nan;

	  val x = 1.0 / scala.math.sqrt(xhi);
	  val ax = xhi * x;

	  //double[] axdd = {ax, 0.0}; //new DoubleDouble(ax);
	  val axddSqr = mult(ax, 0.0, ax, 0.0);
	  val diffSq = sub(xhi, xlo, axddSqr(0), axddSqr(1));  //this.subtract(axdd.sqr());
	  val d2 = diffSq(0) * (x * 0.5);     //diffSq.hi * (x * 0.5);

	  val res = addDown(ax, 0.0, d2, 0.0);  //return axdd.add(new DoubleDouble(d2));
	  return res;
  }

  final def abs(xhi: Double, xlo: Double): Array[Double] = {
    if (xhi != xhi) return nan;

		if (xhi < 0.0 || (xhi == 0.0 && xlo < 0.0)) {
		  val res = Array(-xhi, -xlo);
		  return res;
			//return negate();
	  }
	  else {
	    val res = Array(xhi, xlo);
		  return res;
		}
		//return new DoubleDouble(this);
  }

  final def floor(xhi: Double, xlo: Double): Array[Double] = {
    if (xhi != xhi) return nan;
	  val fhi= scala.math.floor(xhi);
	  var flo = 0.0;

	  // Hi is already integral.  Floor the low word
	  if (fhi == xhi) {
	  	flo = scala.math.floor(xlo);
	  }
	  val res = Array(fhi, flo);
	  return res;
	  	// FIXME do we need to renormalize here?
	  //return new DoubleDouble(fhi, flo);

  }

  final def min(x: Array[Double], y: Array[Double]): Array[Double] = {
    if((x(0) < y(0)) || (x(0) == y(0) && x(1) < y(1)))
      return x;
    else
      return y;
  }

  final def max(x: Array[Double], y: Array[Double]): Array[Double] = {
    if((x(0) < y(0)) || (x(0) == y(0) && x(1) < y(1)))
      return y;
    else
      return x;
  }

  final def rdOff(x: Array[Double]): Array[Double] = {
    return mult(eps, 0.0, x(0), x(1));
  }

  final def rdOffAtimesBplusC(a: Array[Double], b: Array[Double], c: Array[Double]): Array[Double] = {
    val z0 = add(mult(a, b), c);
    val zmin = addDown(multDown(a,b), c);
    val zmax = addUp(multUp(a,b), c);
    return max( subUp(z0, zmin), subUp(zmax, z0));
  }

  final def lessEq(x: Array[Double], y: Array[Double]): Boolean = {
    if(x(0) == y(0) && x(1) == y(1))
      return true;

    if((x(0) < y(0)) || (x(0) == y(0) && x(1) < y(1)))
      return true;
    else
      return false;
  }

  final def greaterEq(x: Array[Double], y: Array[Double]): Boolean = {
    if(x(0) == y(0) && x(1) == y(1))
      return true;

    if((x(0) < y(0)) || (x(0) == y(0) && x(1) < y(1)))
      return false;
    else
      return true;
  }

  final def notZero(x: Array[Double]): Boolean = {
    return (x(0) != 0.0 || x(1) != 0.0);
  }
}
