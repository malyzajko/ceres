package ceres
package benchmarks
package astro

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Scanner;

import scala.io.Source

import ceres.common.{QuadDouble => QD}
import smartfloat.{AffineFloat => AF, IntervalFloat => IF, SmartFloat => SF}

object MoonPositionTest extends App {

  //quadDoublePosition
  affinePosition
  //intervalPosition
  //smartPosition

/*  test("moon position at 20120210") {
    println("\nDouble")
    //val res = Moon.moonPosition(2012, 3, 14, 0)
    val res = Moon.moonPosition(2012, 2, 10, 0)
    println(res)
  }*/

  def quadDoublePosition = {
    println("quad double moon position at 20120210")
    println("\nQuadDouble")
    //val res = Moon.moonPosition(2012, 3, 14, 0)
    var res = QDMoon.moonPosition(2012, 2, 10, 0)
    println(res)
    println(res._3.toLongString())
    println(res._6.toLongString())

    println("\nQuadDouble - precise")
    //val res = Moon.moonPosition(2012, 3, 14, 0)
    res = QDMoon2.moonPosition(2012, 2, 10, 0)
    println(res)
    println(res._3.toLongString())
    println(res._6.toLongString())

  }

  def affinePosition = {
    println("affine double moon position at 20120210")
    println("\nAffineFloat")
    //val res = Moon.moonPosition(2012, 3, 14, 0)
    var res = AFMoon.moonPosition(2012, 2, 10, 0)
    println(res._3 + "    " + res._3.interval)
    println(res._6 + "    " + res._6.interval)

    println(AF.opCounts)
    /*println("\nAffineFloat2")
    //val res = Moon.moonPosition(2012, 3, 14, 0)
    res = AFMoon2.moonPosition(2012, 2, 10, 0)
    println(res._3 + "    " + res._3.interval)
    println(res._6 + "    " + res._6.interval)
    */
  }

  def intervalPosition = {
    println("interval moon position at 20120210")
    println("\nIntervalFloat")
    //val res = Moon.moonPosition(2012, 3, 14, 0)
    var res = IFMoon.moonPosition(2012, 2, 10, 0)
    println(res._3 + "    " + res._3.interval)
    println(res._6 + "    " + res._6.interval)

    /*println("\nIntervalFloat2")
    //val res = Moon.moonPosition(2012, 3, 14, 0)
    res = IFMoon2.moonPosition(2012, 2, 10, 0)
    println(res._3 + "    " + res._3.interval)
    println(res._6 + "    " + res._6.interval)
    */
  }

  def smartPosition = {
    println("smart double moon position at 20120210")
    println("\nSmartFloat")
    //val res = Moon.moonPosition(2012, 3, 14, 0)
    var res = SFMoon.moonPosition(2012, 2, 10, 0)
    println(res._3 + "    " + res._3.interval)
    println(res._6 + "    " + res._6.interval)

  }



}



object Moon {

	val keyb = new Scanner(System.in);

	def moonPosition(y: Int, m: Int, day: Int, minutes: Int, hours: Int = 12):
    (Double, Double, Double, Double, Double, Double) = {
    val Pi = 3.141592653589793
    var year = y
    var month = m
	  println("Compute for the " + day + "-" + month +
        "-" + year + " at " + hours + ":" + minutes);

		// Compute MJD:
		if(month == 1 || month == 2){
			year -= 1;
			month += 12;
		}

		val A = (year/100.0).toInt;
		val B = 2-A+(A/4.0).toInt;
		var julianDay = (365.25*(year+4716)).toInt+(30.6001*(month+1)).toInt+day+B-1524.5;
		julianDay = julianDay+hours/24.0;

		//println("JD = " + julianDay);

		val T = (julianDay-2451545.0)/36525.0;
    //println("T: " + T);

		// Longitude average of Moon:
		var Ll = 218.3164591+481267.88134236*T-0.0013268*math.pow(T,2)+math.pow(T,3)/538841.0-
      math.pow(T,4)/65194000.0;
		Ll = Ll%360.0;
		Ll = Ll*Pi/180.0;
    //println("Ll: " + Ll);

    // Elongation average ofMmoon:
		var D = 297.8502042+445267.1115168*T-0.0016300*math.pow(T,2)+math.pow(T,3)/545868.0-
      math.pow(T,4)/113065000.0;
		D = D%360.0;
		D = D*Pi/180.0;
    //println("D: " + D);

		// Anomaly average of Sun:
		var M = 357.5291092+35999.0502909*T-0.0001536*math.pow(T,2)+math.pow(T,3)/24490000.0;
		M = M%360.0;
		M = M*Pi/180.0;
    //println("M: " + M);

		// Anomaly average of Moon:
		var Ml = 134.9634114+477198.8676313*T+0.0089970*math.pow(T,2)+math.pow(T,3)/69699.0-
      math.pow(T,4)/14712000.0;
		Ml = Ml%360.0;
		Ml = Ml*Pi/180.0;
    //println("Ml: " + Ml);

		// Argument of latitude of the Moon (Average distance of the moon of ascendant node):
		var F = 93.2720993+483202.0175273*T-0.0034029*math.pow(T,2)-math.pow(T,3)/3526000.0+
      math.pow(T,4)/863310000.0;
		F = F%360.0;
		F = F*Pi/180.0;
    //println("F: " + F);

		// 3 other arguments necessary (in degrees):
		var A1 = 119.75 + 131.849 * T;
		A1 = A1%360.0;
		A1 = A1*Pi/180.0;
//--30
		var A2 = 53.09 + 479264.290 * T;
		A2 = A2%360.0;
		A2 = A2*Pi/180.0;

		var A3 = 313.45 + 481266.484 * T;
		A3 = A3%360.0;
		A3=A3*Pi/180.0;
    //println("A1: " + A1);
    //println("A2: " + A2);
    //println("A3: " + A3);

		var ll: Array[String] = null;
		var coeffD, coeffM, coeffMl, coeffF = 0;
		var coeffSigl, coeffSigr, coeffSigb, E = 0.0;
		var sigr, sigl, sigb = 0.0;

    // Read periodic terms of longitude (Sigl) and moon's distance (Sigr):
    for(line <- Source.fromFile("src/benchmarks/moon/Moontable1.rdb").getLines().drop(3)) {
      ll = line.split("\t");

      try {
				coeffD = Integer.parseInt(ll(0));
				coeffM = Integer.parseInt(ll(1));
				coeffMl = Integer.parseInt(ll(2));
				coeffF = Integer.parseInt(ll(3));
				coeffSigl = java.lang.Double.parseDouble(ll(4));
				coeffSigr = java.lang.Double.parseDouble(ll(5));

				E = 1-0.002516*T-0.0000074*math.pow(T,2);
//--50
				if ((coeffM == 1) || (coeffM == -1)){
					sigl = sigl + E * coeffSigl * math.sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
					sigr = sigr + E * coeffSigr * math.cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}
				else if ((coeffM == 2) || (coeffM == -2)){
					sigl = sigl + math.pow(E,2) * coeffSigl * math.sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
					sigr = sigr + math.pow(E,2) * coeffSigr * math.cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}
				else {
					sigl = sigl + coeffSigl * math.sin(D*coeffD+Ml*coeffMl+F*coeffF);
					sigr = sigr + coeffSigr * math.cos(D*coeffD+Ml*coeffMl+F*coeffF);
				}

			} catch {
        case e: NumberFormatException =>
				  println("Number format exception in MoonTable1");
			}
    }
    //println("sigl: " + sigl);
    //println("sigr: " + sigr);


    for(line <- Source.fromFile("src/benchmarks/moon/Moontable2.rdb").getLines().drop(3)) {
			ll = line.split("\t");

			try {
				coeffD = Integer.parseInt(ll(0));
				coeffM = Integer.parseInt(ll(1));
				coeffMl = Integer.parseInt(ll(2));
				coeffF = Integer.parseInt(ll(3));
				coeffSigb = java.lang.Double.parseDouble(ll(4));

				E = 1.-0.002516*T-0.0000074*math.pow(T,2);

				if ((coeffM == 1) || (coeffM == -1)){
					sigb = sigb + E * coeffSigb * math.sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}

				else if ((coeffM == 2) || (coeffM == -2)){
					sigb = sigb + math.pow(E,2) * coeffSigb * math.sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}

				else {
					sigb = sigb + coeffSigb * math.sin(D*coeffD+Ml*coeffMl+F*coeffF);
				}

			} catch {
        case e: NumberFormatException =>
				  println("Number format exception in MoonTable2");
			}
    }
    //println("sigb: " + sigb);


		// Additional terms due to Jupiter, Venus and flatening of Earth:
		sigl = sigl + 3958.0*math.sin(A1)+1962.*math.sin(Ll-F)+318.0*math.sin(A2);
		sigb = sigb - 2235.0*math.sin(Ll)+382.0*math.sin(A3)+175.0*math.sin(A1-F)+
      175.0*math.sin(A1+F)+127.0*math.sin(Ll-Ml)-115.0*math.sin(Ll+Ml);
//--90
    //println("\nAfter correction by Jupiter and mates:")
    //println("sigl: " + sigl);
    //println("sigb: " + sigb);


		// Moon coordinates in degrees
		var lbda = (Ll*180/Pi + sigl/1000000.0);
		lbda = lbda%360.0;
		var beta = (sigb/1000000.0);
		beta=beta%360.0;

		// Distance Moon - center of earth (in KM):
		val GrdDelta = 385000.56 + sigr/1000.0;

		// Apparent lambda by adding to lambda the nutation in longitude:
		var L=280.4665+36000.7698*T;
		L = L%360.0;
		L = L*Pi/180.0;

		// Longitude of the mean ascending node
		var omega=125.0445550-1934.1361849*T+0.0020762*math.pow(T,2)+math.pow(T,3)/467410.0-
      math.pow(T,4)/60616000.0;
		omega = omega%360.0;
		var dphi = -17.20/3600.0*math.sin(Pi*omega/180.0)-1.32/3600.0*math.sin(2.0*L)-
      0.23/3600.0*math.sin(2.0*Ll)+0.21/3600.0*math.sin(2.0*Pi*omega/180.0);
		dphi = dphi%360.0;
		var applbda = lbda + dphi;
		applbda = applbda%360.0;

		// True obliquity of the ecliptic
		val eps0 = (23.0+26.0/60.0+21.448/3600.0)-46.8150/3600.0*T-0.00059/3600.0*math.pow(T,2)+
      0.001813/3600.0*math.pow(T,3);
		val deps = 9.20/3600.0*math.cos(Pi*omega/180.0)+0.57/3600.0*math.cos(2.0*L)+
      0.10/3600.0*math.cos(2.0*Ll)-0.09/3600.0*math.cos(2*Pi*omega/180.0);
		var eps = eps0 + deps;
		eps = eps % 360.0;

		val alpha = darctan((math.sin(applbda*Pi/180.0)*math.cos(eps*Pi/180.0)-
          math.tan(beta*Pi/180.0)*math.sin(eps*Pi/180.0)),math.cos(applbda*Pi/180.0));
		var delta = math.asin(math.sin(beta*Pi/180.0)*math.cos(eps*Pi/180.0)+
        math.cos(beta*Pi/180.0)*math.sin(eps*Pi/180.0)*math.sin(applbda*Pi/180.0));
		delta = delta*180.0/Pi;

		val halpha = (alpha/15.0).toInt;
    val mnalpha = ((alpha/15.0-halpha)*60.0).toInt;
		val salpha = ((alpha/15.0-halpha)*60.0-mnalpha)*60.0;

		if (delta>90.0){
			delta = delta-360.0;
		}

		else {
			delta = 1.0*delta;
		}
    println("\nAlpha: " + alpha)
    println("Delta: " + delta)
		val ddelta = (delta).toInt;
		val mndelta = math.abs(((delta-ddelta)*60.0).toInt);
		val sdelta = math.abs(((delta-ddelta)*60.-((delta-ddelta)*60.).toInt)*60.0);

		println("Moon coordinates:");
		println("Alpha = %dh %dm %s ".format(halpha, mnalpha, salpha.toString))
		println("Delta = %dd %dm %s ".format(ddelta, mndelta, sdelta.toString))
    return (halpha, mnalpha, salpha, ddelta, mndelta, sdelta)
	}

	def darctan(y: Double, x: Double): Double = {
    val Pi = 3.141592653589793
		if((x == 0) && (y == 0)){
			return 0.0;
		}

		else if ((y < 0.0) && (x > 0.0)){
			return (math.atan(y/x)*180.0/Pi)+360.0;
		}

		else if (x < 0.0){
			return (math.atan(y/x)*180.0/Pi)+180.0;
		}

		else {
			return math.atan(y/x)*180.0/Pi;
		}
	}

}

object QDMoon {
  import QD._

	val keyb = new Scanner(System.in);

	def moonPosition(y: Int, m: Int, day: Int, minutes: Int, hours: Int = 12):
    (QD, QD, QD, QD, QD, QD) = {
    val Pi = QD(3.141592653589793)
    var year = y
    var month = m
	  println("Compute for the " + day + "-" + month +
        "-" + year + " at " + hours + ":" + minutes);

		// Compute MJD:
		if(month == 1 || month == 2){
			year -= 1;
			month += 12;
		}

		val A = (year/100.0).toInt;
		val B = 2-A+(A/4.0).toInt;
		var julianDay: QD = (365.25*(year+4716)).toInt+(30.6001*(month+1)).toInt+day+B-QD(1524.5);
		julianDay = julianDay+QD(hours)/24.0;

		//println("JD = " + julianDay);

		val T: QD = (julianDay-2451545.0)/36525.0;
    //println("T: " + T);

		// Longitude average of Moon:
		var Ll = QD(218.3164591)+QD(481267.88134236)*T-0.0013268*pow(T, 2)+pow(T, 3)/538841.0-
      pow(T, 4)/65194000.0;
		Ll = Ll%360.0;
		Ll = Ll*Pi/180.0;
    //println("Ll: " + Ll);

    // Elongation average ofMmoon:
		var D = QD(297.8502042)+445267.1115168*T-0.0016300*pow(T, 2)+pow(T, 3)/545868.0-
      pow(T, 4)/113065000.0;
		D = D%360.0;
		D = D*Pi/180.0;
    //println("D: " + D);

		// Anomaly average of Sun:
		var M = QD(357.5291092)+35999.0502909*T-0.0001536*pow(T, 2)+pow(T, 3)/24490000.0;
		M = M%360.0;
		M = M*Pi/180.0;
    //println("M: " + M);

		// Anomaly average of Moon:
		var Ml = QD(134.9634114)+477198.8676313*T+0.0089970*pow(T, 2)+pow(T,3)/69699.0-
      pow(T, 4)/14712000.0;
		Ml = Ml%360.0;
		Ml = Ml*Pi/180.0;
    //println("Ml: " + Ml);

		// Argument of latitude of the Moon (Average distance of the moon of ascendant node):
		var F = QD(93.2720993)+483202.0175273*T-0.0034029*pow(T, 2)-pow(T, 3)/3526000.0+
      pow(T, 4)/863310000.0;
		F = F%360.0;
		F = F*Pi/180.0;
    //println("F: " + F);

		// 3 other arguments necessary (in degrees):
		var A1 = QD(119.75) + 131.849 * T;
		A1 = A1%360.0;
		A1 = A1*Pi/180.0;

		var A2 = QD(53.09) + 479264.290 * T;
		A2 = A2%360.0;
		A2 = A2*Pi/180.0;

		var A3 = QD(313.45) + 481266.484 * T;
		A3 = A3%360.0;
		A3=A3*Pi/180.0;
    //println("A1: " + A1);
    //println("A2: " + A2);
    //println("A3: " + A3);

		var ll: Array[String] = null;
		var coeffD, coeffM, coeffMl, coeffF = 0;
		var coeffSigl, coeffSigr, coeffSigb, E = QD(0.0);
		var sigr, sigl, sigb = QD(0.0);

    // Read periodic terms of longitude (Sigl) and moon's distance (Sigr):
    for(line <- Source.fromFile("src/benchmarks/moon/Moontable1.rdb").getLines().drop(3)) {
      ll = line.split("\t");

      try {
				coeffD = Integer.parseInt(ll(0));
				coeffM = Integer.parseInt(ll(1));
				coeffMl = Integer.parseInt(ll(2));
				coeffF = Integer.parseInt(ll(3));
				coeffSigl = QD(java.lang.Double.parseDouble(ll(4)));
				coeffSigr = QD(java.lang.Double.parseDouble(ll(5)));

				E = QD(1)-0.002516*T-0.0000074*pow(T, 2);

				if ((coeffM == 1) || (coeffM == -1)){
					sigl = sigl + E * coeffSigl * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
					sigr = sigr + E * coeffSigr * cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}
				else if ((coeffM == 2) || (coeffM == -2)){
					sigl = sigl + pow(E, 2) * coeffSigl * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
					sigr = sigr + pow(E, 2) * coeffSigr * cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}
				else {
					sigl = sigl + coeffSigl * sin(D*coeffD+Ml*coeffMl+F*coeffF);
					sigr = sigr + coeffSigr * cos(D*coeffD+Ml*coeffMl+F*coeffF);
				}

			} catch {
        case e: NumberFormatException =>
				  println("Number format exception in MoonTable1");
			}
    }
    //println("sigl: " + sigl);
    //println("sigr: " + sigr);


    for(line <- Source.fromFile("src/benchmarks/moon/Moontable2.rdb").getLines().drop(3)) {
			ll = line.split("\t");

			try {
				coeffD = Integer.parseInt(ll(0));
				coeffM = Integer.parseInt(ll(1));
				coeffMl = Integer.parseInt(ll(2));
				coeffF = Integer.parseInt(ll(3));
				coeffSigb = QD(java.lang.Double.parseDouble(ll(4)));

				E = QD(1.0)-0.002516*T-0.0000074*pow(T, 2);

				if ((coeffM == 1) || (coeffM == -1)){
					sigb = sigb + E * coeffSigb * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}

				else if ((coeffM == 2) || (coeffM == -2)){
					sigb = sigb + pow(E, 2) * coeffSigb * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}

				else {
					sigb = sigb + coeffSigb * sin(D*coeffD+Ml*coeffMl+F*coeffF);
				}

			} catch {
        case e: NumberFormatException =>
				  println("Number format exception in MoonTable2");
			}
    }
    //println("sigb: " + sigb);


		// Additional terms due to Jupiter, Venus and flatening of Earth:
		sigl = sigl + 3958.0*sin(A1)+1962.*sin(Ll-F)+318.0*sin(A2);
		sigb = sigb - 2235.0*sin(Ll)+382.0*sin(A3)+175.0*sin(A1-F)+
      175.0*sin(A1+F)+127.0*sin(Ll-Ml)-115.0*sin(Ll+Ml);

    //println("\nAfter correction by Jupiter and mates:")
    //println("sigl: " + sigl);
    //println("sigb: " + sigb);


		// Moon coordinates in degrees
		var lbda = (Ll*180/Pi + sigl/1000000.0);
		lbda = lbda%360.0;
		var beta = (sigb/1000000.0);
		beta=beta%360.0;

		// Distance Moon - center of earth (in KM):
		val GrdDelta = QD(385000.56) + sigr/1000.0;

		// Apparent lambda by adding to lambda the nutation in longitude:
		var L=QD(280.4665)+36000.7698*T;
		L = L%360.0;
		L = L*Pi/180.0;

		// Longitude of the mean ascending node
		var omega=QD(125.0445550)-1934.1361849*T+0.0020762*pow(T, 2)+pow(T, 3)/467410.0-
      pow(T, 4)/60616000.0;
		omega = omega%360.0;
		var dphi = QD(-17.20)/3600.0*sin(Pi*omega/180.0)-QD(1.32)/3600.0*sin(2.0*L)-
      QD(0.23)/3600.0*sin(2.0*Ll)+QD(0.21)/3600.0*sin(2.0*Pi*omega/180.0);
		dphi = dphi%360.0;
		var applbda = lbda + dphi;
		applbda = applbda%360.0;

		// True obliquity of the ecliptic
		val eps0 = (QD(23.0)+QD(26.0)/60.0+QD(21.448)/3600.0)-QD(46.8150)/3600.0*T-QD(0.00059)/3600.0*pow(T, 2)+
      QD(0.001813)/3600.0*pow(T, 3);
		val deps = QD(9.20)/3600.0*cos(Pi*omega/180.0)+QD(0.57)/3600.0*cos(2.0*L)+
      QD(0.10)/3600.0*cos(2.0*Ll)-QD(0.09)/3600.0*cos(2*Pi*omega/180.0);
		var eps = eps0 + deps;
		eps = eps % 360.0;

		val alpha = darctan((sin(applbda*Pi/180.0)*cos(eps*Pi/180.0)-
          tan(beta*Pi/180.0)*sin(eps*Pi/180.0)),cos(applbda*Pi/180.0));
		var delta = asin(sin(beta*Pi/180.0)*cos(eps*Pi/180.0)+
        cos(beta*Pi/180.0)*sin(eps*Pi/180.0)*sin(applbda*Pi/180.0));
		delta = delta*180.0/Pi;

		val halpha = (alpha/15.0).toInt;
    val mnalpha = ((alpha/15.0-halpha)*60.0).toInt;
		val salpha = ((alpha/15.0-halpha)*60.0-mnalpha)*60.0;

		if (delta>90.0){
			delta = delta-360.0;
		}
		else {
			delta = 1.0*delta;
		}
    println("Alpha: " + alpha.toLongString())
    println("Delta: " + delta.toLongString())
		val ddelta = (delta).toInt;
		val mndelta = abs(((delta-ddelta)*60.0).toInt);
		val sdelta = abs(((delta-ddelta)*60.-((delta-ddelta)*60.).toInt)*60.0);

		println("Moon coordinates:");
		println("Alpha = %sh %sm %s ".format(halpha.toString, mnalpha.toString, salpha.toString))
		println("Delta = %sd %sm %s ".format(ddelta.toString, mndelta.toString, sdelta.toString))
    return (halpha, mnalpha, salpha, ddelta, mndelta, sdelta)
	}

	def darctan(y: QD, x: QD): QD = {
    val Pi = QD(3.141592653589793)
		if((x == 0) && (y == 0)){
			return QD(0.0);
		}

		else if ((y < 0.0) && (x > 0.0)){
			return (atan(y/x)*180.0/Pi)+360.0;
		}

		else if (x < 0.0){
			return (atan(y/x)*180.0/Pi)+180.0;
		}

		else {
			return atan(y/x)*180.0/Pi;
		}
	}

}


object QDMoon2 {
  import QD._

	val keyb = new Scanner(System.in);

	def moonPosition(y: Int, m: Int, day: Int, minutes: Int, hours: Int = 12):
    (QD, QD, QD, QD, QD, QD) = {
    val Pi = QD(3.141592653589793)
    var year = y
    var month = m
	  println("Compute for the " + day + "-" + month +
        "-" + year + " at " + hours + ":" + minutes);

		// Compute MJD:
		if(month == 1 || month == 2){
			year -= 1;
			month += 12;
		}

		val A = (year/100.0).toInt;
		val B = 2-A+(A/4.0).toInt;
		var julianDay: QD = (365.25*(year+4716)).toInt+(30.6001*(month+1)).toInt+day+B-QD(1524.5);
		julianDay = julianDay+QD(hours)/24.0;

		println("JD = " + julianDay);

		val T: QD = (julianDay-2451545.0)/QD(36525.0);
    println("T: " + T);

		// Longitude average of Moon:
		var Ll = QD(218.3164591)+QD(481267.88134236)*T-QD(0.0013268)*pow(T,2.0)+pow(T,3)/QD(538841.0)-
      pow(T,4.0)/QD(65194000.0);
		Ll = Ll%360.0;
		Ll = Ll*Pi/180.0;
    //println("Ll: " + Ll);

    // Elongation average ofMmoon:
		var D = QD(297.8502042)+QD(445267.1115168)*T-QD(0.0016300)*pow(T,2.0)+pow(T,3)/QD(545868.0)-
      pow(T,4)/QD(113065000.0);
		D = D%QD(360.0);
		D = D*Pi/QD(180.0);
    //println("D: " + D);

		// Anomaly average of Sun:
		var M = QD(357.5291092)+QD(35999.0502909)*T-QD(0.0001536)*pow(T,2)+pow(T,3)/QD(24490000.0);
		M = M%QD(360.0);
		M = M*Pi/QD(180.0);
    //println("M: " + M);

		// Anomaly average of Moon:
		var Ml = QD(134.9634114)+QD(477198.8676313)*T+QD(0.0089970)*pow(T,2)+pow(T,3)/QD(69699.0)-
      pow(T,4)/QD(14712000.0);
		Ml = Ml%QD(360.0);
		Ml = Ml*Pi/QD(180.0);
    //println("Ml: " + Ml);

		// Argument of latitude of the Moon (Average distance of the moon of ascendant node):
		var F = QD(93.2720993)+QD(483202.0175273)*T-QD(0.0034029)*pow(T,2)-pow(T,3)/QD(3526000.0)+
      pow(T,4)/QD(863310000.0);
		F = F%QD(360.0);
		F = F*Pi/QD(180.0);
    //println("F: " + F);

		// 3 other arguments necessary (in degrees):
		var A1 = QD(119.75) + QD(131.849) * T;
		A1 = A1%QD(360.0);
		A1 = A1*Pi/QD(180.0);

		var A2 = QD(53.09) + QD(479264.290) * T;
		A2 = A2%QD(360.0);
		A2 = A2*Pi/QD(180.0);

		var A3 = QD(313.45) + QD(481266.484) * T;
		A3 = A3%QD(360.0);
		A3=A3*Pi/QD(180.0);
    //println("A1: " + A1);
    //println("A2: " + A2);
    //println("A3: " + A3);

		var ll: Array[String] = null;
		var coeffD, coeffM, coeffMl, coeffF = 0;
		var coeffSigl, coeffSigr, coeffSigb, E = QD(0.0);
		var sigr, sigl, sigb = QD(0.0);

    // Read periodic terms of longitude (Sigl) and moon's distance (Sigr):
    for(line <- Source.fromFile("src/benchmarks/moon/Moontable1.rdb").getLines().drop(3)) {
      ll = line.split("\t");

      try {
				coeffD = Integer.parseInt(ll(0));
				coeffM = Integer.parseInt(ll(1));
				coeffMl = Integer.parseInt(ll(2));
				coeffF = Integer.parseInt(ll(3));
				coeffSigl = QD(java.lang.Double.parseDouble(ll(4)));
				coeffSigr = QD(java.lang.Double.parseDouble(ll(5)));

				E = QD(1)-QD(0.002516)*T-QD(0.0000074)*pow(T,2);

				if ((coeffM == 1) || (coeffM == -1)){
					sigl = sigl + E * coeffSigl * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
					sigr = sigr + E * coeffSigr * cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}
				else if ((coeffM == 2) || (coeffM == -2)){
					sigl = sigl + pow(E,2) * coeffSigl * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
					sigr = sigr + pow(E,2) * coeffSigr * cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}
				else {
					sigl = sigl + coeffSigl * sin(D*coeffD+Ml*coeffMl+F*coeffF);
					sigr = sigr + coeffSigr * cos(D*coeffD+Ml*coeffMl+F*coeffF);
				}

			} catch {
        case e: NumberFormatException =>
				  println("Number format exception in MoonTable1");
			}
    }
    //println("sigl: " + sigl);
    //println("sigr: " + sigr);


    for(line <- Source.fromFile("src/benchmarks/moon/Moontable2.rdb").getLines().drop(3)) {
			ll = line.split("\t");

			try {
				coeffD = Integer.parseInt(ll(0));
				coeffM = Integer.parseInt(ll(1));
				coeffMl = Integer.parseInt(ll(2));
				coeffF = Integer.parseInt(ll(3));
				coeffSigb = QD(java.lang.Double.parseDouble(ll(4)));

				E = QD(1.0)-QD(0.002516)*T-QD(0.0000074)*pow(T,2);

				if ((coeffM == 1) || (coeffM == -1)){
					sigb = sigb + E * coeffSigb * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}

				else if ((coeffM == 2) || (coeffM == -2)){
					sigb = sigb + pow(E,2) * coeffSigb * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}

				else {
					sigb = sigb + coeffSigb * sin(D*coeffD+Ml*coeffMl+F*coeffF);
				}

			} catch {
        case e: NumberFormatException =>
				  println("Number format exception in MoonTable2");
			}
    }
    //println("sigb: " + sigb);


		// Additional terms due to Jupiter, Venus and flatening of Earth:
		sigl = sigl + QD(3958.0)*sin(A1)+1962.*sin(Ll-F)+QD(318.0)*sin(A2);
		sigb = sigb - QD(2235.0)*sin(Ll)+QD(382.0)*sin(A3)+QD(175.0)*sin(A1-F)+
      QD(175.0)*sin(A1+F)+QD(127.0)*sin(Ll-Ml)-QD(115.0)*sin(Ll+Ml);

    //println("\nAfter correction by Jupiter and mates:")
    //println("sigl: " + sigl);
    //println("sigb: " + sigb);


		// Moon coordinates in degrees
		var lbda = (Ll*180/Pi + sigl/QD(1000000.0));
		lbda = lbda%QD(360.0);
		var beta = (sigb/QD(1000000.0));
		beta=beta%QD(360.0);

		// Distance Moon - center of earth (in KM):
		val GrdDelta = QD(385000.56) + sigr/QD(1000.0);

		// Apparent lambda by adding to lambda the nutation in longitude:
		var L=QD(280.4665)+QD(36000.7698)*T;
		L = L%QD(360.0);
		L = L*Pi/QD(180.0);

		// Longitude of the mean ascending node
		var omega=QD(125.0445550)-QD(1934.1361849)*T+QD(0.0020762)*pow(T,2)+pow(T,3)/QD(467410.0)-
      pow(T,4)/QD(60616000.0);
		omega = omega%QD(360.0);
		var dphi = QD(-17.20)/QD(3600.0)*sin(Pi*omega/QD(180.0))-QD(1.32)/QD(3600.0)*sin(QD(2.0)*L)-
      QD(0.23)/QD(3600.0)*sin(QD(2.0)*Ll)+QD(0.21)/QD(3600.0)*sin(QD(2.0)*Pi*omega/QD(180.0));
		dphi = dphi%QD(360.0);
		var applbda = lbda + dphi;
		applbda = applbda%QD(360.0);

		// True obliquity of the ecliptic
		val eps0 = (QD(23.0)+QD(26.0)/QD(60.0)+QD(21.448)/QD(3600.0))-QD(46.8150)/QD(3600.0)*T-QD(0.00059)/QD(3600.0)*pow(T,2)+
      QD(0.001813)/QD(3600.0)*pow(T,3);
		val deps = QD(9.20)/QD(3600.0)*cos(Pi*omega/QD(180.0))+QD(0.57)/QD(3600.0)*cos(QD(2.0)*L)+
      QD(0.10)/QD(3600.0)*cos(QD(2.0)*Ll)-QD(0.09)/QD(3600.0)*cos(2*Pi*omega/QD(180.0));
		var eps = eps0 + deps;
		eps = eps % QD(360.0);

		val alpha = darctan((sin(applbda*Pi/QD(180.0))*cos(eps*Pi/QD(180.0))-
          tan(beta*Pi/QD(180.0))*sin(eps*Pi/QD(180.0))),cos(applbda*Pi/QD(180.0)));
		var delta = asin(sin(beta*Pi/QD(180.0))*cos(eps*Pi/QD(180.0))+
        cos(beta*Pi/QD(180.0))*sin(eps*Pi/QD(180.0))*sin(applbda*Pi/QD(180.0)));
		delta = delta*QD(180.0)/Pi;

		val halpha = (alpha/QD(15.0)).toInt;
    val mnalpha = ((alpha/QD(15.0)-halpha)*QD(60.0)).toInt;
		val salpha = ((alpha/QD(15.0)-halpha)*QD(60.0)-mnalpha)*QD(60.0);

		if (delta>QD(90.0)){
			delta = delta-QD(360.0);
		}
		else {
			delta = QD(1.0)*delta;
		}
    println("\nAlpha: " + alpha)
    println("Delta: " + delta)
		val ddelta = (delta).toInt;
		val mndelta = abs(((delta-ddelta)*QD(60.0)).toInt);
		val sdelta = abs(((delta-ddelta)*60.-((delta-ddelta)*60.).toInt)*QD(60.0));

		println("Moon coordinates:");
		println("Alpha = %sh %sm %s ".format(halpha.toString, mnalpha.toString, salpha.toString))
		println("Delta = %sd %sm %s ".format(ddelta.toString, mndelta.toString, sdelta.toString))
    return (halpha, mnalpha, salpha, ddelta, mndelta, sdelta)
	}

	def darctan(y: QD, x: QD): QD = {
    val Pi = QD(3.141592653589793)
		if((x == 0) && (y == 0)){
			return QD(0.0);
		}

		else if ((y < QD(0.0)) && (x > QD(0.0))){
			return (atan(y/x)*QD(180.0)/Pi)+QD(360.0);
		}

		else if (x < QD(0.0)){
			return (atan(y/x)*QD(180.0)/Pi)+QD(180.0);
		}

		else {
			return atan(y/x)*QD(180.0)/Pi;
		}
	}

}

object AFMoon2 {
  import AF._

	val keyb = new Scanner(System.in);

	def moonPosition(y: Int, m: Int, day: Int, minutes: Int, hours: Int = 12):
    (AF, AF, AF, AF, AF, AF) = {
    val Pi = AF(3.141592653589793)
    var year = y
    var month = m
	  println("Compute for the " + day + "-" + month +
        "-" + year + " at " + hours + ":" + minutes);

		// Compute MJD:
		if(month == 1 || month == 2){
			year -= 1;
			month += 12;
		}

		val A = (year/100.0).toInt;
		val B = 2-A+(A/4.0).toInt;
		var julianDay: AF = (365.25*(year+4716)).toInt+(30.6001*(month+1)).toInt+day+B-AF(1524.5);
		julianDay = julianDay+AF(hours)/24.0;

		println("JD = " + julianDay);

		val T: AF = (julianDay-2451545.0)/AF(36525.0);
    println("T: " + T);

		// Longitude average of Moon:
		var Ll = AF(218.3164591)+AF(481267.88134236)*T-AF(0.0013268)*pow2(T)+pow3(T)/AF(538841.0)-
      pow4(T)/AF(65194000.0);
		Ll = Ll%360.0;
		Ll = Ll*Pi/180.0;
    //println("Ll: " + Ll);

    // Elongation average ofMmoon:
		var D = AF(297.8502042)+AF(445267.1115168)*T-AF(0.0016300)*pow2(T)+pow3(T)/AF(545868.0)-
      pow4(T)/AF(113065000.0);
		D = D%AF(360.0);
		D = D*Pi/AF(180.0);
    //println("D: " + D);

		// Anomaly average of Sun:
		var M = AF(357.5291092)+AF(35999.0502909)*T-AF(0.0001536)*pow2(T)+pow3(T)/AF(24490000.0);
		M = M%AF(360.0);
		M = M*Pi/AF(180.0);
    //println("M: " + M);

		// Anomaly average of Moon:
		var Ml = AF(134.9634114)+AF(477198.8676313)*T+AF(0.0089970)*pow2(T)+pow3(T)/AF(69699.0)-
      pow4(T)/AF(14712000.0);
		Ml = Ml%AF(360.0);
		Ml = Ml*Pi/AF(180.0);
    //println("Ml: " + Ml);

		// Argument of latitude of the Moon (Average distance of the moon of ascendant node):
		var F = AF(93.2720993)+AF(483202.0175273)*T-AF(0.0034029)*pow2(T)-pow3(T)/AF(3526000.0)+
      pow4(T)/AF(863310000.0);
		F = F%AF(360.0);
		F = F*Pi/AF(180.0);
    //println("F: " + F);

		// 3 other arguments necessary (in degrees):
		var A1 = AF(119.75) + AF(131.849) * T;
		A1 = A1%AF(360.0);
		A1 = A1*Pi/AF(180.0);

		var A2 = AF(53.09) + AF(479264.290) * T;
		A2 = A2%AF(360.0);
		A2 = A2*Pi/AF(180.0);

		var A3 = AF(313.45) + AF(481266.484) * T;
		A3 = A3%AF(360.0);
		A3=A3*Pi/AF(180.0);
    //println("A1: " + A1);
    //println("A2: " + A2);
    //println("A3: " + A3);

		var ll: Array[String] = null;
		var coeffD, coeffM, coeffMl, coeffF = 0;
		var coeffSigl, coeffSigr, coeffSigb, E = AF(0.0);
		var sigr, sigl, sigb = AF(0.0);

    // Read periodic terms of longitude (Sigl) and moon's distance (Sigr):
    for(line <- Source.fromFile("src/benchmarks/moon/Moontable1.rdb").getLines().drop(3)) {
      ll = line.split("\t");

      try {
				coeffD = Integer.parseInt(ll(0));
				coeffM = Integer.parseInt(ll(1));
				coeffMl = Integer.parseInt(ll(2));
				coeffF = Integer.parseInt(ll(3));
				coeffSigl = AF(java.lang.Double.parseDouble(ll(4)));
				coeffSigr = AF(java.lang.Double.parseDouble(ll(5)));

				E = AF(1)-AF(0.002516)*T-AF(0.0000074)*pow2(T);

				if ((coeffM == 1) || (coeffM == -1)){
					sigl = sigl + E * coeffSigl * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
					sigr = sigr + E * coeffSigr * cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}
				else if ((coeffM == 2) || (coeffM == -2)){
					sigl = sigl + pow2(E) * coeffSigl * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
					sigr = sigr + pow2(E) * coeffSigr * cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}
				else {
					sigl = sigl + coeffSigl * sin(D*coeffD+Ml*coeffMl+F*coeffF);
					sigr = sigr + coeffSigr * cos(D*coeffD+Ml*coeffMl+F*coeffF);
				}

			} catch {
        case e: NumberFormatException =>
				  println("Number format exception in MoonTable1");
			}
    }
    //println("sigl: " + sigl);
    //println("sigr: " + sigr);


    for(line <- Source.fromFile("src/benchmarks/moon/Moontable2.rdb").getLines().drop(3)) {
			ll = line.split("\t");

			try {
				coeffD = Integer.parseInt(ll(0));
				coeffM = Integer.parseInt(ll(1));
				coeffMl = Integer.parseInt(ll(2));
				coeffF = Integer.parseInt(ll(3));
				coeffSigb = AF(java.lang.Double.parseDouble(ll(4)));

				E = AF(1.0)-AF(0.002516)*T-AF(0.0000074)*pow2(T);

				if ((coeffM == 1) || (coeffM == -1)){
					sigb = sigb + E * coeffSigb * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}

				else if ((coeffM == 2) || (coeffM == -2)){
					sigb = sigb + pow2(E) * coeffSigb * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}

				else {
					sigb = sigb + coeffSigb * sin(D*coeffD+Ml*coeffMl+F*coeffF);
				}

			} catch {
        case e: NumberFormatException =>
				  println("Number format exception in MoonTable2");
			}
    }
    //println("sigb: " + sigb);


		// Additional terms due to Jupiter, Venus and flatening of Earth:
		sigl = sigl + AF(3958.0)*sin(A1)+1962.*sin(Ll-F)+AF(318.0)*sin(A2);
		sigb = sigb - AF(2235.0)*sin(Ll)+AF(382.0)*sin(A3)+AF(175.0)*sin(A1-F)+
      AF(175.0)*sin(A1+F)+AF(127.0)*sin(Ll-Ml)-AF(115.0)*sin(Ll+Ml);

    //println("\nAfter correction by Jupiter and mates:")
    //println("sigl: " + sigl);
    //println("sigb: " + sigb);


		// Moon coordinates in degrees
		var lbda = (Ll*180/Pi + sigl/AF(1000000.0));
		lbda = lbda%AF(360.0);
		var beta = (sigb/AF(1000000.0));
		beta=beta%AF(360.0);

		// Distance Moon - center of earth (in KM):
		val GrdDelta = AF(385000.56) + sigr/AF(1000.0);

		// Apparent lambda by adding to lambda the nutation in longitude:
		var L=AF(280.4665)+AF(36000.7698)*T;
		L = L%AF(360.0);
		L = L*Pi/AF(180.0);

		// Longitude of the mean ascending node
		var omega=AF(125.0445550)-AF(1934.1361849)*T+AF(0.0020762)*pow2(T)+pow3(T)/AF(467410.0)-
      pow4(T)/AF(60616000.0);
		omega = omega%AF(360.0);
		var dphi = AF(-17.20)/AF(3600.0)*sin(Pi*omega/AF(180.0))-AF(1.32)/AF(3600.0)*sin(AF(2.0)*L)-
      AF(0.23)/AF(3600.0)*sin(AF(2.0)*Ll)+AF(0.21)/AF(3600.0)*sin(AF(2.0)*Pi*omega/AF(180.0));
		dphi = dphi%AF(360.0);
		var applbda = lbda + dphi;
		applbda = applbda%AF(360.0);

		// True obliquity of the ecliptic
		val eps0 = (AF(23.0)+AF(26.0)/AF(60.0)+AF(21.448)/AF(3600.0))-AF(46.8150)/AF(3600.0)*T-AF(0.00059)/AF(3600.0)*pow2(T)+
      AF(0.001813)/AF(3600.0)*pow3(T);
		val deps = AF(9.20)/AF(3600.0)*cos(Pi*omega/AF(180.0))+AF(0.57)/AF(3600.0)*cos(AF(2.0)*L)+
      AF(0.10)/AF(3600.0)*cos(AF(2.0)*Ll)-AF(0.09)/AF(3600.0)*cos(2*Pi*omega/AF(180.0));
		var eps = eps0 + deps;
		eps = eps % AF(360.0);

		val alpha = darctan((sin(applbda*Pi/AF(180.0))*cos(eps*Pi/AF(180.0))-
          tan(beta*Pi/AF(180.0))*sin(eps*Pi/AF(180.0))),cos(applbda*Pi/AF(180.0)));
		var delta = asin(sin(beta*Pi/AF(180.0))*cos(eps*Pi/AF(180.0))+
        cos(beta*Pi/AF(180.0))*sin(eps*Pi/AF(180.0))*sin(applbda*Pi/AF(180.0)));
		delta = delta*AF(180.0)/Pi;

		val halpha = (alpha/AF(15.0)).toInt;
    val mnalpha = ((alpha/AF(15.0)-halpha)*AF(60.0)).toInt;
		val salpha = ((alpha/AF(15.0)-halpha)*AF(60.0)-mnalpha)*AF(60.0);

		if (delta>AF(90.0)){
			delta = delta-AF(360.0);
		}
		else {
			delta = AF(1.0)*delta;
		}
    println("\nAlpha: " + alpha)
    println("Delta: " + delta)
		val ddelta = (delta).toInt;
		val mndelta = abs(((delta-ddelta)*AF(60.0)).toInt);
		val sdelta = abs(((delta-ddelta)*60.-((delta-ddelta)*60.).toInt)*AF(60.0));

		println("Moon coordinates:");
		println("Alpha = %sh %sm %s ".format(halpha.toString, mnalpha.toString, salpha.toString))
		println("Delta = %sd %sm %s ".format(ddelta.toString, mndelta.toString, sdelta.toString))
    return (halpha, mnalpha, salpha, ddelta, mndelta, sdelta)
	}

	def darctan(y: AF, x: AF): AF = {
    val Pi = AF(3.141592653589793)
		if((x == 0) && (y == 0)){
			return AF(0.0);
		}

		else if ((y < AF(0.0)) && (x > AF(0.0))){
			return (atan(y/x)*AF(180.0)/Pi)+AF(360.0);
		}

		else if (x < AF(0.0)){
			return (atan(y/x)*AF(180.0)/Pi)+AF(180.0);
		}

		else {
			return atan(y/x)*AF(180.0)/Pi;
		}
	}

}


object AFMoon {
  import AF._

	val keyb = new Scanner(System.in);

	def moonPosition(y: Int, m: Int, day: Int, minutes: Int, hours: Int = 12):
    (AF, AF, AF, AF, AF, AF) = {
    val Pi = AF(3.141592653589793)
    var year = y
    var month = m
	  println("Compute for the " + day + "-" + month +
        "-" + year + " at " + hours + ":" + minutes);

		// Compute MJD:
		if(month == 1 || month == 2){
			year -= 1;
			month += 12;
		}

		val A = (year/100.0).toInt;
		val B = 2-A+(A/4.0).toInt;
		var julianDay: AF = (365.25*(year+4716)).toInt+(30.6001*(month+1)).toInt+day+B-AF(1524.5);
		julianDay = julianDay+AF(hours)/24.0;

		//println("JD = " + julianDay);

		val T: AF = (julianDay-2451545.0)/36525.0;
    //println("T: " + T);

		// Longitude average of Moon:
		var Ll = AF(218.3164591)+AF(481267.88134236)*T-0.0013268*pow2(T)+pow3(T)/538841.0-
      pow4(T)/65194000.0;
		Ll = Ll%360.0;
		Ll = Ll*Pi/180.0;
    //println("Ll: " + Ll);

    // Elongation average ofMmoon:
		var D = AF(297.8502042)+445267.1115168*T-0.0016300*pow2(T)+pow3(T)/545868.0-
      pow4(T)/113065000.0;
		D = D%360.0;
		D = D*Pi/180.0;
    //println("D: " + D);

		// Anomaly average of Sun:
		var M = AF(357.5291092)+35999.0502909*T-0.0001536*pow2(T)+pow3(T)/24490000.0;
		M = M%360.0;
		M = M*Pi/180.0;
    //println("M: " + M);

		// Anomaly average of Moon:
		var Ml = AF(134.9634114)+477198.8676313*T+0.0089970*pow2(T)+pow3(T)/69699.0-
      pow4(T)/14712000.0;
		Ml = Ml%360.0;
		Ml = Ml*Pi/180.0;
    //println("Ml: " + Ml);

		// Argument of latitude of the Moon (Average distance of the moon of ascendant node):
		var F = AF(93.2720993)+483202.0175273*T-0.0034029*pow2(T)-pow3(T)/3526000.0+
      pow4(T)/863310000.0;
		F = F%360.0;
		F = F*Pi/180.0;
    //println("F: " + F);

		// 3 other arguments necessary (in degrees):
		var A1 = AF(119.75) + 131.849 * T;
		A1 = A1%360.0;
		A1 = A1*Pi/180.0;

		var A2 = AF(53.09) + 479264.290 * T;
		A2 = A2%360.0;
		A2 = A2*Pi/180.0;

		var A3 = AF(313.45) + 481266.484 * T;
		A3 = A3%360.0;
		A3=A3*Pi/180.0;
    //println("A1: " + A1);
    //println("A2: " + A2);
    //println("A3: " + A3);

		var ll: Array[String] = null;
		var coeffD, coeffM, coeffMl, coeffF = 0;
		var coeffSigl, coeffSigr, coeffSigb, E = AF(0.0);
		var sigr, sigl, sigb = AF(0.0);

    // Read periodic terms of longitude (Sigl) and moon's distance (Sigr):
    for(line <- Source.fromFile("src/benchmarks/moon/Moontable1.rdb").getLines().drop(3)) {
      ll = line.split("\t");

      try {
				coeffD = Integer.parseInt(ll(0));
				coeffM = Integer.parseInt(ll(1));
				coeffMl = Integer.parseInt(ll(2));
				coeffF = Integer.parseInt(ll(3));
				coeffSigl = AF(java.lang.Double.parseDouble(ll(4)));
				coeffSigr = AF(java.lang.Double.parseDouble(ll(5)));

				E = AF(1)-0.002516*T-0.0000074*pow2(T);

				if ((coeffM == 1) || (coeffM == -1)){
					sigl = sigl + E * coeffSigl * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
					sigr = sigr + E * coeffSigr * cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}
				else if ((coeffM == 2) || (coeffM == -2)){
					sigl = sigl + pow2(E) * coeffSigl * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
					sigr = sigr + pow2(E) * coeffSigr * cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}
				else {
					sigl = sigl + coeffSigl * sin(D*coeffD+Ml*coeffMl+F*coeffF);
					sigr = sigr + coeffSigr * cos(D*coeffD+Ml*coeffMl+F*coeffF);
				}

			} catch {
        case e: NumberFormatException =>
				  println("Number format exception in MoonTable1");
			}
    }
    //println("sigl: " + sigl);
    //println("sigr: " + sigr);


    for(line <- Source.fromFile("src/benchmarks/moon/Moontable2.rdb").getLines().drop(3)) {
			ll = line.split("\t");

			try {
				coeffD = Integer.parseInt(ll(0));
				coeffM = Integer.parseInt(ll(1));
				coeffMl = Integer.parseInt(ll(2));
				coeffF = Integer.parseInt(ll(3));
				coeffSigb = AF(java.lang.Double.parseDouble(ll(4)));

				E = AF(1.0)-0.002516*T-0.0000074*pow2(T);

				if ((coeffM == 1) || (coeffM == -1)){
					sigb = sigb + E * coeffSigb * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}

				else if ((coeffM == 2) || (coeffM == -2)){
					sigb = sigb + pow2(E) * coeffSigb * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}

				else {
					sigb = sigb + coeffSigb * sin(D*coeffD+Ml*coeffMl+F*coeffF);
				}

			} catch {
        case e: NumberFormatException =>
				  println("Number format exception in MoonTable2");
			}
    }
    //println("sigb: " + sigb);


		// Additional terms due to Jupiter, Venus and flatening of Earth:
		sigl = sigl + 3958.0*sin(A1)+1962.*sin(Ll-F)+318.0*sin(A2);
		sigb = sigb - 2235.0*sin(Ll)+382.0*sin(A3)+175.0*sin(A1-F)+
      175.0*sin(A1+F)+127.0*sin(Ll-Ml)-115.0*sin(Ll+Ml);

    //println("\nAfter correction by Jupiter and mates:")
    //println("sigl: " + sigl);
    //println("sigb: " + sigb);


		// Moon coordinates in degrees
		var lbda = (Ll*180/Pi + sigl/1000000.0);
		lbda = lbda%360.0;
		var beta = (sigb/1000000.0);
		beta=beta%360.0;

		// Distance Moon - center of earth (in KM):
		val GrdDelta = AF(385000.56) + sigr/1000.0;

		// Apparent lambda by adding to lambda the nutation in longitude:
		var L=AF(280.4665)+36000.7698*T;
		L = L%360.0;
		L = L*Pi/180.0;

		// Longitude of the mean ascending node
		var omega=AF(125.0445550)-1934.1361849*T+0.0020762*pow2(T)+pow3(T)/467410.0-
      pow4(T)/60616000.0;
		omega = omega%360.0;
		var dphi = AF(-17.20)/3600.0*sin(Pi*omega/180.0)-AF(1.32)/3600.0*sin(2.0*L)-
      AF(0.23)/3600.0*sin(2.0*Ll)+AF(0.21)/3600.0*sin(2.0*Pi*omega/180.0);
		dphi = dphi%360.0;
		var applbda = lbda + dphi;
		applbda = applbda%360.0;
    println(" ---> aa: " + applbda)


		// True obliquity of the ecliptic
		val eps0 = (AF(23.0)+AF(26.0)/60.0+AF(21.448)/3600.0)-AF(46.8150)/3600.0*T-AF(0.00059)/3600.0*pow2(T)+
      AF(0.001813)/3600.0*pow3(T);
		val deps = AF(9.20)/3600.0*cos(Pi*omega/180.0)+AF(0.57)/3600.0*cos(2.0*L)+
      AF(0.10)/3600.0*cos(2.0*Ll)-AF(0.09)/3600.0*cos(2*Pi*omega/180.0);
		var eps = eps0 + deps;
    println("eps: " + eps)
		eps = eps % 360.0;

		val alpha = darctan((sin(applbda*Pi/180.0)*cos(eps*Pi/180.0)-
          tan(beta*Pi/180.0)*sin(eps*Pi/180.0)),cos(applbda*Pi/180.0));
		var delta = asin(sin(beta*Pi/180.0)*cos(eps*Pi/180.0)+
        cos(beta*Pi/180.0)*sin(eps*Pi/180.0)*sin(applbda*Pi/180.0));
		delta = delta*180.0/Pi;

		val halpha = (alpha/15.0).toInt;
    val mnalpha = ((alpha/15.0-halpha)*60.0).toInt;
		val salpha = ((alpha/15.0-halpha)*60.0-mnalpha)*60.0;

		if (delta>90.0){
			delta = delta-360.0;
		}
		else {
			delta = 1.0*delta;
		}
    println("Alpha: " + alpha + "   " + alpha.interval)
    println("Delta: " + delta + "   " + delta.interval)
		val ddelta = (delta).toInt;
		val mndelta = abs(((delta-ddelta)*60.0).toInt);
		val sdelta = abs(((delta-ddelta)*60.-((delta-ddelta)*60.).toInt)*60.0);

		println("Moon coordinates:");
		println("Alpha = %sh %sm %s ".format(halpha.toString, mnalpha.toString, salpha.toString))
		println("Delta = %sd %sm %s ".format(ddelta.toString, mndelta.toString, sdelta.toString))
    return (halpha, mnalpha, salpha, ddelta, mndelta, sdelta)
	}

	def darctan(y: AF, x: AF): AF = {
    val Pi = AF(3.141592653589793)
		if((x == 0) && (y == 0)){
			return AF(0.0);
		}

		else if ((y < 0.0) && (x > 0.0)){
			return (atan(y/x)*180.0/Pi)+360.0;
		}

		else if (x < 0.0){
			return (atan(y/x)*180.0/Pi)+180.0;
		}

		else {
			return atan(y/x)*180.0/Pi;
		}
	}

}


object IFMoon {
  import IF._

	val keyb = new Scanner(System.in);

	def moonPosition(y: Int, m: Int, day: Int, minutes: Int, hours: Int = 12):
    (IF, IF, IF, IF, IF, IF) = {
    val Pi = IF(3.141592653589793)
    var year = y
    var month = m
	  println("Compute for the " + day + "-" + month +
        "-" + year + " at " + hours + ":" + minutes);

		// Compute MJD:
		if(month == 1 || month == 2){
			year -= 1;
			month += 12;
		}

		val A = (year/100.0).toInt;
		val B = 2-A+(A/4.0).toInt;
		var julianDay: IF = (365.25*(year+4716)).toInt+(30.6001*(month+1)).toInt+day+B-IF(1524.5);
		julianDay = julianDay+IF(hours)/24.0;

		//println("JD = " + julianDay);

		val T: IF = (julianDay-2451545.0)/36525.0;
    //println("T: " + T);

		// Longitude average of Moon:
		var Ll = IF(218.3164591)+IF(481267.88134236)*T-0.0013268*pow2(T)+pow3(T)/538841.0-
      pow4(T)/65194000.0;
		Ll = Ll%360.0;
		Ll = Ll*Pi/180.0;
    //println("Ll: " + Ll);

    // Elongation average ofMmoon:
		var D = IF(297.8502042)+445267.1115168*T-0.0016300*pow2(T)+pow3(T)/545868.0-
      pow4(T)/113065000.0;
		D = D%360.0;
		D = D*Pi/180.0;
    //println("D: " + D);

		// Anomaly average of Sun:
		var M = IF(357.5291092)+35999.0502909*T-0.0001536*pow2(T)+pow3(T)/24490000.0;
		M = M%360.0;
		M = M*Pi/180.0;
    //println("M: " + M);

		// Anomaly average of Moon:
		var Ml = IF(134.9634114)+477198.8676313*T+0.0089970*pow2(T)+pow3(T)/69699.0-
      pow4(T)/14712000.0;
		Ml = Ml%360.0;
		Ml = Ml*Pi/180.0;
    //println("Ml: " + Ml);

		// Argument of latitude of the Moon (Average distance of the moon of ascendant node):
		var F = IF(93.2720993)+483202.0175273*T-0.0034029*pow2(T)-pow3(T)/3526000.0+
      pow4(T)/863310000.0;
		F = F%360.0;
		F = F*Pi/180.0;
    //println("F: " + F);

		// 3 other arguments necessary (in degrees):
		var A1 = IF(119.75) + 131.849 * T;
		A1 = A1%360.0;
		A1 = A1*Pi/180.0;

		var A2 = IF(53.09) + 479264.290 * T;
		A2 = A2%360.0;
		A2 = A2*Pi/180.0;

		var A3 = IF(313.45) + 481266.484 * T;
		A3 = A3%360.0;
		A3=A3*Pi/180.0;
    //println("A1: " + A1);
    //println("A2: " + A2);
    //println("A3: " + A3);

		var ll: Array[String] = null;
		var coeffD, coeffM, coeffMl, coeffF = 0;
		var coeffSigl, coeffSigr, coeffSigb, E = IF(0.0);
		var sigr, sigl, sigb = IF(0.0);

    // Read periodic terms of longitude (Sigl) and moon's distance (Sigr):
    for(line <- Source.fromFile("src/benchmarks/moon/Moontable1.rdb").getLines().drop(3)) {
      ll = line.split("\t");

      try {
				coeffD = Integer.parseInt(ll(0));
				coeffM = Integer.parseInt(ll(1));
				coeffMl = Integer.parseInt(ll(2));
				coeffF = Integer.parseInt(ll(3));
				coeffSigl = IF(java.lang.Double.parseDouble(ll(4)));
				coeffSigr = IF(java.lang.Double.parseDouble(ll(5)));

				E = IF(1)-0.002516*T-0.0000074*pow2(T);

				if ((coeffM == 1) || (coeffM == -1)){
					sigl = sigl + E * coeffSigl * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
					sigr = sigr + E * coeffSigr * cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}
				else if ((coeffM == 2) || (coeffM == -2)){
					sigl = sigl + pow2(E) * coeffSigl * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
					sigr = sigr + pow2(E) * coeffSigr * cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}
				else {
					sigl = sigl + coeffSigl * sin(D*coeffD+Ml*coeffMl+F*coeffF);
					sigr = sigr + coeffSigr * cos(D*coeffD+Ml*coeffMl+F*coeffF);
				}

			} catch {
        case e: NumberFormatException =>
				  println("Number format exception in MoonTable1");
			}
    }
    //println("sigl: " + sigl);
    //println("sigr: " + sigr);


    for(line <- Source.fromFile("src/benchmarks/moon/Moontable2.rdb").getLines().drop(3)) {
			ll = line.split("\t");

			try {
				coeffD = Integer.parseInt(ll(0));
				coeffM = Integer.parseInt(ll(1));
				coeffMl = Integer.parseInt(ll(2));
				coeffF = Integer.parseInt(ll(3));
				coeffSigb = IF(java.lang.Double.parseDouble(ll(4)));

				E = IF(1.0)-0.002516*T-0.0000074*pow2(T);

				if ((coeffM == 1) || (coeffM == -1)){
					sigb = sigb + E * coeffSigb * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}

				else if ((coeffM == 2) || (coeffM == -2)){
					sigb = sigb + pow2(E) * coeffSigb * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
				}

				else {
					sigb = sigb + coeffSigb * sin(D*coeffD+Ml*coeffMl+F*coeffF);
				}

			} catch {
        case e: NumberFormatException =>
				  println("Number format exception in MoonTable2");
			}
    }
    //println("sigb: " + sigb);


		// Additional terms due to Jupiter, Venus and flatening of Earth:
		sigl = sigl + 3958.0*sin(A1)+1962.*sin(Ll-F)+318.0*sin(A2);
		sigb = sigb - 2235.0*sin(Ll)+382.0*sin(A3)+175.0*sin(A1-F)+
      175.0*sin(A1+F)+127.0*sin(Ll-Ml)-115.0*sin(Ll+Ml);

    //println("\nAfter correction by Jupiter and mates:")
    //println("sigl: " + sigl);
    //println("sigb: " + sigb);


		// Moon coordinates in degrees
		var lbda = (Ll*180/Pi + sigl/1000000.0);
		lbda = lbda%360.0;
		var beta = (sigb/1000000.0);
		beta=beta%360.0;

		// Distance Moon - center of earth (in KM):
		val GrdDelta = IF(385000.56) + sigr/1000.0;

		// Apparent lambda by adding to lambda the nutation in longitude:
		var L=IF(280.4665)+36000.7698*T;
		L = L%360.0;
		L = L*Pi/180.0;

		// Longitude of the mean ascending node
		var omega=IF(125.0445550)-1934.1361849*T+0.0020762*pow2(T)+pow3(T)/467410.0-
      pow4(T)/60616000.0;
		omega = omega%360.0;
		var dphi = IF(-17.20)/3600.0*sin(Pi*omega/180.0)-IF(1.32)/3600.0*sin(2.0*L)-
      IF(0.23)/3600.0*sin(2.0*Ll)+IF(0.21)/3600.0*sin(2.0*Pi*omega/180.0);
		dphi = dphi%360.0;
		var applbda = lbda + dphi;
		applbda = applbda%360.0;

		// True obliquity of the ecliptic
		val eps0 = (IF(23.0)+IF(26.0)/60.0+IF(21.448)/3600.0)-IF(46.8150)/3600.0*T-IF(0.00059)/3600.0*pow2(T)+
      IF(0.001813)/3600.0*pow3(T);
		val deps = IF(9.20)/3600.0*cos(Pi*omega/180.0)+IF(0.57)/3600.0*cos(2.0*L)+
      IF(0.10)/3600.0*cos(2.0*Ll)-IF(0.09)/3600.0*cos(2*Pi*omega/180.0);
		var eps = eps0 + deps;
		eps = eps % 360.0;

		val alpha = darctan((sin(applbda*Pi/180.0)*cos(eps*Pi/180.0)-
          tan(beta*Pi/180.0)*sin(eps*Pi/180.0)),cos(applbda*Pi/180.0));
		var delta = asin(sin(beta*Pi/180.0)*cos(eps*Pi/180.0)+
        cos(beta*Pi/180.0)*sin(eps*Pi/180.0)*sin(applbda*Pi/180.0));
		delta = delta*180.0/Pi;

		val halpha = (alpha/15.0).toInt;
    val mnalpha = ((alpha/15.0-halpha)*60.0).toInt;
		val salpha = ((alpha/15.0-halpha)*60.0-mnalpha)*60.0;

		if (delta>90.0){
			delta = delta-360.0;
		}
		else {
			delta = 1.0*delta;
		}
    println("Alpha: " + alpha)
    println("Delta: " + delta)
		val ddelta = (delta).toInt;
		val mndelta = abs(((delta-ddelta)*60.0).toInt);
		val sdelta = abs(((delta-ddelta)*60.-((delta-ddelta)*60.).toInt)*60.0);

		println("Moon coordinates:");
		println("Alpha = %sh %sm %s ".format(halpha.toString, mnalpha.toString, salpha.toString))
		println("Delta = %sd %sm %s ".format(ddelta.toString, mndelta.toString, sdelta.toString))
    return (halpha, mnalpha, salpha, ddelta, mndelta, sdelta)
	}

	def darctan(y: IF, x: IF): IF = {
    val Pi = IF(3.141592653589793)
		if((x == 0) && (y == 0)){
			return IF(0.0);
		}

		else if ((y < 0.0) && (x > 0.0)){
			return (atan(y/x)*180.0/Pi)+360.0;
		}

		else if (x < 0.0){
			return (atan(y/x)*180.0/Pi)+180.0;
		}

		else {
			return atan(y/x)*180.0/Pi;
		}
	}

}


object SFMoon {
  import SF._

  val keyb = new Scanner(System.in);

  def moonPosition(y: Int, m: Int, day: Int, minutes: Int, hours: Int = 12):
    (SF, SF, SF, SF, SF, SF) = {
    val Pi = SF(3.141592653589793)
    var year = y
    var month = m
    println("Compute for the " + day + "-" + month +
        "-" + year + " at " + hours + ":" + minutes);

    // Compute MJD:
    if(month == 1 || month == 2){
      year -= 1;
      month += 12;
    }

    val A = (year/100.0).toInt;
    val B = 2-A+(A/4.0).toInt;
    var julianDay: SF = (365.25*(year+4716)).toInt+(30.6001*(month+1)).toInt+day+B-SF(1524.5);
    julianDay = julianDay+SF(hours)/24.0;

    //println("JD = " + julianDay);

    val T: SF = (julianDay-2451545.0)/36525.0;
    //println("T: " + T);

    // Longitude average of Moon:
    var Ll = SF(218.3164591)+SF(481267.88134236)*T-0.0013268*pow2(T)+pow3(T)/538841.0-
      pow4(T)/65194000.0;
    Ll = Ll%360.0;
    Ll = Ll*Pi/180.0;
    //println("Ll: " + Ll);

    // Elongation average ofMmoon:
    var D = SF(297.8502042)+445267.1115168*T-0.0016300*pow2(T)+pow3(T)/545868.0-
      pow4(T)/113065000.0;
    D = D%360.0;
    D = D*Pi/180.0;
    //println("D: " + D);

    // Anomaly average of Sun:
    var M = SF(357.5291092)+35999.0502909*T-0.0001536*pow2(T)+pow3(T)/24490000.0;
    M = M%360.0;
    M = M*Pi/180.0;
    //println("M: " + M);

    // Anomaly average of Moon:
    var Ml = SF(134.9634114)+477198.8676313*T+0.0089970*pow2(T)+pow3(T)/69699.0-
      pow4(T)/14712000.0;
    Ml = Ml%360.0;
    Ml = Ml*Pi/180.0;
    //println("Ml: " + Ml);

    // Argument of latitude of the Moon (Average distance of the moon of ascendant node):
    var F = SF(93.2720993)+483202.0175273*T-0.0034029*pow2(T)-pow3(T)/3526000.0+
      pow4(T)/863310000.0;
    F = F%360.0;
    F = F*Pi/180.0;
    //println("F: " + F);

    // 3 other arguments necessary (in degrees):
    var A1 = SF(119.75) + 131.849 * T;
    A1 = A1%360.0;
    A1 = A1*Pi/180.0;

    var A2 = SF(53.09) + 479264.290 * T;
    A2 = A2%360.0;
    A2 = A2*Pi/180.0;

    var A3 = SF(313.45) + 481266.484 * T;
    A3 = A3%360.0;
    A3=A3*Pi/180.0;
    //println("A1: " + A1);
    //println("A2: " + A2);
    //println("A3: " + A3);

    var ll: Array[String] = null;
    var coeffD, coeffM, coeffMl, coeffF = 0;
    var coeffSigl, coeffSigr, coeffSigb, E = SF(0.0);
    var sigr, sigl, sigb = SF(0.0);

    // Read periodic terms of longitude (Sigl) and moon's distance (Sigr):
    for(line <- Source.fromFile("src/benchmarks/moon/Moontable1.rdb").getLines().drop(3)) {
      ll = line.split("\t");

      try {
        coeffD = Integer.parseInt(ll(0));
        coeffM = Integer.parseInt(ll(1));
        coeffMl = Integer.parseInt(ll(2));
        coeffF = Integer.parseInt(ll(3));
        coeffSigl = SF(java.lang.Double.parseDouble(ll(4)));
        coeffSigr = SF(java.lang.Double.parseDouble(ll(5)));

        E = SF(1)-0.002516*T-0.0000074*pow2(T);

        if ((coeffM == 1) || (coeffM == -1)){
          sigl = sigl + E * coeffSigl * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
          sigr = sigr + E * coeffSigr * cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
        }
        else if ((coeffM == 2) || (coeffM == -2)){
          sigl = sigl + pow2(E) * coeffSigl * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
          sigr = sigr + pow2(E) * coeffSigr * cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
        }
        else {
          sigl = sigl + coeffSigl * sin(D*coeffD+Ml*coeffMl+F*coeffF);
          sigr = sigr + coeffSigr * cos(D*coeffD+Ml*coeffMl+F*coeffF);
        }

      } catch {
        case e: NumberFormatException =>
          println("Number format exception in MoonTable1");
      }
    }
    //println("sigl: " + sigl);
    //println("sigr: " + sigr);


    for(line <- Source.fromFile("src/benchmarks/moon/Moontable2.rdb").getLines().drop(3)) {
      ll = line.split("\t");

      try {
        coeffD = Integer.parseInt(ll(0));
        coeffM = Integer.parseInt(ll(1));
        coeffMl = Integer.parseInt(ll(2));
        coeffF = Integer.parseInt(ll(3));
        coeffSigb = SF(java.lang.Double.parseDouble(ll(4)));

        E = SF(1.0)-0.002516*T-0.0000074*pow2(T);

        if ((coeffM == 1) || (coeffM == -1)){
          sigb = sigb + E * coeffSigb * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
        }

        else if ((coeffM == 2) || (coeffM == -2)){
          sigb = sigb + pow2(E) * coeffSigb * sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF);
        }

        else {
          sigb = sigb + coeffSigb * sin(D*coeffD+Ml*coeffMl+F*coeffF);
        }

      } catch {
        case e: NumberFormatException =>
          println("Number format exception in MoonTable2");
      }
    }
    //println("sigb: " + sigb);


    // Additional terms due to Jupiter, Venus and flatening of Earth:
    sigl = sigl + 3958.0*sin(A1)+1962.*sin(Ll-F)+318.0*sin(A2);
    sigb = sigb - 2235.0*sin(Ll)+382.0*sin(A3)+175.0*sin(A1-F)+
      175.0*sin(A1+F)+127.0*sin(Ll-Ml)-115.0*sin(Ll+Ml);

    //println("\nAfter correction by Jupiter and mates:")
    //println("sigl: " + sigl);
    //println("sigb: " + sigb);


    // Moon coordinates in degrees
    var lbda = (Ll*180/Pi + sigl/1000000.0);
    lbda = lbda%360.0;
    var beta = (sigb/1000000.0);
    beta=beta%360.0;

    // Distance Moon - center of earth (in KM):
    val GrdDelta = SF(385000.56) + sigr/1000.0;

    // Apparent lambda by adding to lambda the nutation in longitude:
    var L=SF(280.4665)+36000.7698*T;
    L = L%360.0;
    L = L*Pi/180.0;

    // Longitude of the mean ascending node
    var omega=SF(125.0445550)-1934.1361849*T+0.0020762*pow2(T)+pow3(T)/467410.0-
      pow4(T)/60616000.0;
    omega = omega%360.0;
    var dphi = SF(-17.20)/3600.0*sin(Pi*omega/180.0)-SF(1.32)/3600.0*sin(2.0*L)-
      SF(0.23)/3600.0*sin(2.0*Ll)+SF(0.21)/3600.0*sin(2.0*Pi*omega/180.0);
    dphi = dphi%360.0;
    var applbda = lbda + dphi;
    applbda = applbda%360.0;
    println("---> sm" + applbda)

    // True obliquity of the ecliptic
    val eps0 = (SF(23.0)+SF(26.0)/60.0+SF(21.448)/3600.0)-SF(46.8150)/3600.0*T-SF(0.00059)/3600.0*pow2(T)+
      SF(0.001813)/3600.0*pow3(T);
    val deps = SF(9.20)/3600.0*cos(Pi*omega/180.0)+SF(0.57)/3600.0*cos(2.0*L)+
      SF(0.10)/3600.0*cos(2.0*Ll)-SF(0.09)/3600.0*cos(2*Pi*omega/180.0);
    var eps = eps0 + deps;
    println("eps: " + eps)
    eps = eps % 360.0;

    val alpha = darctan((sin(applbda*Pi/180.0)*cos(eps*Pi/180.0)-
          tan(beta*Pi/180.0)*sin(eps*Pi/180.0)),cos(applbda*Pi/180.0));
    var delta = asin(sin(beta*Pi/180.0)*cos(eps*Pi/180.0)+
        cos(beta*Pi/180.0)*sin(eps*Pi/180.0)*sin(applbda*Pi/180.0));
    delta = delta*180.0/Pi;

    val halpha = (alpha/15.0).toInt;
    val mnalpha = ((alpha/15.0-halpha)*60.0).toInt;
    val salpha = ((alpha/15.0-halpha)*60.0-mnalpha)*60.0;

    if (delta>90.0){
      delta = delta-360.0;
    }
    else {
      delta = 1.0*delta;
    }
    println("Alpha: " + alpha + "   " + alpha.interval)
    println("Delta: " + delta + "   " + delta.interval)
    val ddelta = (delta).toInt;
    val mndelta = abs(((delta-ddelta)*60.0).toInt);
    val sdelta = abs(((delta-ddelta)*60.-((delta-ddelta)*60.).toInt)*60.0);

    println("Moon coordinates:");
    println("Alpha = %sh %sm %s ".format(halpha.toString, mnalpha.toString, salpha.toString))
    println("Delta = %sd %sm %s ".format(ddelta.toString, mndelta.toString, sdelta.toString))
    return (halpha, mnalpha, salpha, ddelta, mndelta, sdelta)
  }

  def darctan(y: SF, x: SF): SF = {
    val Pi = SF(3.141592653589793)
    if((x == 0) && (y == 0)){
      return SF(0.0);
    }

    else if ((y < 0.0) && (x > 0.0)){
      return (atan(y/x)*180.0/Pi)+360.0;
    }

    else if (x < 0.0){
      return (atan(y/x)*180.0/Pi)+180.0;
    }

    else {
      return atan(y/x)*180.0/Pi;
    }
  }

}