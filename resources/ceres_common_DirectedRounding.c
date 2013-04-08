#include "ceres_common_DirectedRounding.h"
#include <fenv.h>
#include <assert.h>
#include <math.h>

#pragma STDC FENV_ACCESS ON

JNIEXPORT jdouble JNICALL Java_ceres_common_DirectedRounding_addUp
  (JNIEnv *env, jclass cls, jdouble a, jdouble b) {
    int save_round;
    int setround_ok;
    save_round = fegetround();
    setround_ok = fesetround(FE_UPWARD);
    assert(setround_ok == 0);

    double result = a + b;

    fesetround(save_round);
    return result;
}



JNIEXPORT jdouble JNICALL Java_ceres_common_DirectedRounding_addDown
  (JNIEnv *env, jclass cls, jdouble a, jdouble b) {
    int save_round;
    int setround_ok;
    save_round = fegetround();
    setround_ok = fesetround(FE_DOWNWARD);
    assert(setround_ok == 0);

    double result = a + b;

    fesetround(save_round);
    return result;
}

JNIEXPORT jdouble JNICALL Java_ceres_common_DirectedRounding_subUp
  (JNIEnv *env, jclass cls, jdouble a, jdouble b) {
    int save_round;
    int setround_ok;
    save_round = fegetround();
    setround_ok = fesetround(FE_UPWARD);
    assert(setround_ok == 0);

    double result = a - b;

    fesetround(save_round);
    return result;
}

JNIEXPORT jdouble JNICALL Java_ceres_common_DirectedRounding_subDown
  (JNIEnv *env, jclass cls, jdouble a, jdouble b) {
    int save_round;
    int setround_ok;
    save_round = fegetround();
    setround_ok = fesetround(FE_DOWNWARD);
    assert(setround_ok == 0);

    double result = a - b;

    fesetround(save_round);
    return result;
}

JNIEXPORT jdouble JNICALL Java_ceres_common_DirectedRounding_multUp
  (JNIEnv *env, jclass cls, jdouble a, jdouble b) {
    int save_round;
    int setround_ok;
    save_round = fegetround();
    setround_ok = fesetround(FE_UPWARD);
    assert(setround_ok == 0);

    double result = a * b;

    fesetround(save_round);
    return result;
}

JNIEXPORT jdouble JNICALL Java_ceres_common_DirectedRounding_multDown
  (JNIEnv *env, jclass cls, jdouble a, jdouble b) {
    int save_round;
    int setround_ok;
    save_round = fegetround();
    setround_ok = fesetround(FE_DOWNWARD);
    assert(setround_ok == 0);

    double result = a * b;

    fesetround(save_round);
    return result;
}

JNIEXPORT jdouble JNICALL Java_ceres_common_DirectedRounding_divUp
  (JNIEnv *env, jclass cls, jdouble a, jdouble b) {
    int save_round;
    int setround_ok;
    save_round = fegetround();
    setround_ok = fesetround(FE_UPWARD);
    assert(setround_ok == 0);

    double result = a / b;

    fesetround(save_round);
    return result;
}


JNIEXPORT jdouble JNICALL Java_ceres_common_DirectedRounding_divDown
  (JNIEnv *env, jclass cls, jdouble a, jdouble b) {
    int save_round;
    int setround_ok;
    save_round = fegetround();
    setround_ok = fesetround(FE_DOWNWARD);
    assert(setround_ok == 0);

    double result = a / b;

    fesetround(save_round);
    return result;
}

JNIEXPORT jdouble JNICALL Java_ceres_common_DirectedRounding_sqrtDown
  (JNIEnv *env, jclass cls, jdouble a) {
    int save_round;
    int setround_ok;
    save_round = fegetround();
    setround_ok = fesetround(FE_DOWNWARD);
    assert(setround_ok == 0);

    double result = sqrt(a);

    fesetround(save_round);
    return result;
}

JNIEXPORT jdouble JNICALL Java_ceres_common_DirectedRounding_sqrtUp
  (JNIEnv *env, jclass cls, jdouble a) {
    int save_round;
    int setround_ok;
    save_round = fegetround();
    setround_ok = fesetround(FE_UPWARD);
    assert(setround_ok == 0);

    double result = sqrt(a);

    fesetround(save_round);
    return result;
}



JNIEXPORT jdouble JNICALL Java_ceres_common_DirectedRounding_addRoundoff
  (JNIEnv *env, jclass cls, jdouble delta, jdouble zi, jdouble ai, jdouble bi) {
    int save_round;
    int setround_ok;
    save_round = fegetround();
    setround_ok = fesetround(FE_UPWARD);
    assert(setround_ok == 0);
    double x = bi-zi;
    double y = zi-ai;
    double max = ( x > y ) ? x : y;

    double result = delta + max;

    fesetround(save_round);
    return result;

  }


JNIEXPORT jdouble JNICALL Java_ceres_common_DirectedRounding_computeZeta
  (JNIEnv *env, jclass cls, jdouble dmin, jdouble dmax) {
    int save_round;
    int setround_ok;
    save_round = fegetround();
    setround_ok = fesetround(FE_DOWNWARD);
    assert(setround_ok == 0);
    double down = dmax/2.0;

    setround_ok = fesetround(FE_UPWARD);
    assert(setround_ok == 0);
    double up = dmin/2.0;

    setround_ok = fesetround(save_round);
    assert(setround_ok == 0);
    double result = up + down;

    return result;
  }

JNIEXPORT jdouble JNICALL Java_ceres_common_DirectedRounding_computeDelta
  (JNIEnv *env, jclass cls, jdouble zeta, jdouble dmin, jdouble dmax) {
    int save_round;
    int setround_ok;
    save_round = fegetround();
    setround_ok = fesetround(FE_UPWARD);
    assert(setround_ok == 0);
    double x = zeta-dmin;
    double y = dmax-zeta;
    double result = ( x > y ) ? x : y;

    fesetround(save_round);
    return result;
  }


JNIEXPORT jdouble JNICALL Java_ceres_common_DirectedRounding_addRoundoffAtimesBplusC
  (JNIEnv *env, jclass cls, jdouble delta, jdouble a, jdouble b, jdouble c) {
    int save_round;
    int setround_ok;
    save_round = fegetround(); //assume default is rd to nearest!
    double zi = a * b + c;

    setround_ok = fesetround(FE_DOWNWARD);
    assert(setround_ok == 0);
    double ai = a * b + c;

    setround_ok = fesetround(FE_UPWARD);
    assert(setround_ok == 0);
    double bi = a * b + c;

    double x = bi-zi;
    double y = zi-ai;
    double max = ( x > y ) ? x : y;

    double result = delta + max;

    fesetround(save_round);
    return result;
  }


JNIEXPORT jdouble JNICALL Java_ceres_common_DirectedRounding_addRoundoffABplusCD
  (JNIEnv *env, jclass cls, jdouble delta, jdouble a, jdouble b, jdouble c, jdouble d) {
    int save_round;
    int setround_ok;
    save_round = fegetround(); //assume default is rd to nearest!
    double zi = a * b + c * d;

    setround_ok = fesetround(FE_DOWNWARD);
    assert(setround_ok == 0);
    double ai = a * b + c * d;

    setround_ok = fesetround(FE_UPWARD);
    assert(setround_ok == 0);
    double bi = a * b + c * d;

    double x = bi-zi;
    double y = zi-ai;
    double max = ( x > y ) ? x : y;

    double result = delta + max;

    fesetround(save_round);
    return result;
  }
