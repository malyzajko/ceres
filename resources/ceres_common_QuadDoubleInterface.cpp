#include "ceres_common_QuadDoubleInterface.h"
#include <qd/qd_real.h>
#include <qd/fpu.h>

JNIEXPORT jlong JNICALL Java_ceres_common_QuadDoubleInterface_fpuFixStart
  (JNIEnv *env, jclass cls) {
  unsigned int old;
  fpu_fix_start(&old);
  return old;
}

JNIEXPORT void JNICALL Java_ceres_common_QuadDoubleInterface_fpuFixEnd
  (JNIEnv *env, jclass cls, jlong old) {
  unsigned int old_c = (unsigned int) old;
  fpu_fix_end(&old_c);
}


JNIEXPORT jdoubleArray JNICALL Java_ceres_common_QuadDoubleInterface_add
  (JNIEnv *env, jclass cls, jdouble x0, jdouble x1, jdouble x2, jdouble x3,
   jdouble y0, jdouble y1, jdouble y2, jdouble y3) {

  jboolean isCopy;
  jdoubleArray result = env -> NewDoubleArray(4);
  jdouble* arrayElems = env -> GetDoubleArrayElements(result, &isCopy);

  qd_real x = qd_real(x0, x1, x2, x3);
  qd_real y = qd_real(y0, y1, y2, y3);
  qd_real z = x + y;

  arrayElems[0] = z[0];
  arrayElems[1] = z[1];
  arrayElems[2] = z[2];
  arrayElems[3] = z[3];

  // Release elements, or result will not be copied back
  if (isCopy == JNI_TRUE) {
    env -> ReleaseDoubleArrayElements(result, arrayElems, 0);
  }
  return result;
}

JNIEXPORT jdoubleArray JNICALL Java_ceres_common_QuadDoubleInterface_sub
  (JNIEnv *env, jclass cls, jdouble x0, jdouble x1, jdouble x2, jdouble x3,
   jdouble y0, jdouble y1, jdouble y2, jdouble y3) {
  jboolean isCopy;
  jdoubleArray result = env -> NewDoubleArray(4);
  jdouble* arrayElems = env -> GetDoubleArrayElements(result, &isCopy);

  qd_real x = qd_real(x0, x1, x2, x3);
  qd_real y = qd_real(y0, y1, y2, y3);
  qd_real z = x - y;

  arrayElems[0] = z[0];
  arrayElems[1] = z[1];
  arrayElems[2] = z[2];
  arrayElems[3] = z[3];

  // Release elements, or result will not be copied back
  if (isCopy == JNI_TRUE) {
    env -> ReleaseDoubleArrayElements(result, arrayElems, 0);
  }
  return result;

}

JNIEXPORT jdoubleArray JNICALL Java_ceres_common_QuadDoubleInterface_mult
  (JNIEnv *env, jclass cls, jdouble x0, jdouble x1, jdouble x2, jdouble x3,
   jdouble y0, jdouble y1, jdouble y2, jdouble y3) {
  jboolean isCopy;
  jdoubleArray result = env -> NewDoubleArray(4);
  jdouble* arrayElems = env -> GetDoubleArrayElements(result, &isCopy);

  qd_real x = qd_real(x0, x1, x2, x3);
  qd_real y = qd_real(y0, y1, y2, y3);
  qd_real z = x * y;

  arrayElems[0] = z[0];
  arrayElems[1] = z[1];
  arrayElems[2] = z[2];
  arrayElems[3] = z[3];

  // Release elements, or result will not be copied back
  if (isCopy == JNI_TRUE) {
    env -> ReleaseDoubleArrayElements(result, arrayElems, 0);
  }
  return result;

}

JNIEXPORT jdoubleArray JNICALL Java_ceres_common_QuadDoubleInterface_div
  (JNIEnv *env, jclass cls, jdouble x0, jdouble x1, jdouble x2, jdouble x3,
   jdouble y0, jdouble y1, jdouble y2, jdouble y3) {
  jboolean isCopy;
  jdoubleArray result = env -> NewDoubleArray(4);
  jdouble* arrayElems = env -> GetDoubleArrayElements(result, &isCopy);

  qd_real x = qd_real(x0, x1, x2, x3);
  qd_real y = qd_real(y0, y1, y2, y3);
  qd_real z = x / y;

  arrayElems[0] = z[0];
  arrayElems[1] = z[1];
  arrayElems[2] = z[2];
  arrayElems[3] = z[3];

  // Release elements, or result will not be copied back
  if (isCopy == JNI_TRUE) {
    env -> ReleaseDoubleArrayElements(result, arrayElems, 0);
  }
  return result;
}

JNIEXPORT jdoubleArray JNICALL Java_ceres_common_QuadDoubleInterface_sqrt
  (JNIEnv *env, jclass cls, jdouble x0, jdouble x1, jdouble x2, jdouble x3) {
  jboolean isCopy;
  jdoubleArray result = env -> NewDoubleArray(4);
  jdouble* arrayElems = env -> GetDoubleArrayElements(result, &isCopy);

  qd_real x = qd_real(x0, x1, x2, x3);
  qd_real z = sqrt(x);

  arrayElems[0] = z[0];
  arrayElems[1] = z[1];
  arrayElems[2] = z[2];
  arrayElems[3] = z[3];

  // Release elements, or result will not be copied back
  if (isCopy == JNI_TRUE) {
    env -> ReleaseDoubleArrayElements(result, arrayElems, 0);
  }
  return result;
}


JNIEXPORT jdoubleArray JNICALL Java_ceres_common_QuadDoubleInterface_log
  (JNIEnv *env, jclass cls, jdouble x0, jdouble x1, jdouble x2, jdouble x3) {
  jboolean isCopy;
  jdoubleArray result = env -> NewDoubleArray(4);
  jdouble* arrayElems = env -> GetDoubleArrayElements(result, &isCopy);

  qd_real x = qd_real(x0, x1, x2, x3);
  qd_real z = log(x);

  arrayElems[0] = z[0];
  arrayElems[1] = z[1];
  arrayElems[2] = z[2];
  arrayElems[3] = z[3];

  // Release elements, or result will not be copied back
  if (isCopy == JNI_TRUE) {
    env -> ReleaseDoubleArrayElements(result, arrayElems, 0);
  }
  return result;
}

JNIEXPORT jdoubleArray JNICALL Java_ceres_common_QuadDoubleInterface_exp
  (JNIEnv *env, jclass cls, jdouble x0, jdouble x1, jdouble x2, jdouble x3) {
  jboolean isCopy;
  jdoubleArray result = env -> NewDoubleArray(4);
  jdouble* arrayElems = env -> GetDoubleArrayElements(result, &isCopy);

  qd_real x = qd_real(x0, x1, x2, x3);
  qd_real z = exp(x);

  arrayElems[0] = z[0];
  arrayElems[1] = z[1];
  arrayElems[2] = z[2];
  arrayElems[3] = z[3];

  // Release elements, or result will not be copied back
  if (isCopy == JNI_TRUE) {
    env -> ReleaseDoubleArrayElements(result, arrayElems, 0);
  }
  return result;
}

JNIEXPORT jdoubleArray JNICALL Java_ceres_common_QuadDoubleInterface_pow
  (JNIEnv *env, jclass cls, jdouble x0, jdouble x1, jdouble x2, jdouble x3,
   jdouble y0, jdouble y1, jdouble y2, jdouble y3) {
  jboolean isCopy;
  jdoubleArray result = env -> NewDoubleArray(4);
  jdouble* arrayElems = env -> GetDoubleArrayElements(result, &isCopy);

  qd_real x = qd_real(x0, x1, x2, x3);
  qd_real y = qd_real(y0, y1, y2, y3);
  qd_real z = pow(x, y);

  arrayElems[0] = z[0];
  arrayElems[1] = z[1];
  arrayElems[2] = z[2];
  arrayElems[3] = z[3];

  // Release elements, or result will not be copied back
  if (isCopy == JNI_TRUE) {
    env -> ReleaseDoubleArrayElements(result, arrayElems, 0);
  }
  return result;
}

JNIEXPORT jdoubleArray JNICALL Java_ceres_common_QuadDoubleInterface_cos
  (JNIEnv *env, jclass cls, jdouble x0, jdouble x1, jdouble x2, jdouble x3) {
  jboolean isCopy;
  jdoubleArray result = env -> NewDoubleArray(4);
  jdouble* arrayElems = env -> GetDoubleArrayElements(result, &isCopy);

  qd_real x = qd_real(x0, x1, x2, x3);
  qd_real z = cos(x);

  arrayElems[0] = z[0];
  arrayElems[1] = z[1];
  arrayElems[2] = z[2];
  arrayElems[3] = z[3];

  // Release elements, or result will not be copied back
  if (isCopy == JNI_TRUE) {
    env -> ReleaseDoubleArrayElements(result, arrayElems, 0);
  }
  return result;
}

JNIEXPORT jdoubleArray JNICALL Java_ceres_common_QuadDoubleInterface_sin
  (JNIEnv *env, jclass cls, jdouble x0, jdouble x1, jdouble x2, jdouble x3) {
  jboolean isCopy;
  jdoubleArray result = env -> NewDoubleArray(4);
  jdouble* arrayElems = env -> GetDoubleArrayElements(result, &isCopy);

  qd_real x = qd_real(x0, x1, x2, x3);
  qd_real z = sin(x);

  arrayElems[0] = z[0];
  arrayElems[1] = z[1];
  arrayElems[2] = z[2];
  arrayElems[3] = z[3];

  // Release elements, or result will not be copied back
  if (isCopy == JNI_TRUE) {
    env -> ReleaseDoubleArrayElements(result, arrayElems, 0);
  }
  return result;
}

JNIEXPORT jdoubleArray JNICALL Java_ceres_common_QuadDoubleInterface_tan
  (JNIEnv *env, jclass cls, jdouble x0, jdouble x1, jdouble x2, jdouble x3) {
  jboolean isCopy;
  jdoubleArray result = env -> NewDoubleArray(4);
  jdouble* arrayElems = env -> GetDoubleArrayElements(result, &isCopy);

  qd_real x = qd_real(x0, x1, x2, x3);
  qd_real z = tan(x);

  arrayElems[0] = z[0];
  arrayElems[1] = z[1];
  arrayElems[2] = z[2];
  arrayElems[3] = z[3];

  // Release elements, or result will not be copied back
  if (isCopy == JNI_TRUE) {
    env -> ReleaseDoubleArrayElements(result, arrayElems, 0);
  }
  return result;
}

JNIEXPORT jdoubleArray JNICALL Java_ceres_common_QuadDoubleInterface_acos
  (JNIEnv *env, jclass cls, jdouble x0, jdouble x1, jdouble x2, jdouble x3) {
  jboolean isCopy;
  jdoubleArray result = env -> NewDoubleArray(4);
  jdouble* arrayElems = env -> GetDoubleArrayElements(result, &isCopy);

  qd_real x = qd_real(x0, x1, x2, x3);
  qd_real z = acos(x);

  arrayElems[0] = z[0];
  arrayElems[1] = z[1];
  arrayElems[2] = z[2];
  arrayElems[3] = z[3];

  // Release elements, or result will not be copied back
  if (isCopy == JNI_TRUE) {
    env -> ReleaseDoubleArrayElements(result, arrayElems, 0);
  }
  return result;
}

JNIEXPORT jdoubleArray JNICALL Java_ceres_common_QuadDoubleInterface_asin
  (JNIEnv *env, jclass cls, jdouble x0, jdouble x1, jdouble x2, jdouble x3) {
  jboolean isCopy;
  jdoubleArray result = env -> NewDoubleArray(4);
  jdouble* arrayElems = env -> GetDoubleArrayElements(result, &isCopy);

  qd_real x = qd_real(x0, x1, x2, x3);
  qd_real z = asin(x);

  arrayElems[0] = z[0];
  arrayElems[1] = z[1];
  arrayElems[2] = z[2];
  arrayElems[3] = z[3];

  // Release elements, or result will not be copied back
  if (isCopy == JNI_TRUE) {
    env -> ReleaseDoubleArrayElements(result, arrayElems, 0);
  }
  return result;
}

JNIEXPORT jdoubleArray JNICALL Java_ceres_common_QuadDoubleInterface_atan
  (JNIEnv *env, jclass cls, jdouble x0, jdouble x1, jdouble x2, jdouble x3) {
  jboolean isCopy;
  jdoubleArray result = env -> NewDoubleArray(4);
  jdouble* arrayElems = env -> GetDoubleArrayElements(result, &isCopy);

  qd_real x = qd_real(x0, x1, x2, x3);
  qd_real z = atan(x);

  arrayElems[0] = z[0];
  arrayElems[1] = z[1];
  arrayElems[2] = z[2];
  arrayElems[3] = z[3];

  // Release elements, or result will not be copied back
  if (isCopy == JNI_TRUE) {
    env -> ReleaseDoubleArrayElements(result, arrayElems, 0);
  }
  return result;
}

JNIEXPORT jstring JNICALL Java_ceres_common_QuadDoubleInterface_toString
  (JNIEnv *env, jclass cls, jint digits, jdouble x0, jdouble x1, jdouble x2, jdouble x3) {
  qd_real x = qd_real(x0, x1, x2, x3);
  std::string x_string = x.to_string(digits, 0, std::ios_base::fixed);
  const char * c_string = x_string.c_str();
  return env -> NewStringUTF(c_string);
}

