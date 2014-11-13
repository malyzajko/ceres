ceres
=====
Useful stuff for certified numerical results in Scala





Compiling native code for Directed Rounding
-------------------------------
Compile the native library in /resources.

On Ubuntu this is something like:
gcc -shared -fPIC -I/usr/lib/jvm/java-7-openjdk-amd64/include/
  -I/usr/lib/jvm/java-7-openjdk-amd64/include/linux/
  -o libDirectedRounding.so ceres_DirectedRounding.c

On Mac:
gcc -m64 -I/System/Library/Frameworks/JavaVM.framework/Headers
     -c ceres_DirectedRounding.c
gcc -m64 -dynamiclib -o libDirectedRounding.jnilib ceres_DirectedRounding.o

Make sure the library is called libDirectedRounding.*


Compiling the quad-double interface
-------------------------------
In order to use the type QuadDouble, you need to download and build this library.
Compile the provided JNI interface (also in `resources/`) for your architecture.

To compile on Linux:

    g++ -fPIC -I/usr/lib/jvm/java-7-openjdk-amd64/include/
      -I/usr/lib/jvm/java-7-openjdk-amd64/include/linux/ -I/home/edarulov/share/qd/include
      -c ceres_common_QuadDoubleInterface.cpp

    g++ -shared -o libQuadDouble.so ceres_common_QuadDoubleInterface.o ~/share/qd/src/*.o

