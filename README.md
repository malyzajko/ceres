ceres
=====
Useful stuff for certified numerical results in Scala





Compiling native code for Directed Rounding
-------------------------------
On Ubuntu this is something like:
gcc -shared -fPIC -I/usr/lib/jvm/java-7-openjdk-amd64/include/
  -I/usr/lib/jvm/java-7-openjdk-amd64/include/linux/
  -o libDirectedRounding.so ceres_common_DirectedRounding.c

On Mac:
gcc -m64 -I/System/Library/Frameworks/JavaVM.framework/Headers
     -c ceres_common_DirectedRounding.c
gcc -m64 -dynamiclib -o libDirectedRounding.jnilib ceres_common_DirectedRounding.o

Make sure the library is called libDirectedRounding.*

