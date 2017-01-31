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


Installation instructions for Windows
-------------------------------------
1. install gcc
2. install java development kit (JDK)
   http://www.oracle.com/technetwork/java/javase/downloads/index.html
3. add javac to the system path:
   C:\Program Files\Java\jdk1.8.0\bin
4. install sbt
   http://www.scala-sbt.org/download.html
5. fork+clone ceres from github
6. start the Git Shell
7. go to the ceres folder
8. type "cmd" to start a windows shell with the correct paths.
   - type "build" to build the JNI libs for windows (using build.bat)
   - type "exit" to leave the windows shell
9. type "sbt". this will start the sbt console.
10. type "run". this will compile and run the project.
