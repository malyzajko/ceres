set JDK="c:\Program Files\Java\jdk1.8.0_31"
:: on Windows, the JNI DLL name should not have a "lib" prefix
gcc -shared -I%JDK%\include -I%JDK%\include\win32 -o lib\DirectedRounding.dll resources\ceres_DirectedRounding.c
