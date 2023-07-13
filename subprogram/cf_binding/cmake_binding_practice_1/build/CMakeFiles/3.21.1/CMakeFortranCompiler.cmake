set(CMAKE_Fortran_COMPILER "/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/bin/intel64/ifort")
set(CMAKE_Fortran_COMPILER_ARG1 "")
set(CMAKE_Fortran_COMPILER_ID "Intel")
set(CMAKE_Fortran_COMPILER_VERSION "18.0.3.20180410")
set(CMAKE_Fortran_COMPILER_WRAPPER "")
set(CMAKE_Fortran_PLATFORM_ID "Linux")
set(CMAKE_Fortran_SIMULATE_ID "")
set(CMAKE_Fortran_SIMULATE_VERSION "")




set(CMAKE_AR "/usr/bin/ar")
set(CMAKE_Fortran_COMPILER_AR "")
set(CMAKE_RANLIB "/usr/bin/ranlib")
set(CMAKE_Fortran_COMPILER_RANLIB "")
set(CMAKE_COMPILER_IS_GNUG77 )
set(CMAKE_Fortran_COMPILER_LOADED 1)
set(CMAKE_Fortran_COMPILER_WORKS TRUE)
set(CMAKE_Fortran_ABI_COMPILED TRUE)
set(CMAKE_COMPILER_IS_MINGW )
set(CMAKE_COMPILER_IS_CYGWIN )
if(CMAKE_COMPILER_IS_CYGWIN)
  set(CYGWIN 1)
  set(UNIX 1)
endif()

set(CMAKE_Fortran_COMPILER_ENV_VAR "FC")

set(CMAKE_Fortran_COMPILER_SUPPORTS_F90 1)

if(CMAKE_COMPILER_IS_MINGW)
  set(MINGW 1)
endif()
set(CMAKE_Fortran_COMPILER_ID_RUN 1)
set(CMAKE_Fortran_SOURCE_FILE_EXTENSIONS f;F;fpp;FPP;f77;F77;f90;F90;for;For;FOR;f95;F95)
set(CMAKE_Fortran_IGNORE_EXTENSIONS h;H;o;O;obj;OBJ;def;DEF;rc;RC)
set(CMAKE_Fortran_LINKER_PREFERENCE 20)
if(UNIX)
  set(CMAKE_Fortran_OUTPUT_EXTENSION .o)
else()
  set(CMAKE_Fortran_OUTPUT_EXTENSION .obj)
endif()

# Save compiler ABI information.
set(CMAKE_Fortran_SIZEOF_DATA_PTR "8")
set(CMAKE_Fortran_COMPILER_ABI "ELF")
set(CMAKE_Fortran_LIBRARY_ARCHITECTURE "")

if(CMAKE_Fortran_SIZEOF_DATA_PTR AND NOT CMAKE_SIZEOF_VOID_P)
  set(CMAKE_SIZEOF_VOID_P "${CMAKE_Fortran_SIZEOF_DATA_PTR}")
endif()

if(CMAKE_Fortran_COMPILER_ABI)
  set(CMAKE_INTERNAL_PLATFORM_ABI "${CMAKE_Fortran_COMPILER_ABI}")
endif()

if(CMAKE_Fortran_LIBRARY_ARCHITECTURE)
  set(CMAKE_LIBRARY_ARCHITECTURE "")
endif()





set(CMAKE_Fortran_IMPLICIT_INCLUDE_DIRECTORIES "/shared/ucl/apps/openblas/0.3.7-serial/gnu-4.9.2/include;/shared/ucl/apps/python/3.8.6/gnu-4.9.2/include;/shared/ucl/apps/gsl/2.4/intel-2017/include;/shared/ucl/apps/intel/2018.Update3/impi/2018.3.222/include64;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/daal/include;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/tbb/include;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/mkl/include;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/ipp/include;/shared/ucl/apps/emacs/28.1/include;/shared/ucl/apps/giflib/5.1.1/gnu-4.9.2/include;/shared/ucl/apps/apr-util/1.6.1/include;/shared/ucl/apps/apr/1.7.0/include;/shared/ucl/apps/flex/2.5.39/gnu-4.9.2/include;/lustre/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/compiler/include/intel64;/lustre/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/compiler/include/icc;/lustre/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/compiler/include;/lustre/shared/ucl/apps/gcc/4.9.2/lib/gcc/x86_64-unknown-linux-gnu/4.9.2/include;/lustre/shared/ucl/apps/gcc/4.9.2/lib/gcc/x86_64-unknown-linux-gnu/4.9.2/include-fixed;/lustre/shared/ucl/apps/gcc/4.9.2/include;/usr/include")
set(CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES "ifport;ifcoremt;imf;svml;m;ipgo;irc;pthread;svml;c;gcc;gcc_s;irc_s;dl;c")
set(CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES "/shared/ucl/apps/openblas/0.3.7-serial/gnu-4.9.2/lib;/shared/ucl/apps/python/3.8.6/gnu-4.9.2/lib;/shared/ucl/apps/gsl/2.4/intel-2017/lib;/shared/ucl/apps/intel/2018.Update3/impi/2018.3.222/intel64/lib;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64_lin;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/tbb/lib/intel64_lin/gcc4.4;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/daal/lib/intel64_lin;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/tbb/lib/intel64/gcc4.4;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/mkl/lib/intel64;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/ipp/lib/intel64;/shared/ucl/apps/emacs/28.1/lib;/shared/ucl/apps/giflib/5.1.1/gnu-4.9.2/lib;/shared/ucl/apps/subversion/1.14.1/lib;/shared/ucl/apps/apr-util/1.6.1/lib;/shared/ucl/apps/apr/1.7.0/lib;/shared/ucl/apps/git/2.32.0/gnu-4.9.2/lib64;/shared/ucl/apps/flex/2.5.39/gnu-4.9.2/lib;/shared/ucl/apps/gcc/4.9.2/lib;/shared/ucl/apps/gcc/4.9.2/lib64;/lustre/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64_lin;/lustre/shared/ucl/apps/gcc/4.9.2/lib/gcc/x86_64-unknown-linux-gnu/4.9.2;/lustre/shared/ucl/apps/gcc/4.9.2/lib/gcc;/lustre/shared/ucl/apps/gcc/4.9.2/lib64;/lib64;/usr/lib64;/lustre/shared/ucl/apps/gcc/4.9.2/lib;/lib;/usr/lib")
set(CMAKE_Fortran_IMPLICIT_LINK_FRAMEWORK_DIRECTORIES "")
