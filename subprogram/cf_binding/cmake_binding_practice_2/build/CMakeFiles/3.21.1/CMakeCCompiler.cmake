set(CMAKE_C_COMPILER "/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/bin/intel64/icc")
set(CMAKE_C_COMPILER_ARG1 "")
set(CMAKE_C_COMPILER_ID "Intel")
set(CMAKE_C_COMPILER_VERSION "18.0.3.20180410")
set(CMAKE_C_COMPILER_VERSION_INTERNAL "")
set(CMAKE_C_COMPILER_WRAPPER "")
set(CMAKE_C_STANDARD_COMPUTED_DEFAULT "90")
set(CMAKE_C_COMPILE_FEATURES "c_std_90;c_function_prototypes;c_std_99;c_restrict;c_variadic_macros;c_std_11;c_static_assert")
set(CMAKE_C90_COMPILE_FEATURES "c_std_90;c_function_prototypes")
set(CMAKE_C99_COMPILE_FEATURES "c_std_99;c_restrict;c_variadic_macros")
set(CMAKE_C11_COMPILE_FEATURES "c_std_11;c_static_assert")
set(CMAKE_C17_COMPILE_FEATURES "")
set(CMAKE_C23_COMPILE_FEATURES "")

set(CMAKE_C_PLATFORM_ID "Linux")
set(CMAKE_C_SIMULATE_ID "GNU")
set(CMAKE_C_COMPILER_FRONTEND_VARIANT "")
set(CMAKE_C_SIMULATE_VERSION "4.9.2")




set(CMAKE_AR "/usr/bin/ar")
set(CMAKE_C_COMPILER_AR "")
set(CMAKE_RANLIB "/usr/bin/ranlib")
set(CMAKE_C_COMPILER_RANLIB "")
set(CMAKE_LINKER "/usr/bin/ld")
set(CMAKE_MT "")
set(CMAKE_COMPILER_IS_GNUCC )
set(CMAKE_C_COMPILER_LOADED 1)
set(CMAKE_C_COMPILER_WORKS TRUE)
set(CMAKE_C_ABI_COMPILED TRUE)
set(CMAKE_COMPILER_IS_MINGW )
set(CMAKE_COMPILER_IS_CYGWIN )
if(CMAKE_COMPILER_IS_CYGWIN)
  set(CYGWIN 1)
  set(UNIX 1)
endif()

set(CMAKE_C_COMPILER_ENV_VAR "CC")

if(CMAKE_COMPILER_IS_MINGW)
  set(MINGW 1)
endif()
set(CMAKE_C_COMPILER_ID_RUN 1)
set(CMAKE_C_SOURCE_FILE_EXTENSIONS c;m)
set(CMAKE_C_IGNORE_EXTENSIONS h;H;o;O;obj;OBJ;def;DEF;rc;RC)
set(CMAKE_C_LINKER_PREFERENCE 10)

# Save compiler ABI information.
set(CMAKE_C_SIZEOF_DATA_PTR "8")
set(CMAKE_C_COMPILER_ABI "ELF")
set(CMAKE_C_BYTE_ORDER "LITTLE_ENDIAN")
set(CMAKE_C_LIBRARY_ARCHITECTURE "")

if(CMAKE_C_SIZEOF_DATA_PTR)
  set(CMAKE_SIZEOF_VOID_P "${CMAKE_C_SIZEOF_DATA_PTR}")
endif()

if(CMAKE_C_COMPILER_ABI)
  set(CMAKE_INTERNAL_PLATFORM_ABI "${CMAKE_C_COMPILER_ABI}")
endif()

if(CMAKE_C_LIBRARY_ARCHITECTURE)
  set(CMAKE_LIBRARY_ARCHITECTURE "")
endif()

set(CMAKE_C_CL_SHOWINCLUDES_PREFIX "")
if(CMAKE_C_CL_SHOWINCLUDES_PREFIX)
  set(CMAKE_CL_SHOWINCLUDES_PREFIX "${CMAKE_C_CL_SHOWINCLUDES_PREFIX}")
endif()





set(CMAKE_C_IMPLICIT_INCLUDE_DIRECTORIES "/lustre/shared/ucl/apps/python/bundles/python38-5.0.0/venv/include;/shared/ucl/apps/openblas/0.3.7-serial/gnu-4.9.2/include;/shared/ucl/apps/python/3.8.6/gnu-4.9.2/include;/shared/ucl/apps/gsl/2.4/intel-2017/include;/shared/ucl/apps/intel/2018.Update3/impi/2018.3.222/include64;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/daal/include;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/tbb/include;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/mkl/include;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/ipp/include;/shared/ucl/apps/emacs/28.1/include;/shared/ucl/apps/giflib/5.1.1/gnu-4.9.2/include;/shared/ucl/apps/apr-util/1.6.1/include;/shared/ucl/apps/apr/1.7.0/include;/shared/ucl/apps/flex/2.5.39/gnu-4.9.2/include;/lustre/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/compiler/include/intel64;/lustre/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/compiler/include/icc;/lustre/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/compiler/include;/lustre/shared/ucl/apps/gcc/4.9.2/lib/gcc/x86_64-unknown-linux-gnu/4.9.2/include;/lustre/shared/ucl/apps/gcc/4.9.2/lib/gcc/x86_64-unknown-linux-gnu/4.9.2/include-fixed;/lustre/shared/ucl/apps/gcc/4.9.2/include;/usr/include")
set(CMAKE_C_IMPLICIT_LINK_LIBRARIES "imf;svml;irng;m;ipgo;decimal;cilkrts;stdc++;gcc;gcc_s;irc;svml;c;gcc;gcc_s;irc_s;dl;c")
set(CMAKE_C_IMPLICIT_LINK_DIRECTORIES "/shared/ucl/apps/openblas/0.3.7-serial/gnu-4.9.2/lib;/shared/ucl/apps/python/3.8.6/gnu-4.9.2/lib;/shared/ucl/apps/gsl/2.4/intel-2017/lib;/shared/ucl/apps/intel/2018.Update3/impi/2018.3.222/intel64/lib;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64_lin;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/tbb/lib/intel64_lin/gcc4.4;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/daal/lib/intel64_lin;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/tbb/lib/intel64/gcc4.4;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/mkl/lib/intel64;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64;/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/ipp/lib/intel64;/shared/ucl/apps/emacs/28.1/lib;/shared/ucl/apps/giflib/5.1.1/gnu-4.9.2/lib;/shared/ucl/apps/subversion/1.14.1/lib;/shared/ucl/apps/apr-util/1.6.1/lib;/shared/ucl/apps/apr/1.7.0/lib;/shared/ucl/apps/git/2.32.0/gnu-4.9.2/lib64;/shared/ucl/apps/flex/2.5.39/gnu-4.9.2/lib;/shared/ucl/apps/gcc/4.9.2/lib;/shared/ucl/apps/gcc/4.9.2/lib64;/lustre/shared/ucl/apps/intel/2018.Update3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64_lin;/lustre/shared/ucl/apps/gcc/4.9.2/lib/gcc/x86_64-unknown-linux-gnu/4.9.2;/lustre/shared/ucl/apps/gcc/4.9.2/lib/gcc;/lustre/shared/ucl/apps/gcc/4.9.2/lib64;/lib64;/usr/lib64;/lustre/shared/ucl/apps/gcc/4.9.2/lib;/lib;/usr/lib")
set(CMAKE_C_IMPLICIT_LINK_FRAMEWORK_DIRECTORIES "")
