# ---------------------------------------------------------------------------
#
#   Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
#   Affiliation:    University College London
#   Date:           2023.05.25 - 
#
# ---------------------------------------------------------------------------

cmake_minimum_required(VERSION 3.11)
project(KLMC3 VERSION 1.0.0 LANGUAGES C Fortran)

#
# USER SETTING
#
# USER SECTION ----------------------------------
# [1] setting GULP library root
set(COMPILER "CRAY") # POSSIBLE TYPES: "CRAY", "INTEL"
# [2] find GULP static library file
set(GULPROOT /work/e05/e05/wkjee/Software/gulpklmc/klmc3_tf_interface.update.12092023/KLMC3-libgulp-6.1.2-07.2024)
# [3] if using Python library root
set(ENABLE_PYTHON "FALSE") # POSSBILE TYPES: "TRUE", "FALSE"
set(PYTHONLIB /work/e05/e05/wkjee/miniconda3/lib/libpython3.11.so) # using custom env - miniconda
set(PYTHONINCLUDE /work/e05/e05/wkjee/miniconda3/include/python3.11/) # using custom env -miniconda
#set(PYTHONLIB /opt/cray/pe/python/3.9.13.1/lib/libpython3.so) # using central module - cray-python on ARCHER2
#set(PYTHONINCLUDE /opt/cray/pe/python/3.9.13.1/include/python3.9) # using central module - cray-python on ARCHER2

#
# [4] DEVELOPING PURPOSE USING SCOREP PROFILER ON ARCHER2
#
set(SCOREP "FALSE") # POSSIBLE TYPES: "TRUE", "FALSE". BEWARE TO USE SCOPEP, MUST KNOW HOW TO SET ENV & COMPILE GULPLIB
# USER SECTION END ------------------------------

#
# VARIABLE SETTING
#
# CMAKE DEFAULT VARIABLES (OR RESERVED)
#    CMAKE_C_COMPILER
#    CMAKE_Fortran_COMPILER
#    CMAKE_C_FLAGS
#    CMAKE_Fortran_FLAGS


#
# MPI SETTING
#
find_package(MPI REQUIRED)
include_directories(${MPI_INCLUDE_PATH})
if(MPI_FOUND)
	message(STATUS "MPI LIB FOUND")
	# RESERVED MPI CMAKE VARIABLES (ENVIRONMENTAL)
	message(STATUS "C COMPILER: ${MPI_CMAKE_C_COMPILER}")
	message(STATUS "FORTRAN COMPILER: ${MPI_Fortran_COMPILER}")
	#message(STATUS "C COMPILER FLAGS: ${MPI_C_COMPILE_FLAGS}")
	#message(STATUS "FORTRAN COMPILER FLAGS: ${MPI_Fortran_COMPILE_FLAGS}")
	#message(STATUS "C LINK FLAGS: ${MPI_C_LINK_FLAGS}")
	#message(STATUS "FORTRAN LINK FLAGS: ${MPI_Fortran_LINK_FLAGS}")
	#message(STATUS "C HEADER: ${MPI_C_INCLUDE_PATH}")
	#message(STATUS "FORTRAN HEADER: ${MPI_Fortran_INCLUDE_PATH}")
else()
	message(FATAL_ERROR "MPI LIB IS NOT FOUND!")
endif()

#   
# COMPILER SETTING
#   
if(COMPILER STREQUAL "CRAY")
	# SCOREP DEBUGGING MODE
	if(SCOREP STREQUAL "TRUE")
		set(CMAKE_C_COMPILER scorep-cc)
		set(CMAKE_Fortran_COMPILER scorep-ftn)
	else()
		set(CMAKE_C_COMPILER cc)
		set(CMAKE_Fortran_COMPILER ftn)
	endif()
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c11 -O3")
	set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -fallow-argument-mismatch -ffree-line-length-512")
elseif(COMPILER STREQUAL "INTEL")
	set(CMAKE_C_COMPILER mpicc)
	set(CMAKE_Fortran_COMPILER mpif90)
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c11 -O3 -mcmodel=large")
	set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3")
	# INTEL MKL/CORE SETTINGS: NO WHITESPACE ALLOWED FOR THE VARIABLES $LINK_MKL ...
	set(LINK_MKL "-lmkl_scalapack_lp64 -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lmkl_blacs_intelmpi_lp64 -lpthread -lm -ldl")
	set(LINK_CORE "-lifcore -lifport")
	message(STATUS "INTEL MKL LINK: ${LINK_MKL}")
	message(STATUS "INTEL CORE LINK: ${LINK_CORE}")
else()
	message(FATAL_ERROR "CORRECT COMPILER SETTING!")
endif()
message(STATUS "C COMPILER: ${CMAKE_C_COMPILER}")
message(STATUS "C COMPILER FLAGS: ${CMAKE_C_FLAGS}")
message(STATUS "FORTRAN COMPILER: ${CMAKE_Fortran_COMPILER}")
message(STATUS "FORTRAN COMPILER FLAGS: ${CMAKE_Fortran_FLAGS}")

#
# PYTHON SETTING
#
if(ENABLE_PYTHON STREQUAL "TRUE")
	message(STATUS "ENABLE_PYTHON FLAG: ${USE_PYTHON}")
	add_compile_definitions(USE_PYTHON)	# i.e., allow #ifdef USE_PYTHON ... #endif
	# CHECK PYTHON LIB
	if(EXISTS "${PYTHONLIB}")
		message(STATUS "PYTHON LIB PATH: ${PYTHONLIB}")
	else()
		message(FATAL_ERROR "PYTHON LIB DOES NOT EXISTS! PLEASE CHECK THE PATH ${PYTHONLIB}")
	endif()
	# CHECK PYTHON INCLUDE
	if(EXISTS "${PYTHONINCLUDE}")
		message(STATUS "PYTHON INLCUDE PATH: ${PYTHONINCLUDE}")
	else()
		message(FATAL_ERROR "PYTHON INCLUDE DOES NOT EXISTS! PLEASE CHECK THE PATH ${PYTHONINCLUDE}")
	endif()
endif()

#
# GULP SETTING: GULP OBJECT *.o *.mod FILES
#
set(GULPLIB "https://github.com/sweetmixture/KLMC3-libgulp-6.1.2.git")
set(GULPOBJ ${GULPROOT}/Src/Linux_MPI)
set(GULPGFNFF ${GULPROOT}/Utils/pGFNFF/Src)
if(EXISTS "${GULPROOT}")
	message(STATUS "GULPROOT FOUND")
	if(EXISTS "${GULPROOT}/Src/Linux_MPI")
		message(STATUS "GULPOBJ FOUND")
		if(EXISTS "${GULPROOT}/Utils/pGFNFF/Src")
			message(STATUS "GULPGFNFF FOUND")
		else()
			message(FATAL_ERROR "GULPOBJ pGFNFF DOES NOT EXISTS! ${GULPGFNFF}
			GULPKLMC-lib COMPILATION POSSIBLY FIX THIS ERROR,
			DID YOU INSTALL GULPKLMC-lib ${GULPLIB}?")
		endif()
	else()
		message(FATAL_ERROR "GULPOBJ DOES NOT EXISTS! ${GULPOBJ}
		GULPKLMC-lib COMPILATION POSSIBLY FIX THIS ERROR,
		DID YOU INSTALL GULPKLMC-lib ${GULPLIB}?")
	endif()
else()
	message(FATAL_ERROR "GULPROOT DOES NOT EXISTS! ${GULP_ROOT}
			RECHECK GULPROOT PATH SETTING")
endif()

#
# GULP LIB SETTING
#

# GULPKLMC LIB
find_library(GULPKLMC NAMES libgulpklmc.a HINTS ${GULPOBJ} REQUIRED)
message(STATUS "GULPKLMC LIB: ${GULPKLMC}")
if(NOT GULPKLMC)
    message(FATAL_ERROR "libgulpklmc.a GULPKLMC LIB DOES NOT EXISTS!
			COMPLETE ARCHIVING USING ${GULPROOT}/Src/_build_libgulp/*_compile.sh POSSIBLY FIX THIS ERROR")
endif()
# GULP pGFNFF LIB
find_library(GULPpGFNFF NAMES libpGFNFF.a HINTS ${GULPGFNFF} REQUIRED)
message(STATUS "GULPpGFNFF LIB: ${GULPpGFNFF}")
if(NOT GULPpGFNFF)
    message(FATAL_ERROR "libpGFNFF.a GULPpGFNFF LIB DOES NOT EXISTS!")
endif()


# --------------------
# ENV SETTING DONE
# --------------------

# 
# C SOURCE
#
set(SRC
	# INDENTATION SHOWS CALL STACK ORDER
    taskfarm.c
        taskfarm_def.c
            read_input.c
        master_worker_ready_input.c
        timer.c
        print_message.c
        error.c
        file.c
)
set(SRC_DEV 
	${CMAKE_SOURCE_DIR}/develop/unit_test.c
)

#
# FORTRAN SOURCE
#
set(SRC_SUBPROGRAM
    ${CMAKE_SOURCE_DIR}/subroutines/subprogram_pi.c
)
set(SRC_FORTRAN
    ${CMAKE_SOURCE_DIR}/subroutines/fsubroutine_pi.F90
    ${CMAKE_SOURCE_DIR}/subroutines/gulpklmc.F90
    ${CMAKE_SOURCE_DIR}/subroutines/gulpklmc_initmax.F90
    ${CMAKE_SOURCE_DIR}/subroutines/gulpklmc_deallocate_all.F90
)

#
# C HEADERS
#
include_directories(${CMAKE_SOURCE_DIR}/includes)
include_directories(${CMAKE_SOURCE_DIR}/develop/includes)
include_directories(${CMAKE_SOURCE_DIR}/subroutines/includes)

#
# FORTRAN *.mod *.o
#
include_directories(${GULPOBJ})

#
# SET EXECUTABLE NAME
#
set(EXE klmc3.062024.x)

# ------------------------------------------------------------------------

#
# IF USE PYTHON: ADD ADDITIONAL C SOURCE, C INCLUDES
#
if(ENABLE_PYTHON STREQUAL "TRUE")
	# PYTHON INTERFACE ADDITIONAL SOURCE
    list(APPEND SRC
		master_worker_python.c
			${CMAKE_SOURCE_DIR}/python_interface/call_python_serial.c
			${CMAKE_SOURCE_DIR}/python_interface/python_serial_initfinal.c
    )
	# PYTHON INCLUDES
	include_directories(${CMAKE_SOURCE_DIR}/python_interface/includes)
	include_directories(${PYTHONINCLUDE})

	message(STATUS ${SRC})
endif()

#
# BUILD SETTING
#
add_executable(${EXE} ${SRC} ${SRC_DEV} ${SRC_SUBPROGRAM} ${SRC_FORTRAN})

# ------------------------------------------------------------------------

# CRAY
if(COMPILER STREQUAL "CRAY")
	if(ENABLE_PYTHON STREQUAL "TRUE")
		target_link_libraries(${EXE} ${MPI_LIBRARIES} MPI::MPI_Fortran ${GULPKLMC} ${GULPpGFNFF} ${PYTHONLIB})
	else()
		target_link_libraries(${EXE} ${MPI_LIBRARIES} MPI::MPI_Fortran ${GULPKLMC} ${GULPpGFNFF})
	endif()
# INTEL
elseif(COMPILER STREQUAL "INTEL")
	if(ENABLE_PYTHON STREQUAL "TRUE")
		target_link_libraries(${EXE} ${MPI_LIBRARIES} MPI::MPI_Fortran ${GULPKLMC} ${GULPpGFNFF} ${LINK_MKL} ${PYTHONLIB})
	else()
		target_link_libraries(${EXE} ${MPI_LIBRARIES} MPI::MPI_Fortran ${GULPKLMC} ${GULPpGFNFF} ${LINK_MKL})
	endif()
endif()

#
# FINALISE
#
set_target_properties(${EXE} PROPERTIES LINKER_LANGUAGE C)
