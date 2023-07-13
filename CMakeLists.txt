#
cmake_minimum_required(VERSION 3.11)

# Project Info - very sensitive to Syntax!!
project(
	BaseTaskFarm
	VERSION 1.0.0
	LANGUAGES C Fortran)

# set( [ARG] [VAR1] [VAR2] ... )

# MPI_LIBRARIES # MPI_INCLUDE_PATH -> these variables will be set
find_package(MPI REQUIRED)
include_directories(${MPI_INCLUDE_PATH})

# cray (C) - cc, (Fortran) - ftn
set(CMAKE_C_COMPILER cc)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c11 -O3 -h pic")
# solves issue :lurmstepd: error: Detected 1 oom-kill event(s) in StepId=3904445.0. Some of your processes may have been killed by the cgroup out-of-memory handler.
# when more than 1 node in use
#
set(CMAKE_Fortran_COMPILER ftn)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3")

# Get C compiler version - nothing to do with actual compilation ------------------------
execute_process(COMMAND ${CMAKE_C_COMPILER} --version OUTPUT_VARIABLE C_COMPILER_VERSION)
string(REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+" C_COMPILER_VERSION "${C_COMPILER_VERSION}")
message(STATUS "C Compiler Version: ${C_COMPILER_VERSION}")
# ---------------------------------------------------------------------------------------




# EDIT BLOCK --------------------------------------------------
set(SRC main.c taskfarm_def.c master_worker_task.c)
set(SRC_DEV ${CMAKE_SOURCE_DIR}/develop/unit_test.c)
set(SRC_SUBPROGRAM ${CMAKE_SOURCE_DIR}/subprogram/subprogram_pi.c)
set(SRC_FORTRAN ${CMAKE_SOURCE_DIR}/subprogram/fsubroutine_pi.F90 ${CMAKE_SOURCE_DIR}/subprogram/gulpklmc.F90)

set(EXE tf.x)

# header file path: include_directories( [EXECUTABLE] PUBLIC [PATH1] [PATH2] ... ) 
#include_directories(${EXE} PUBLIC ${CMAKE_SOURCE_DIR}/includes ${CMAKE_SOURCE_DIR}/develop/includes ${CMAKE_SOURCE_DIR}/subprogram/includes)

include_directories(${EXE} PUBLIC ${CMAKE_SOURCE_DIR}/includes)
include_directories(${EXE} PUBLIC ${CMAKE_SOURCE_DIR}/develop/includes)
include_directories(${EXE} PUBLIC ${CMAKE_SOURCE_DIR}/subprogram/includes)

# Modification required: user environment dependent 
include_directories(${EXE} PUBLIC /work/e05/e05/wkjee/Software/gulp-6.1.2/Src/Linux_MPI)	# hard link to the gulp *.mod files
# End modification

# Find libgulpklmc.a libpGFNFF.a
find_library(
	GULPKLMC
	NAMES libgulpklmc.a
	HINTS /work/e05/e05/wkjee/Software/gulp-6.1.2/Src/Linux_MPI
	REQUIRED)
find_library(
	GULPpGFNFF
	NAMES libpGFNFF.a
	HINTS /work/e05/e05/wkjee/Software/gulp-6.1.2/Utils/pGFNFF/Src
	REQUIRED)

add_executable(${EXE} ${SRC} ${SRC_DEV} ${SRC_SUBPROGRAM} ${SRC_FORTRAN})
#add_executable(${EXE} ${SRC} ${SRC_DEV} ${SRC_FORTRAN})
target_link_libraries(${EXE} ${MPI_LIBRARIES} MPI::MPI_Fortran ${GULPKLMC} ${GULPpGFNFF})




set_target_properties(${EXE} PROPERTIES LINKER_LANGUAGE C)
# -------------------------------------------------------------

# Print MPI library
# find_package(MPI REQUIRED) # find twice causing error - July 7 2023 wkjee
#message(STATUS "MPI library: ${MPI_LIBRARIES}")
#message(STATUS "MPI include: ${MPI_INCLUDE_PATH}")
# Print C compiler
#message(STATUS "C Compiler: ${CMAKE_C_COMPILER}")
