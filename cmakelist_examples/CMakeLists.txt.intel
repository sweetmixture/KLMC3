#
cmake_minimum_required(VERSION 3.11)
project(
	BaseTaskFarm
	VERSION 1.0.0
	LANGUAGES C Fortran)

find_package(MPI REQUIRED)
include_directories(${MPI_INCLUDE_PATH})

# ---------------------------------------------------------------------------------------
# USER_MODIFICATION: Modification required: user environment dependent - gulp *.o, *.mod, *.a
set(GULPROOT /work/e05/e05/wkjee/Software/gulpklmc/temporal/KLMC3-libgulp-6.1.2)
set(GULPOBJ ${GULPROOT}/Src/Linux_MPI)
set(GULPGFNFF ${GULPROOT}/Utils/pGFNFF/Src)

# CRAY SYSTEM: ARCHER2
# cray (C) - cc, (Fortran) - ftn
#set(CMAKE_C_COMPILER cc)
#set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c11 -O3")
#set(CMAKE_Fortran_COMPILER ftn)
#set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3")

# INTEL SYSTEM
# intel (C) - mpicc, (Fortran) - mpif90
set(CMAKE_C_COMPILER mpicc)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c11 -O3 -mcmodel=large")
set(CMAKE_Fortran_COMPILER mpif90)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3")
set(LINK_MKL "-lmkl_scalapack_lp64 -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lmkl_blacs_intelmpi_lp64 -lpthread -lm -ldl")
set(LINK_CORE "-lifcore -lifport")
# USER MODIFICATION END

# ---------------------------------------------------------------------------------------
# Get C compiler version - nothing to do with actual compilation ------------------------
execute_process(COMMAND ${CMAKE_C_COMPILER} --version OUTPUT_VARIABLE C_COMPILER_VERSION)
string(REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+" C_COMPILER_VERSION "${C_COMPILER_VERSION}")
message(STATUS "C Compiler Version: ${C_COMPILER_VERSION}")
message(STATUS "GULPROOT :" ${GULPROOT})
message(STATUS "GULPOBJ  :" ${GULPOBJ})
message(STATUS "GULPFGNFF:" ${GULPGFNFF})
# ---------------------------------------------------------------------------------------

# TASKFARM SOURCE
set(SRC 
    taskfarm.c
        taskfarm_def.c
            read_input.c
		master_worker_ready_input.c
        timer.c
        print_message.c
		error.c
)
set(SRC_DEV ${CMAKE_SOURCE_DIR}/develop/unit_test.c)

# APP SOURCE
set(SRC_SUBPROGRAM
	${CMAKE_SOURCE_DIR}/subroutines/subprogram_pi.c)
set(SRC_FORTRAN
	${CMAKE_SOURCE_DIR}/subroutines/fsubroutine_pi.F90
	${CMAKE_SOURCE_DIR}/subroutines/gulpklmc.F90
	${CMAKE_SOURCE_DIR}/subroutines/gulpklmc_initmax.F90
)

set(EXE tf.x)

include_directories(${EXE} PUBLIC ${CMAKE_SOURCE_DIR}/includes)
include_directories(${EXE} PUBLIC ${CMAKE_SOURCE_DIR}/develop/includes)
include_directories(${EXE} PUBLIC ${CMAKE_SOURCE_DIR}/subroutines/includes)
include_directories(${EXE} PUBLIC ${GULPOBJ})

# Find libgulpklmc.a libpGFNFF.a
find_library(
	GULPKLMC
	NAMES libgulpklmc.a
	HINTS ${GULPOBJ}
	REQUIRED)
find_library(
	GULPpGFNFF
	NAMES libpGFNFF.a
	HINTS ${GULPGFNFF}
	REQUIRED)

add_executable(${EXE} ${SRC} ${SRC_DEV} ${SRC_SUBPROGRAM} ${SRC_FORTRAN})

# USER_MODIFICATION
# CRAY STSTEM
#target_link_libraries(${EXE} ${MPI_LIBRARIES} MPI::MPI_Fortran ${GULPKLMC} ${GULPpGFNFF})
# INTEL SYSTEM
target_link_libraries(${EXE} ${MPI_LIBRARIES} MPI::MPI_Fortran ${GULPKLMC} ${GULPpGFNFF} ${LINK_MKL})
# USER_MODIFICATION END

# finalise 
set_target_properties(${EXE} PROPERTIES LINKER_LANGUAGE C)

# -------------------------------------------------------------
# Print MPI library
# find_package(MPI REQUIRED) # find twice causing error - July 7 2023 wkjee
# message(STATUS "MPI library: ${MPI_LIBRARIES}")
# message(STATUS "MPI include: ${MPI_INCLUDE_PATH}")
# Print C compiler
# message(STATUS "C Compiler: ${CMAKE_C_COMPILER}")
