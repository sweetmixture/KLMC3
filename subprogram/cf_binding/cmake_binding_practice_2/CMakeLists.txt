cmake_minimum_required(VERSION 3.11)

project(
	CrossCompileCFortran
	VERSION 0.1
	LANGUAGES C Fortran
)

find_package(MPI REQUIRED)

#enable_language(C)		# already set in 'project'
#enable_language(Fortran)

set(CMAKE_C_COMPILIER icc)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c11 -O3")

set(CMAKE_Fortran_COMPILER ifort)

set(SRC call_fortran_main.c)
set(SRF fsubroutine_test.f90 fsubroutine_pi.f90)
set(EXE cf.x)


add_executable(${EXE} ${SRC} ${SRF})
target_link_libraries(${EXE} ${MPI_LIBRARIES} MPI::MPI_Fortran)
#target_link_libraries(${EXE} -lifcore)	# required if directly compiled or using 'makefile'

# Disable the generation of the Fortran main function
set_target_properties(${EXE} PROPERTIES LINKER_LANGUAGE C)
