cmake_minimum_required(VERSION 3.11)
project(DEV_KLMC_HDF5IO VERSION 1.0 LANGUAGES C)


find_package(HDF5 COMPONENTS C REQUIRED)
#set(HDF5_ROOT XXX) 

if( NOT HDF5_FOUND )
	message(FATAL_ERROR "HDF5 NOT FOUND")
endif()

#set(DUMMY "ABC")
#if( HDF5_FOUND )
#	message(NOTICE "Found this ${HDF5_C_LIBRARIES} ${DUMMY}")
#endif()


# Source
set(SOURCES
	main.c
	io_custom_hdf5.c
	/work/e05/e05/wkjee/Software/gulpklmc/klmc3_tf_interface.update.12092023/KLMC3.052024/read_input.c
	/work/e05/e05/wkjee/Software/gulpklmc/klmc3_tf_interface.update.12092023/KLMC3.052024/file.c
)
# Include
include_directories(
	/work/e05/e05/wkjee/Software/gulpklmc/klmc3_tf_interface.update.12092023/KLMC3.052024/includes
	${CMAKE_SOURCE_DIR}/includes
)
add_executable(io_dev.x ${SOURCES})
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c11 -O3")

