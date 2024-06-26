find_library(
    GULPKLMC
    NAMES libgulpklmc.a
    HINTS ${GULPOBJ}
    REQUIRED
)

find_library(
    GULPpGFNFF
    NAMES libpGFNFF.a
    HINTS ${GULPGFNFF}
    REQUIRED
)

if(NOT GULPKLMC)
    message(FATAL_ERROR "Could not find libgulpklmc.a library!")
endif()

if(NOT GULPpGFNFF)
    message(FATAL_ERROR "Could not find libpGFNFF.a library!")
endif()

message(STATUS "Found GULPKLMC: ${GULPKLMC}")
message(STATUS "Found GULPpGFNFF: ${GULPpGFNFF}")
                                                                                  
