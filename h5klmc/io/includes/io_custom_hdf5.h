#ifndef _KLMC_HDF5_
#define _KLMC_HDF5_

#include "hdf5.h"
#include <string.h>

/* * * * *
 using functions
 * * * * */
#define H5_KLMC_DTYPE_DOUBLE 153104
#define H5_KLMC_DTYPE_FLOAT  153106
#define H5_KLMC_DTYPE_INT    153111

herr_t h5klmc_create_write_dset_1d(
const hid_t loc,
const char* dset_name,
const hsize_t dims[1], // length of this vector, or arr
const int dtype,
const void* data
);

herr_t h5klmc_create_write_dset_2d(
const hid_t loc,
const char* dset_name,
const hsize_t dims[2],
const int dtype,
const void** data
);

herr_t h5klmc_create_write_dset_3d(
const hid_t loc,
const char* dset_name,
const hsize_t dims[3],
const int dtype,
const void*** data
);


// some hard-coded 'gulp' strings ...


#define GULP_3D_FORMULA "Formula ="
#define GULP_3D_INIT_LATTICE_V "Cartesian lattice vectors"
#define GULP_3D_INIT_LATTICE_P "Cell parameters"



#endif







