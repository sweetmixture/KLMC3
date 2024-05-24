#include "io_custom_hdf5.h"

herr_t h5klmc_create_write_dset_1d(
const hid_t loc,
const char* dset_name,
const hsize_t dims[1], // length of this vector, or arr
const int dtype,
const void* data
){
	hid_t sid, did;
	herr_t status;

	sid = H5Screate_simple( 1, dims, NULL );

	// data type-casting: is required?

	if( dtype == H5_KLMC_DTYPE_DOUBLE ){
		did = H5Dcreate( loc, dset_name, H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		status = H5Dwrite( did, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
	}
	else if ( dtype == H5_KLMC_DTYPE_FLOAT ){
		did = H5Dcreate( loc, dset_name, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		status = H5Dwrite( did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
	}
	else if ( dtype == H5_KLMC_DTYPE_INT ){
		did = H5Dcreate( loc, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		status = H5Dwrite( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
	}

	H5Sclose(sid);
	H5Dclose(did);

	return status;
}

herr_t h5klmc_create_write_dset_2d(
const hid_t loc,
const char* dset_name,
const hsize_t dims[2],
const int dtype,
const void** data
){
	hid_t sid, did;
	herr_t status;

	sid = H5Screate_simple( 2, dims, NULL );

	// data type-casting: is required?

	if( dtype == H5_KLMC_DTYPE_DOUBLE ){
		did = H5Dcreate( loc, dset_name, H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		//status = H5Dwrite( did, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, (const double**)data);
		status = H5Dwrite( did, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
	}
	else if ( dtype == H5_KLMC_DTYPE_FLOAT ){
		did = H5Dcreate( loc, dset_name, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		//status = H5Dwrite( did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, (const float**)data);
		status = H5Dwrite( did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
	}
	else if ( dtype == H5_KLMC_DTYPE_INT ){
		did = H5Dcreate( loc, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		//status = H5Dwrite( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, (const int**)data);
		status = H5Dwrite( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
	}

	H5Sclose(sid);
	H5Dclose(did);

	return status;
}

herr_t h5klmc_create_write_dset_3d(
const hid_t loc,
const char* dset_name,
const hsize_t dims[3],
const int dtype,
const void*** data
){
	hid_t sid, did;
	herr_t status;

	sid = H5Screate_simple( 3, dims, NULL );

	// data type-casting: is required?

	if( dtype == H5_KLMC_DTYPE_DOUBLE ){
		did = H5Dcreate( loc, dset_name, H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		status = H5Dwrite( did, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
	}
	else if ( dtype == H5_KLMC_DTYPE_FLOAT ){
		did = H5Dcreate( loc, dset_name, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		status = H5Dwrite( did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
	}
	else if ( dtype == H5_KLMC_DTYPE_INT ){
		did = H5Dcreate( loc, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		status = H5Dwrite( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
	}

	H5Sclose(sid);
	H5Dclose(did);

	return status;
}



/* * * * * *
 String
 * * * * * */

