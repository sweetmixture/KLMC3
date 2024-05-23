#include "hdf5.h"

int main() {
	// File and dataset identifiers
	// hid_t file_id, dataset_id, dataspace_id;
	// herr_t status;
	
	hid_t file_id;
	hid_t dataset_id;
	hid_t dataspace_id;

	herr_t status;

	// Dimensions of the dataset
	// * DATASPACE
	hsize_t dims[2];
	dims[0] = 4;
	dims[1] = 6;
	
	// Data to write
	int data[4][6] = {
		{1, 2, 3, 4, 5, 6},
		{7, 8, 9, 10, 11, 12},
		{13, 14, 15, 16, 17, 18},
		{19, 20, 21, 22, 23, 24}
	};
	
	// Create a new file using the default properties
/*
hid_t H5Fopen(const char* filename, unsigned flags, hid_t fapl_id)	

hid_t H5Fcreate(const char* filename, unsigned flags, hid_t fcpl_id, hid_t fapl_id)	

	flags: H5F_ACC_TRUNC: Truncate file, if it already exists, erasing all data previously stored in the file
	       H5F_ACC_EXCL: Fail if file already exists

*/
	// using 'file_id' to do controls
	file_id = H5Fcreate("example.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	
	// Create the data space for the dataset
/*
hid_t H5Screate_simple(int rank, const hsize_t dims[], const hsize_t maxdims[])	
	[in] rank: Number of dimensions of dataspace
	[in] dims: Array specifying the size of each dimension
	[in] maxdims: Array specifying the maximum size of each dimension
*/
	dataspace_id = H5Screate_simple(2, dims, NULL); // goto 'H5Dcreate()'
	
	// Create the dataset with default properties
/* H5Dcreate could be 'H5Dcreate1()' or 'H5Dcreate2()' by overriding done (by macro) - superseded by H5Dcreate2()
   i.e., H5Dcreate1() - about to be deprecated ???

hid_t H5Dcreate2(
hid_t loc_id,		* [in] loc_id		Location identifier. The identifier may be that of a file, group, dataset, named datatype, or attribute.
const char* name,	* [in] name		Name of the dataset to create
hid_t type_id,		* [in] type_id	Datatype identifier
hid_t space_id,		* [in] space_id	Dataspace identifier * using 'H5T_NATIVE_XXX' recommended
hid_t lcpl_id,		  [in] lcpl_id	Link creation property list identifier
hid_t dcpl_id,		  [in] dcpl_id	Dataset creation property list identifier
hid_t dapl_id 		  [in] dapl_id	Dataset access property list identifier
)	
*/
	dataset_id = H5Dcreate(file_id, "/dset", H5T_NATIVE_INT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT); // goto 'H5Dwrite()'
	//                     ^^^^^^^ write this under the 'file_id': root group

	// Write the dataset to the file
/*
writing should be followed after creating H5Dcreate() call

herr_t H5Dwrite	(
hid_t dset_id,      * [in]	dset_id	Identifier of the dataset to read from
hid_t mem_type_id,  * [in]	mem_type_id	Identifier of the memory datatype
hid_t mem_space_id,   [in]	mem_space_id	Identifier of the memory dataspace
hid_t file_space_id,* [in]	file_space_id	Identifier of the dataset's dataspace in the file
hid_t dxpl_id,        [in]	dxpl_id	Dataset transfer property list identifier
const void*	buf     * [out]	buf	Buffer with data to be written to the file
)	
*/
	status = H5Dwrite(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
	//                ^^^^^^^^^^ write this under the created dset_id: 'H5Dcreate()'


// * Closing Required ... (probaby for the memory management reason?)

	// Close the dataset
	status = H5Dclose(dataset_id);
	
	// Close the dataspace
	status = H5Sclose(dataspace_id);
	
	// Close the file
	status = H5Fclose(file_id);
	
	return 0;
}

