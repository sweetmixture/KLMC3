#include "hdf5.h"

int main(int argc, char* argv[]){


	hid_t file_id;
	hid_t dataset_id;
	hid_t dataspace_id;

	herr_t status;

	// get file id
	file_id = H5Fcreate("t2.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

	hid_t g1_id;
	hid_t g2_id;

	// create g1, g2 under 'root (file_id)'
/*
hid_t H5Gcreate2(
hid_t loc_id,     * [in] loc_id  Location identifier. The identifier may be that of a file, group, dataset, named datatype, or attribute.
const char* name, * [in] name    Name of the group to create
hid_t lcpl_id,      [in] lcpl_id Link creation property list identifier
hid_t gcpl_id,      [in] gcpl_id Group creation property list identifier
hid_t gapl_id       [in] gapl_id Group access property list identifier
)	
Returns a group identifier if successful; otherwise returns H5I_INVALID_HID.
*/

// CREATE GROUPS

	g1_id = H5Gcreate(file_id, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	g2_id = H5Gcreate(file_id, "Group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

	hid_t sg1_id;
	hid_t sg2_id;

// CREATE SUBGROUPS

	// create sg1, sg2 under g1, g2
	sg1_id = H5Gcreate(g1_id, "SubGroup1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	sg2_id = H5Gcreate(g2_id, "SubGroup2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	
	hid_t a1_id, a1_space;
	hid_t a2_id, a2_space;
	// create attribute to g1, g2
/*
hid_t H5Acreate2(
hid_t loc_id,           [in] loc_id     Location identifier. The identifier may be that of a file, group, dataset, or named datatype.
const char* attr_name,  [in] attr_name	Name of attribute
hid_t type_id,          [in] type_id    Attribute datatype identifier
hid_t space_id,         [in] space_id   Dataspace identifier
hid_t acpl_id,          [in] acpl_id    Attribute creation property list identifier
hid_t aapl_id           [in] aapl_id    Attribute access property list identifiera
)
*/

// ADD ATTRIBUTES

	// define space: SCALAR (single), H5Screate(arg) possible arg: H5S_SCALAR, H5S_SIMPLE, and H5S_NULL.
	a1_space = H5Screate(H5S_SCALAR);
	a2_space = H5Screate(H5S_SCALAR);
	// create attr under g1, g2
	a1_id =  H5Acreate(g1_id, "klmc", H5T_NATIVE_CHAR, a1_space, H5P_DEFAULT, H5P_DEFAULT);
	a2_id =  H5Acreate(g2_id, "klmc", H5T_NATIVE_CHAR, a1_space, H5P_DEFAULT, H5P_DEFAULT);
	// write the attr
/*
herr_t H5Awrite(
hid_t attr_id,    [in]	attr_id Attribute identifier
hid_t type_id,    [in]  type_id Datatype (in-memory) identifier
const void* buf   [out] buf     Data to be written
)	
*/
	status = H5Awrite(a1_id, H5T_NATIVE_CHAR, "attr");	// this will be recorded as an integer that, since the datatype 'SCALAR' is not the correct one
	status = H5Awrite(a2_id, H5T_NATIVE_CHAR, "attr");	// this will be recorded as an integer that, since the datatype 'SCALAR' is not the correct one

	H5Sclose(a1_space);
	H5Sclose(a2_space);
	H5Aclose(a1_id);
	H5Aclose(a2_id);

	// practice write attribute in subgroups, sg1_id, sg2_id
	a1_space = H5Screate(H5S_SCALAR);
	a2_space = H5Screate(H5S_SCALAR);
	a1_id = H5Acreate(sg1_id, "task_id", H5T_NATIVE_CHAR, a1_space, H5P_DEFAULT, H5P_DEFAULT);
	a2_id = H5Acreate(sg2_id, "task_id", H5T_NATIVE_CHAR, a2_space, H5P_DEFAULT, H5P_DEFAULT);

	status = H5Awrite(a1_id,H5T_NATIVE_CHAR,"GULP");
	status = H5Awrite(a2_id,H5T_NATIVE_CHAR,"GULP");
	
	H5Sclose(a1_space);
	H5Sclose(a2_space);
	H5Aclose(a1_id);
	H5Aclose(a2_id);

// ADDING DATASETS

	double lvectors[3][3] = {
		{12.12,0,0.123},
		{-0.34,15.12,0.31},
		{0.11,-0.52,15.1251}
	};
	//hsize_t dims[2] = {3, 3};
	hsize_t dims[4] = {3, 3, 5, 1};
	hid_t dset_id1, dspace_id1;
	hid_t dset_id2, dspace_id2;

	// create 2(rank), dims(arr), maxdims(NULL) data space
	dspace_id1 = H5Screate_simple(2,dims,NULL);
	dspace_id2 = H5Screate_simple(2,dims,NULL);

	// create H5D
	dset_id1 = H5Dcreate(sg1_id, "lvectors", H5T_NATIVE_DOUBLE, dspace_id1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	dset_id2 = H5Dcreate(sg2_id, "lvectors", H5T_NATIVE_DOUBLE, dspace_id1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

	// write H5D
	status = H5Dwrite(dset_id1, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, lvectors);
	status = H5Dwrite(dset_id2, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, lvectors);

	H5Dclose(dset_id1);
	H5Dclose(dset_id2);

	H5Sclose(dspace_id1);
	H5Sclose(dspace_id2);

	// * Finalising --------------------------------------------------------------

	// close sg1, sg2
	status = H5Gclose(sg1_id);
	status = H5Gclose(sg2_id);

	// close g1, g2
	status = H5Gclose(g1_id);
	status = H5Gclose(g2_id);

	// close root
	status = H5Fclose(file_id);

	return 0;
}
