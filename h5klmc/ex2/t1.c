#include "hdf5.h"

#define FILENAME "example.h5"

// Function to create and write a dataset
void create_and_write_dataset(hid_t group_id, const char *dataset_name, hsize_t dims[2], int data[4][6]) {
    hid_t dataspace_id, dataset_id;
    herr_t status;

    // Create the data space for the dataset
    dataspace_id = H5Screate_simple(2, dims, NULL);

    // Create the dataset
    dataset_id = H5Dcreate(group_id, dataset_name, H5T_NATIVE_INT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    // Write the data to the dataset
    status = H5Dwrite(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);

    // Close the dataset and dataspace
    status = H5Dclose(dataset_id);
    status = H5Sclose(dataspace_id);
}

// Function to create and write an attribute
void create_and_write_attribute(hid_t group_id, const char *attr_name, const char *attr_data) {
    hid_t attr_id, dataspace_id;
    herr_t status;
    hsize_t dims = 1;

    // Create the data space for the attribute
    dataspace_id = H5Screate(H5S_SCALAR);

    // Create the attribute
    attr_id = H5Acreate(group_id, attr_name, H5T_NATIVE_CHAR, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);

    // Write the attribute data
    status = H5Awrite(attr_id, H5T_NATIVE_CHAR, attr_data);

    // Close the attribute and dataspace
    status = H5Aclose(attr_id);
    status = H5Sclose(dataspace_id);
}

int main() {
    hid_t file_id, group1_id, group2_id, subgroup_id;
    herr_t status;
    hsize_t dims[2] = {4, 6};
    int data[4][6] = {
        {1, 2, 3, 4, 5, 6},
        {7, 8, 9, 10, 11, 12},
        {13, 14, 15, 16, 17, 18},
        {19, 20, 21, 22, 23, 24}
    };

    // Create a new file using the default properties
    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    // Create the first group
    group1_id = H5Gcreate(file_id, "/Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    create_and_write_attribute(group1_id, "Attribute1", "This is Group1");
    create_and_write_dataset(group1_id, "Dataset1", dims, data);

    // Create the second group
    group2_id = H5Gcreate(file_id, "/Group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    create_and_write_attribute(group2_id, "Attribute2", "This is Group2");
    create_and_write_dataset(group2_id, "Dataset2", dims, data);

    // Create a subgroup under the first group
    subgroup_id = H5Gcreate(group1_id, "SubGroup", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    create_and_write_attribute(subgroup_id, "SubAttribute", "This is SubGroup");
    create_and_write_dataset(subgroup_id, "SubDataset", dims, data);

    // Close the groups
    status = H5Gclose(subgroup_id);
    status = H5Gclose(group2_id);
    status = H5Gclose(group1_id);

    // Close the file
    status = H5Fclose(file_id);

    return 0;
}

