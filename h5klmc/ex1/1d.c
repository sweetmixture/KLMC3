#include "hdf5.h"

int main() {
    // Define file, dataset, and dataspace identifiers
    hid_t file_id, dataset_id, dataspace_id;
    hsize_t dims[1]; // One-dimensional array
    herr_t status;

    // Data to write
    int data[5] = {1, 2, 3, 4, 5};

    // Define the dimensions of the dataset
    dims[0] = 5;

    // Create a new file using the default properties
    file_id = H5Fcreate("1d.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    // Create the data space with the given dimensions
    dataspace_id = H5Screate_simple(1, dims, NULL);

    // Create the dataset with default properties
    dataset_id = H5Dcreate(file_id, "1D_Dataset", H5T_NATIVE_INT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    // Write the dataset to the file
    status = H5Dwrite(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);

    // Close the dataset and the dataspace
    status = H5Dclose(dataset_id);
    status = H5Sclose(dataspace_id);

    // Close the file
    status = H5Fclose(file_id);

    return 0;
}

