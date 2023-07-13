/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/


#ifndef __DEVELOP
#define __DEVELOP

#include <mpi.h>

void unittest_tf_config_workgroup_array( MPI_Comm* comm, const int n_comm, const int comm_tag );

void unittest_tf_get_workgroup_config( const MPI_Comm* base_comm, const MPI_Comm* workgroup_comm, const WorkgroupConfig* wc, const int n_workgroup, const int workgroup_tag );

#endif
