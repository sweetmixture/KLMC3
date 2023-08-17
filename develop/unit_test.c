/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/


#include <stdio.h>
#include <mpi.h>

//#include "subprogram.h"

#include "taskfarm_def.h"

void unittest_tf_config_workgroup_array( MPI_Comm* comm, const int n_comm, const int comm_tag )
{
        int size,rank;
        MPI_Comm_size(*comm,&size);
        MPI_Comm_rank(*comm,&rank);

        for(int i=0;i<n_comm;i++){
                if( comm_tag == i ){
                        for(int j=0;j<size;j++){
                                if( rank == j ){
                                        printf("[ comm_tag, comm_size, cpu_rank ] : [ %d, %d, %d ]\n", \
                                                comm_tag, size ,rank );
                                }
                        }
                }
        }

        if( comm_tag == n_comm - 1 ){
                if( rank == 0 ){
                        printf(" total  comm count: %d\n",n_comm);
                        printf(" master comm tag  : %d\n",comm_tag);
                }
        }
        return;
}

// dependency: taskfarm_def.h - WorkgroupConfig
void unittest_tf_get_workgroup_config( 
	const MPI_Comm* base_comm, 
	const MPI_Comm* workgroup_comm,
	const WorkgroupConfig* wc,
	const int n_workgroup,
	const int workgroup_tag
)
{
	int bsize,brank;
	int workgroup_size,worker_rank;

	MPI_Comm_size(*base_comm,&bsize);
	MPI_Comm_rank(*base_comm,&brank);

	MPI_Comm_size(*workgroup_comm,&workgroup_size);
	MPI_Comm_rank(*workgroup_comm,&worker_rank);

	if( brank == 0 ){ printf("Unit Test: unittest_tf_get_workgroup_config\n"); }

	for(int i=0;i<n_workgroup;i++){
		if( workgroup_tag == i  && worker_rank == 0 ){
			//printf("[ i, wg_comm_tag, wg_comm_size, worker_rank, base_rank ] : [ %d, %d, %d, %d, %d ]\n", i, wc[i].workgroup_tag, wc[i].workgroup_size, wc[i].worker_rank, brank );
			printf("[ i, wg_comm_tag, wg_comm_size, base_rank ] : [ %d, %d, %d, %d ]\n", i, wc[i].workgroup_tag, wc[i].workgroup_size, brank );
		}
	}

	return;
}
