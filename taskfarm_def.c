/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/

#include <stdio.h>
#include <mpi.h>
#include "taskfarm_def.h"

// Comm Spliter
int tf_config_workgroup( 
	MPI_Comm* base_comm,
	MPI_Comm* workgroup_comm,
	int* workgroup_tag,
	int* workgroup_size,
	int* worker_rank,
	const int n_workers_per_workgroup )
{       
        int brank,bsize; 
        int n_workgroup; // return
        
        MPI_Comm_size(*base_comm,&bsize);
        MPI_Comm_rank(*base_comm,&brank);
        
        *workgroup_tag =  brank / n_workers_per_workgroup;                      // give 'workgroup tag' for each cpu
        
        // set number of workgroups ( for a given number of cpus )              
        if( bsize % n_workers_per_workgroup == 0 ){
                n_workgroup = bsize / n_workers_per_workgroup;
                n_workgroup++;
                /* case - 1: no residual cpus to be the 'master'
                        last cpu in last workgroup -> 'master'          */
        }
        else if( bsize % n_workers_per_workgroup == 1 ){                        
                n_workgroup = bsize / n_workers_per_workgroup;
                n_workgroup++;
                /* case - 2: 1 residual cpu in the last workgroup
                        last workgroup -> 'master'                      */
        }
        else{                                                                   
                n_workgroup = bsize / n_workers_per_workgroup + 1;
                n_workgroup++;
                /* case - 3: >1 residual cpus in the last workgroup
                        split last workgroup (workgroup_size-1 / 1)
                        and the last workgroup -> 'master'              */
        }

        if( ((brank + 1) == bsize) && ((bsize % n_workers_per_workgroup) != 1) ){
                *workgroup_tag = *workgroup_tag + 1;
        }

	// Comm Spliting
	MPI_Comm_split(*base_comm, *workgroup_tag, brank, workgroup_comm);
	MPI_Comm_size(*workgroup_comm,workgroup_size);
	MPI_Comm_rank(*workgroup_comm,worker_rank);

	MPI_Barrier(*base_comm);

        return n_workgroup;
}

void tf_get_workgroup_config(
	const MPI_Comm* base_comm,		// IN
	const MPI_Comm* workgroup_comm,		// IN
	WorkgroupConfig* workgroup_config,	// IN-OUT
	const int n_workgroup,			// IN
	const int workgroup_tag			// IN
	)
{
/*
	if( workgroup_tag == i && worker_rank == 0 ) {
	... WorkgroupConfig workgroup_config[i].base_rank;
*/
	int base_root = 0;

	int brank,bsize;
	int workgroup_size, worker_rank;

	MPI_Comm_rank(*base_comm,&brank);
	MPI_Comm_size(*base_comm,&bsize);
	
	MPI_Comm_size(*workgroup_comm,&workgroup_size);
	MPI_Comm_rank(*workgroup_comm,&worker_rank);

	WorkgroupConfig wc;

	wc.base_size = bsize;
	wc.base_rank = brank;
	wc.workgroup_tag = workgroup_tag;
	wc.workgroup_size = workgroup_size;
	wc.worker_rank = worker_rank;

	MPI_Request request;
	MPI_Status status;

	/*
		For each workgroup call 'MPI_Send' by it's head rank, send 'WorkgroupConfig' to base_root
	*/
	for(int i=1;i<n_workgroup;i++){
		if( workgroup_tag == i && worker_rank == 0 ){
			MPI_Send(&wc,sizeof(WorkgroupConfig),MPI_CHAR,base_root,i,*base_comm);
			//printf("SEND> workgroup_tag: %d worker_rank: %d / %d (worgroup_size) base_rank: %d\n",workgroup_tag,worker_rank,workgroup_size,brank);
		}
	}

	/*
 		Call 'MPI_Recv' by base_root 'n_workgroup' times to get 'WorkgroupConfig' except the base_root
	*/
	for(int i=1;i<n_workgroup;i++){
		if( brank == base_root ){
				MPI_Recv(&workgroup_config[i],sizeof(WorkgroupConfig),MPI_CHAR,MPI_ANY_SOURCE,i,*base_comm,&status);
				//printf("workgroup_tag: %d worker_rank: %d base_rank: %d\n",workgroup_config[i].workgroup_tag,workgroup_config[i].worker_rank,workgroup_config[i].base_rank);
		}
	}

	/*
 		in-place copying 'WorkgroupConfig(wc)'
	*/
	if ( workgroup_tag == 0 && worker_rank == 0 ){
		workgroup_config[0] = wc;
		//MPI_ISend(&wc,sizeof(WorkgroupConfig),MPI_CHAR,base_root,workgroup_tag,*base_comm);
	}
	
	MPI_Bcast(&workgroup_config[0],sizeof(WorkgroupConfig)*n_workgroup,MPI_CHAR,base_root,*base_comm);
/*
	for(int i=0;i<n_workgroup;i++){
		if( workgroup_tag == i && worker_rank == 0 ){
			printf("i: %d workgroup_tag: %d worker_rank: %d / %d (workgroup_size) base_rank: %d \n",i, workgroup_config[i].workgroup_tag, 
													workgroup_config[i].worker_rank,
													workgroup_config[i].workgroup_size,
													workgroup_config[i].base_rank);
		}
	}
*/
	return;
}


