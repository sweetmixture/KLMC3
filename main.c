/*
	Author:		Woongkyu Jee / woong.jee.16@ucl.ac.uk
	Affiliation:	University College London
	Date:		2023.05.25 - 

	Description:
*/

#include <stdio.h>
#include <stdlib.h>

#include <mpi.h>

#include "taskfarm_def.h"
#include "master_worker_task.h"

#include "subprogram.h"

// develop check only
#include "unit_test.h"

#define NUM_TASKS 1000

int main(int argc, char* argv[])
{
	int brank,bsize;

	MPI_Comm BaseComm;
	BaseComm = MPI_COMM_WORLD;

	MPI_Init(&argc,&argv);
	MPI_Comm_size(BaseComm,&bsize);
	MPI_Comm_rank(BaseComm,&brank);

	// ---------------------------- BaseComm configuration done

	MPI_Comm WorkgroupComm;
	int n_workgroup;		// number of workgroups
	int workgroup_tag;		// workgroup tag
	int workgroup_size;		// workgroup size
	int worker_rank;		// internal worker(cpu) rank within a workgroup
	
	int mastergroup_brank;

	n_workgroup = tf_config_workgroup(&BaseComm, &WorkgroupComm, &workgroup_tag, &workgroup_size, &worker_rank, TF_CPUS_PER_WORKER);
	mastergroup_brank  = bsize - 1;
	//unittest_tf_config_workgroup_array(&WorkgroupComm, n_workgroup, workgroup_tag);	// unittest
	//exit(1);

	// workgroup config saver
	WorkgroupConfig workgroup_config[n_workgroup];
	tf_get_workgroup_config(&BaseComm,&WorkgroupComm,&workgroup_config[0],n_workgroup,workgroup_tag);	// make sure this is done
	//unittest_tf_get_workgroup_config(&BaseComm,&WorkgroupComm,&workgroup_config[0],n_workgroup,workgroup_tag);
	//exit(1);


	/* taskfarm main */
	if( brank == mastergroup_brank ){
		master_worker_task_call_master( &BaseComm, &workgroup_config[0], n_workgroup, NUM_TASKS );
		fprintf(stdout,"MASTER - TaskFarmMain> Finalising MASTER > \n");
	}
	else{
		master_worker_task_call_workgroup( &BaseComm, &WorkgroupComm, n_workgroup, workgroup_tag );
		fprintf(stdout,"WORKER - TaskFarmMain> Finalising Workgroups: base_rank : %d - workgroup_tag %d -  wr / ws: %d / %d \n",brank,workgroup_tag,worker_rank,workgroup_size);
	}

	//exit(1);

	MPI_Comm_free(&WorkgroupComm);
	MPI_Finalize();

	return 0;
}
