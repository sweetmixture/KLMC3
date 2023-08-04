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

#include "timer.h"

// develop check only
#include "unit_test.h"
//#include "subroutines.h"

#define NUM_TASKS 34

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

	int ierr;

	char currentTime[64]; 
	double start_t, end_t, total_elapsed_t;

	// call read basic farm configuration
	TaskFarmConfiguration tfc;
	// tfc.num_tasks
	// tfc.cpus_per_workgroup

	ierr = tf_get_taskfarm_configuration( &tfc );
	if( ierr == TF_TRUE ){
		if( brank == 0 ){
			fprintf(stdout,"MASTER - TaskFarmMain> cpus_per_workgroup : %d\n", tfc.cpus_per_workgroup);
			fprintf(stdout,"MASTER - TaskFarmMain> requested ntasks   : %d\n", tfc.num_tasks);
			fprintf(stdout,"MASTER - TaskFarmMain> task id start      : %d\n", tfc.task_start);
			fprintf(stdout,"MASTER - TaskFarmMain> task id end        : %d\n", tfc.task_end);
			fflush(stdout);
		}
	}
	else{
		if( brank == 0 ){
			fprintf(stdout,"MASTER - TaskFarmMain> TaskFarmConfiguration Request Failed\n");
			fflush(stdout);
		}
		exit(1);
	}

	n_workgroup = tf_config_workgroup(&BaseComm, &WorkgroupComm, &workgroup_tag, &workgroup_size, &worker_rank, tfc.cpus_per_workgroup );
	mastergroup_brank  = bsize - 1;
	//unittest_tf_config_workgroup_array(&WorkgroupComm, n_workgroup, workgroup_tag);	// unittest
	//exit(1);
	if( brank == 0 ){
		fprintf(stdout,"MASTER - TaskFarmMain> MPI_Comm Split Done\n");
	}

	// workgroup config saver
	WorkgroupConfig workgroup_config[n_workgroup];
	tf_get_workgroup_config(&BaseComm,&WorkgroupComm,&workgroup_config[0],n_workgroup,workgroup_tag);	// make sure this is done before calling master()/workgroup()
	//unittest_tf_get_workgroup_config(&BaseComm,&WorkgroupComm,&workgroup_config[0],n_workgroup,workgroup_tag);
	//exit(1);
	if( brank == mastergroup_brank ){
		start_t = get_time();
		getCurrentDateTime(currentTime);
		fprintf(stdout,"MASTER - TaskFarmMain> Workgroup Configuration Done\n");
		fprintf(stdout,"MASTER - TaskFarmMain> TaskFarm starts at: %s\n",currentTime);
		fflush(stdout);
	}


	/* taskfarm main */
	if( brank == mastergroup_brank ){
		//master_worker_task_call_master( &BaseComm, &workgroup_config[0], n_workgroup, tfc.num_tasks );
		master_worker_task_call_master( &BaseComm, &workgroup_config[0], n_workgroup, tfc.task_start, tfc.task_end );
		fprintf(stdout,"MASTER - TaskFarmMain> Finalising MASTER > \n");
	}
	else{
		master_worker_task_call_workgroup( &BaseComm, &WorkgroupComm, n_workgroup, workgroup_tag );
		fprintf(stdout,"WORKER - TaskFarmMain> Finalising Workgroups: base_rank : %d - workgroup_tag %d -  wr / ws: %d / %d \n",brank,workgroup_tag,worker_rank,workgroup_size);
	}

	// wait all cpus
	MPI_Barrier(BaseComm);

	if( brank == mastergroup_brank ){
		end_t = get_time();
		getCurrentDateTime(currentTime);
		fprintf(stdout,"MASTER - TaskFarmMain> All Task Done at: %s / total_elapsed_t: %24.12lf\n",currentTime,end_t - start_t);
	}

	MPI_Comm_free(&WorkgroupComm);
	MPI_Finalize();

	return 0;
}
