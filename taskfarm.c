/*
	Author:		Woongkyu Jee / woong.jee.16@ucl.ac.uk
	Affiliation:	University College London
	Date:		2023.05.25 - 

	Description:
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <mpi.h>

#include "taskfarm_def.h"
#include "master_worker_task.h"
#include "timer.h"

// develop check only
#include "unit_test.h"

int main(int argc, char* argv[])
{
	char currentTime[64]; 
	double start_t, end_t, total_elapsed_t;
	bool berr;

	int brank,bsize;

	MPI_Comm BaseComm;
	BaseComm = MPI_COMM_WORLD;

	MPI_Init(&argc,&argv);
	MPI_Comm_size(BaseComm,&bsize);
	MPI_Comm_rank(BaseComm,&brank);

	// ---------------------------- BaseComm configuration done

	MPI_Comm WorkgroupComm;
	TaskFarmConfiguration tfc;

	const int mrank = bsize - 1;	// last CPU is always dedicated to 'master' 

	/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

	// PREPARATION 
	tfc.bsize = bsize;
	tfc.brank = brank;
	tfc.mrank = mrank;

	// 1 STEP : READ INPUT
	berr = tf_get_taskfarm_configuration( &tfc );


// REF below 'if' 'else'
	if( berr ){
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

	// 2 STEP : MPI COMMUNICATOR SPLITTING
	berr = tf_config_workgroup(&BaseComm, &WorkgroupComm, &tfc );

// REF do something if berr success


	//unittest_tf_config_workgroup_array(&WorkgroupComm, tfc.n_workgroup, tfc.workgroup_tag);	// unittest
	//exit(1);
	if( brank == 0 ){
		fprintf(stdout,"MASTER - TaskFarmMain> MPI_Comm Split Done\n");
	}

	// workgroup config saver
	WorkgroupConfig wgc[tfc.n_workgroup];

	// 3 STEP : WORKGROUP CONFIGURATION SETTING
	berr = tf_get_workgroup_config(&BaseComm,&WorkgroupComm,&tfc,&wgc[0]);	// make sure this is done before calling master()/workgroup()

// 3 STEP DEPRECATES ...
	//tf_get_workgroup_config(&BaseComm,&WorkgroupComm,&wgc[0],tfc.n_workgroup,tfc.workgroup_tag);	// make sure this is done before calling master()/workgroup()
	//berr = tf_get_workgroup_config(&BaseComm,&WorkgroupComm,&wgc[0],&tfc);	// make sure this is done before calling master()/workgroup()

// REF do something if berr success

////	////	////	////	Refactoring 17.08.23

	//unittest_tf_get_workgroup_config(&BaseComm,&WorkgroupComm,&wgc[0],tfc.n_workgroup,tfc.workgroup_tag);
	//exit(1);
	if( brank == mrank ){
		start_t = get_time();
		getCurrentDateTime(currentTime);
		fprintf(stdout,"MASTER - TaskFarmMain> Workgroup Configuration Done\n");
		fprintf(stdout,"MASTER - TaskFarmMain> TaskFarm starts at: %s\n",currentTime);
		fflush(stdout);
	}

	//exit(0);

	/* taskfarm main */
	if( brank == mrank ){
		//master_worker_task_call_master( &BaseComm, &wgc[0], tfc.n_workgroup, tfc.num_tasks );
		master_worker_task_call_master( &BaseComm, &wgc[0], tfc.n_workgroup, tfc.task_start, tfc.task_end );
		fprintf(stdout,"MASTER - TaskFarmMain> Finalising MASTER > \n");
	}
	else{
		master_worker_task_call_workgroup( &BaseComm, &WorkgroupComm, tfc.n_workgroup, tfc.workgroup_tag );
		fprintf(stdout,"WORKER - TaskFarmMain> Finalising Workgroups: base_rank : %d - workgroup_tag %d -  wr / ws: %d / %d \n",brank,tfc.workgroup_tag,tfc.worker_rank,tfc.workgroup_size);
	}

	// wait all cpus
	MPI_Barrier(BaseComm);

	if( brank == mrank ){
		end_t = get_time();
		getCurrentDateTime(currentTime);
		fprintf(stdout,"MASTER - TaskFarmMain> All Task Done at: %s / total_elapsed_t: %24.12lf\n",currentTime,end_t - start_t);
	}

	MPI_Comm_free(&WorkgroupComm);
	MPI_Finalize();

	return 0;
}
