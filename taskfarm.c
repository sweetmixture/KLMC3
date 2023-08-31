/*
	Author:		Woongkyu Jee / woong.jee.16@ucl.ac.uk
	Affiliation:	University College London
	Date:		2023.05.25 - 

	Description:
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <mpi.h>

#include "taskfarm_def.h"
#include "master_worker_task.h"
#include "print_message.h"
#include "timer.h"

// develop check only
#include "unit_test.h"

int main(int argc, char* argv[])
{
	char msg[512];
	char currentTime[64]; 
	double start_t, end_t, total_elapsed_t;
	bool berr;

	int brank,bsize;
//	int impi;

	MPI_Comm BaseComm;
	BaseComm = MPI_COMM_WORLD;

	MPI_Init(&argc,&argv);

// ERRORCHECK REQ: MPI_Initialized( &impi );

	MPI_Comm_size(BaseComm,&bsize);
	MPI_Comm_rank(BaseComm,&brank);

	// ---------------------------- BaseComm configuration done

	MPI_Comm WorkgroupComm;
	TaskFarmConfiguration tfc;

	//const int mrank = bsize - 1;	// last CPU is always dedicated to 'master'  - deprecated 29.08.23

	/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

	// PREPARATION  - deprecated 29.08.23 - moved into 'tf_get_taskfarm_configuration()'
	// direct setting tfc.brank bsize mrank

	/* * * * *
	 * 1 STEP : READ INPUT
	 * * * * */
	berr = tf_get_taskfarm_configuration( &BaseComm, &tfc );
	MPI_Barrier(BaseComm);

	if( !berr ){ // taskfarm configure failed
		exit(1);
	}
	else{
		// PRINT <stdout> : info from 'taskfarm.config'
		print_stdout(tfc.brank,"\n");
		print_stdout(tfc.brank," KLMC3 Parallel Extension Template 0.1\n");
		print_stdout(tfc.brank,"\n");
		print_stdout(tfc.brank," Woongkyu Jee, University College London / woong.jee.16@ucl.ac.uk\n");
		print_stdout(tfc.brank," Date: 2023.05 - \n");
		print_stdout(tfc.brank,"\n");
		layout_line_stdout(tfc.brank,'-',80);
		print_stdout(tfc.brank,"\n");
		print_stdout(tfc.brank," TaskFarm Configuration\n");
		print_stdout(tfc.brank,"\n");
		sprintf(msg," Application: %s ",tfc.application); print_stdout(tfc.brank,msg);
		if( strcmp(tfc.application,"gulp") == 0 ){
			print_stdout(tfc.brank,"6.1.2\n");
		}
		else if( strcmp(tfc.application,"fhiaims") == 0 ){
			print_stdout(tfc.brank,"11.23\n");
		}
		print_stdout(tfc.brank,"\n");
		sprintf(msg," Request number of tasks         :       %d\n",tfc.num_tasks); print_stdout(tfc.brank,msg);
		sprintf(msg," task start index                :       %d\n",tfc.task_start); print_stdout(tfc.brank,msg);
		sprintf(msg," task end   index                :       %d\n",tfc.task_end); print_stdout(tfc.brank,msg);
		sprintf(msg," Requested CPUs per workgroup    :       %d\n",tfc.cpus_per_workgroup); print_stdout(tfc.brank,msg);
		print_stdout(tfc.brank,"\n");
		sprintf(msg," Total number of CPUs (resource) :       %d\n",bsize); print_stdout(tfc.brank,msg);
		print_stdout(tfc.brank,"\n");
		layout_line_stdout(tfc.brank,'-',80);
		fflush_channel(NULL);
	}
	MPI_Barrier(BaseComm);

	/* * * * *  deprecated 29.08.23 ( set in 'tf_get_taskfarm_configuration()'
	 * Stop using 'brank' -- replaced --> tfc.brank
	 * Stop using 'bsize' -- replaced --> tfc.bsize
	 * Stop using 'mrank' -- replaced --> tfc.mrank
	 * * * * */

	/* * * * *
	 * 2 STEP : MPI COMMUNICATOR SPLITTING : note. processors with same 'tag' (workgroup_tag) carries same 'workgroup_comm'
	 * * * * */
	berr = tf_config_workgroup(&BaseComm, &WorkgroupComm, &tfc );
	MPI_Barrier(BaseComm);

	if( !berr ){ // taskfarm configure failed
		exit(1);
	}
	else{
		// PRINT <stdout> : taskfarm cpu configurations
		print_stdout(tfc.brank,"\n");
		sprintf(msg," CPU Configuration < MPI_Comm_split done >\n"); print_stdout(tfc.brank,msg);
		print_stdout(tfc.brank,"\n");
		sprintf(msg," number of workgroups : %d",tfc.n_workgroup); print_stdout(tfc.brank,msg);
		sprintf(msg," (last workgroup is always dedicated to 'master')\n"); print_stdout(tfc.brank,msg);
		print_stdout(tfc.brank,"\n");
		fflush_channel(NULL);
		MPI_Barrier(BaseComm);

		char all_msg[tfc.bsize][sizeof(msg)];

		if( tfc.brank != tfc.mrank ){
			sprintf(msg," node %12s | group %6d | procid %6d | group_procid %6d\n", tfc.proc_name,tfc.workgroup_tag,brank,tfc.worker_rank);
		}
		else{
			sprintf(msg," node %12s | group %6d | procid %6d | group_procid %6d > master\n", tfc.proc_name,tfc.workgroup_tag,brank,tfc.worker_rank);
		}
		MPI_Barrier(BaseComm);

		MPI_Allgather(msg,sizeof(msg),MPI_CHAR,all_msg,sizeof(msg),MPI_CHAR,BaseComm);	// collecting messages from each processors ( rank )

		for(int i=0;i<tfc.bsize;i++){
			print_stdout(tfc.brank,all_msg[i]);
			fflush_channel(NULL);
		}
		print_stdout(tfc.brank,"\n");
		layout_line_stdout(tfc.brank,'-',80);
		fflush_channel(NULL);
	}
	MPI_Barrier(BaseComm);


	/* * *
	 New parameter setting: workgroup config saver	-> 08.23 - only used by 'master': send messages to 'head' (subrank = 0) of each workgroup - 'base_rank' 
	 * * */
	WorkgroupConfig wgc_global[tfc.n_workgroup];

	/* * * * *
	 * 3 STEP : WORKGROUP CONFIGURATION SETTING
	 * * * * */
	berr = tf_get_workgroup_config(&BaseComm,&WorkgroupComm,&tfc,&wgc_global[0]);	// make sure this is done before calling master()/workgroup()
	MPI_Barrier(BaseComm);

	if( !berr ){ // berr is always 'true' in this version 08.23
		exit(1);
	}

/* * * * * * * * * * * * * * * * *
 * TASK FARM CONFIGURATION DONE
 * * * * * * * * * * * * * * * * */

	/* * *
		timing start
	* * */
	start_t = get_time();
	getCurrentDateTime(currentTime);

	// PRINT <stdout>
	print_stdout(tfc.brank,"\n");
	sprintf(msg," TaskFarm starts at : %s\n",currentTime); print_stdout(tfc.brank,msg);
	print_stdout(tfc.brank,"\n");
	layout_line_stdout(tfc.brank,'-',80);
	fflush_channel(NULL);
	MPI_Barrier(BaseComm);

	/* * * * *
	 * TASK FARM MAIN START
	 * * * * **/
	if( tfc.brank == tfc.mrank ){
		master_worker_task_call_master( &BaseComm, &tfc, &wgc_global[0] );
// -------------------------------------------------------------------------------- -> 31.08 REFACTORING
		//fprintf(stdout,"MASTER - TaskFarmMain> Finalising MASTER > \n");
	}
	else{
		master_worker_task_call_workgroup( &BaseComm, &WorkgroupComm, tfc.n_workgroup, tfc.workgroup_tag );
		//fprintf(stdout,"WORKER - TaskFarmMain> Finalising Workgroups: base_rank : %d - workgroup_tag %d -  wr / ws: %d / %d \n",tfc.brank,tfc.workgroup_tag,tfc.worker_rank,tfc.workgroup_size);
	}
	/* * * * *
	 * TASK FARM MAIN END
	 * * * * */
	MPI_Barrier(BaseComm);

	/* * *
		timing end
	* * */
	end_t = get_time();
	getCurrentDateTime(currentTime);

	// PRINT <stdout>
	print_stdout(tfc.brank,"\n");
	sprintf(msg," TaskFarm ends at   : %s\n",currentTime); print_stdout(tfc.brank,msg);
	sprintf(msg," Elapsed time (s)   : %.8lf\n",end_t - start_t); print_stdout(tfc.brank,msg);
	print_stdout(tfc.brank,"\n");
	layout_line_stdout(tfc.brank,'-',80);
	fflush_channel(NULL);
	MPI_Barrier(BaseComm);

	MPI_Comm_free(&WorkgroupComm);
	MPI_Finalize();

	return 0;
}
