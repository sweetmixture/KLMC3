/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <mpi.h>

#include "master_worker_task.h"
#include "subroutines.h"
#include "error.h"
#include "timer.h"

/* * *
 * use only in this source
 * * */
TaskEnvelope* get_next_TaskEnvelope(
	TaskEnvelope* task_array,
	const int task_count,
	int* sent_task_count
){
	if( *sent_task_count >= task_count ){
		return NULL;
	}
	TaskEnvelope* task = &task_array[*sent_task_count];
	(*sent_task_count)++;

	return task;
}

/*
	set TaskEnvelope ( instructions to 'workgroup' )
 */
void set_TaskEnvelope(
    const int task_id,
    const char* app,
    MasterWorkspace* mws,
    TaskEnvelope* task
){
	// APPLICATION GULP
    if( strcmp(app,"gulp") == 0 ){

        mws->inputfile_count = 1;                       // set inputfile count: gulp (1) *

        sprintf(mws->inputfile[0],"A%d.gin",task_id);   // set input file name		// e.g. 'A123.gin'
        sprintf(mws->rundir,"A%d",task_id);             // set app run directory	// e.g. 'A123'

        // 1. set mws 'inputfile path'
        memset(mws->inputfile_path[0],0,sizeof(mws->inputfile_path[0]));
        strcpy(mws->inputfile_path[0],mws->inputsource_dir);						// e.g. '/root/run'
		strcat(mws->inputfile_path[0],"/");											// e.g. '/root/run/'
        strcat(mws->inputfile_path[0],mws->inputfile[0]);							// e.g. '/root/run/A123.gin'

        // 2. set mws 'rundir_path'
        memset(mws->rundir_path,0,sizeof(mws->rundir_path));
        strcpy(mws->rundir_path,mws->root);											// e.g. '/root/
		strcat(mws->rundir_path,"/");												// e.g. '/root/
        strcat(mws->rundir_path,mws->rundir);										// e.g. '/root/run'

        /* ------------------------------------------------------------------- */
        // 3. set TaskEnvelope 'task'
        task->cmd_count = 1;								// mws->inputfile_count;

        for(int i=0;i<task->cmd_count;i++){
            memset(task->cmd[i],0,sizeof(task->cmd[i]));
			memset(task->inputfile_path[i],0,sizeof(task->inputfile_path[i]));
        }

		strcpy(task->application,"gulp");					// Application
		task->app_ptr = gulpklmc;							// set 'gulp_main()' subroutine
		task->task_id = task_id;							// set task_id
        strcpy(task->task_iopath,mws->rundir_path);			// set task_iopath
        strcpy(task->task_rootpath,mws->root);				// set task_rootpath
        task->task_status = TASK_INIT;						// set task_status

		task->inputfile_count = mws->inputfile_count;
        // gulp main input copy command
        strcpy(task->cmd[0],"cp ");							// cp
        strcat(task->cmd[0],mws->inputfile_path[0]);		// cp /abs/path/inputfile_path/
        strcat(task->cmd[0]," ");							// cp /abs/path/inputfile_path/ 
        strcat(task->cmd[0],mws->rundir_path);				// cp /abs/path/inputfile_path/	/abs/path/rundir_path
        strcat(task->cmd[0],"/gulp_klmc.gin");				// cp /abs/path/inputfile_path/ /abs/path/rundir_paht/gulp_klmc.gin

		strcpy(task->inputfile_path[0],mws->inputfile_path[0]);	// set task inputfile_path (absolute)
        
        return;
    }
    
	// APPLICATION FHIAIMS
    if( strcmp(app,"fhiaims") == 0 ){
        
 		// Not Implemented 01.09.23       
        return;
    }
}

/* * * * *

	Master function

		task configure / distribution /  ...

		updates: 

* * * * */
bool master_worker_task_call_master(
	const MPI_Comm* base_comm,
	const TaskFarmConfiguration* tfc,
	const WorkgroupConfig* wgc
){
	bool berr = true;
	char currentTime[64];

	/* * *
		Files
	* * */
	FILE* iomaster = NULL;
	MasterWorkspace mws;

	/* * *
		Tasks
	* * */
	int task_id;
	int sent_task_count  = 0;
	const int task_count = tfc->num_tasks;
	const int master_tag = tfc->n_workgroup - 1;
	TaskEnvelope* task_array = malloc(task_count*sizeof(TaskEnvelope));

	/* * * END VARIABLES * * */

	// Get basic filesystem environment: (1) root path (2) input source path
	getcwd(mws.root,sizeof(mws.root));
	strcpy(mws.inputsource_dir,mws.root);
	strcat(mws.inputsource_dir,"/run");

	// Try: open log file 'master.log'
	iomaster = fopen(_LOGFILE_MASTER_,"w");

	// Channel 'iomaster' open check
	if( iomaster == NULL ){

		// termination message : REFACTORING REQ ----------------------------------------------------------------
		for(int n=0;n<master_tag;n++){
        	TaskEnvelope end_task;
        	end_task.task_status = TASK_DIETAG;
        	MPI_Send(&end_task,sizeof(TaskEnvelope),MPI_CHAR,wgc[n].base_rank,TASK_DIETAG,*base_comm);
        	//MPI_Send(0,0,MPI_CHAR,wgc[n].base_rank,TASK_DIETAG,*base_comm);
        	fprintf(iomaster,"MASTER - DIETAG > MPI_Send complete: master -> %d (base-rank)\n",wgc[n].base_rank);
			fflush(iomaster);
		}
		// ------------------------------------------------------------------------------------------------------
		berr = false;
		return berr;
	}
	//else : continue ...

	/* * * * *
	 * TASK CONFIGURATION
	 * * * * */
	for(int i=0;i<task_count;i++){
	
		task_id = i + tfc->task_start;	// syntax -> "A${task_id}.gin"

		set_TaskEnvelope( task_id, tfc->application, &mws, &(task_array[i]) );

		// < PRINT S/T >
		// fprintf(iomaster,"MASTER> working path: %s\n",task_array[i].task_iopath);

	}
	fprintf(iomaster," * * * \n");
	fprintf(iomaster," Task envelopes setting done\n");
	fprintf(iomaster," * * * \n");
	fflush(iomaster);

// 05.09.23 Refactoring Target ------------------------------------------------------------------------------------------------------------------------------------------
// master_worker_task.h

	/*
		 messaging tasks variables
	*/
	MPI_Status status;
	MPI_Request request;
	TaskEnvelope* task;
	TaskResultEnvelope taskres;
	
	/* * * * *
		Initial task messaging 
	 * * * * */
	for(int n=0;n<master_tag;n++){

		task = get_next_TaskEnvelope(task_array,task_count,&sent_task_count);

		if( task == NULL ){
			break;
		}

		MPI_Isend(task,sizeof(TaskEnvelope),MPI_CHAR,wgc[n].base_rank,TASK_WORKTAG,*base_comm,&request);
		MPI_Wait(&request,&status);

		getCurrentDateTime(currentTime);

		fprintf(iomaster," %.30s TaskEnvelope sent | task_id %6d > workgroup %5d | size %5d | head_procid %6d\n", currentTime,task->task_id,wgc[n].workgroup_tag,wgc[n].workgroup_size,wgc[n].base_rank);
	}
	fflush(iomaster);
	// Initial task messaging end

	/* * * * *
		task messaging
	 * * * * */
	task = get_next_TaskEnvelope(task_array,task_count,&sent_task_count);

	while( task != NULL ){

		MPI_Recv(&taskres,sizeof(TaskResultEnvelope),MPI_CHAR,MPI_ANY_SOURCE,MPI_ANY_TAG,*base_comm,&status);
		fprintf(iomaster," MPI_Recv complete: from %d - task_id: %d \n",status.MPI_SOURCE,taskres.task_id);

		MPI_Send(task,sizeof(TaskEnvelope),MPI_CHAR,status.MPI_SOURCE,TASK_WORKTAG,*base_comm);	// using ... MPI handle ... MPI_Status stauts -> MPI_SOURCE (send back to right previous 'recv' source)
		fprintf(iomaster," MPI_Send complete: master -> %d (base-rank) - task_id: %d\n",status.MPI_SOURCE,task->task_id);

		task = get_next_TaskEnvelope(task_array,task_count,&sent_task_count);
		
		fflush(iomaster);
	}

	// Final Recv
	for(int n=0;n<master_tag;n++){

		MPI_Recv(&taskres,sizeof(TaskResultEnvelope),MPI_CHAR,MPI_ANY_SOURCE,MPI_ANY_TAG,*base_comm,&status);
		fprintf(iomaster," MPI_Recv complete: from %d - task_id: %d \n",status.MPI_SOURCE,taskres.task_id);
	}

	// Termination message
	for(int n=0;n<master_tag;n++){

			TaskEnvelope end_task;
			end_task.task_status = TASK_DIETAG;

			MPI_Send(&end_task,sizeof(TaskEnvelope),MPI_CHAR,wgc[n].base_rank,TASK_DIETAG,*base_comm);
			//MPI_Send(0,0,MPI_CHAR,wgc[n].base_rank,TASK_DIETAG,*base_comm);
			fprintf(iomaster," MPI_Send (DIE-TAG) complete: master -> %d (base-rank)\n",wgc[n].base_rank);
	}

	free(task_array);
	fclose(iomaster);

	return berr;
}

// ----------------------------------------------------------------------------------------------------------------------

void master_worker_task_call_workgroup( const MPI_Comm* base_comm, const MPI_Comm* workgroup_comm, const int n_workgroup, const int workgroup_tag )
{
	int brank,bsize;
	int workgroup_size,worker_rank;

	MPI_Comm_size(*base_comm,&bsize);
	MPI_Comm_rank(*base_comm,&brank);
	
	MPI_Comm_size(*workgroup_comm,&workgroup_size);
	MPI_Comm_rank(*workgroup_comm,&worker_rank);

	const int workgroup_count = n_workgroup - 1;
	const int master_base_rank = bsize - 1;

	MPI_Status status;
	MPI_Request request;

	TaskEnvelope task;
	TaskResultEnvelope taskres;

	// workgroup logger
	FILE* ioworkgroup = NULL;
	char ioworkgroup_log_files[128];
	sprintf(ioworkgroup_log_files,"workgroup_%d.log",workgroup_tag);

	/*
		Open 'workgroup' log files
	*/
	for(int n=0;n<workgroup_count;n++){
		if( n == workgroup_tag && worker_rank == 0 ){
			ioworkgroup = fopen(ioworkgroup_log_files,"w");
		}
	}

	// iopath control
	char cwd[512];

	for(;;){

		for(int n=0;n<workgroup_count;n++){

			// task recv - head process of each workgroup
			if( n == workgroup_tag && worker_rank == 0 ){
				MPI_Recv(&task,sizeof(TaskEnvelope),MPI_CHAR,master_base_rank,MPI_ANY_TAG,*base_comm,&status);

				if( status.MPI_TAG == TASK_WORKTAG ){

					fprintf(ioworkgroup,"--------------------------------------------------------------------------------\n");
					fprintf(ioworkgroup," * TaskEnvelope recv - workgroup %d\n",workgroup_tag);
					fprintf(ioworkgroup," task_id         : %d\n",task.task_id);
					fprintf(ioworkgroup," application     : %s ( %p )\n",task.application,task.app_ptr);
					fprintf(ioworkgroup," task_status     : %d (work-tag)\n",task.task_status);
					fprintf(ioworkgroup," task_iopath     : %s\n",task.task_iopath);
					fprintf(ioworkgroup," inputfile count : %d\n",task.inputfile_count);
					fprintf(ioworkgroup,"     * shell instructions\n");
					for(int i=0;i<task.cmd_count;i++){
					fprintf(ioworkgroup,"     %2d | %s\n",task.cmd_count,task.cmd[i]);
					fflush(ioworkgroup);
					}
					fprintf(ioworkgroup,"\n");
					fflush(ioworkgroup);

				/* deprecated 05.09.23

					//printf("WORKGROUP [%d] > MPI_Recv complete : task %p %d %d\n",workgroup_tag,task.app_ptr,task.task_id,task.task_status);
					fprintf(ioworkgroup,"--------------------------------------------------------------------\n");
					fprintf(ioworkgroup,"WORKGROUP [%d] > MPI_Recv complete : task %p %d %d\n",workgroup_tag,task.app_ptr,task.task_id,task.task_status);
					fflush(ioworkgroup);
				*/
				}
				else if( status.MPI_TAG == TASK_DIETAG ){

					fprintf(ioworkgroup,"--------------------------------------------------------------------------------\n");
					fprintf(ioworkgroup," * TaskEnvelope recv - workgroup %d\n",workgroup_tag);
// 05.09.23 Refactoring Target ------------------------------------------------------------------------------------------------------------------------------------------
// master_worker_task.h
				/* deprecated 05.09.23
					//printf("WORKGROUP [%d] > MP_Recv DIETAG complete\n",workgroup_tag);
					fprintf(ioworkgroup,"********************************************************************\n");
					fprintf(ioworkgroup,"WORKGROUP [%d] > MPI_Recv complete : task %p %d %d\n",workgroup_tag,task.app_ptr,task.task_id,task.task_status);
					fflush(ioworkgroup);
				*/
				}
			}

			// workgroup interanl bcast - task
			if( n == workgroup_tag ){

				MPI_Bcast(&task,sizeof(TaskEnvelope),MPI_CHAR,0,*workgroup_comm);

				if( worker_rank == 0 ){
					//printf("WORKGROUP [%d] > MPI_Bcast complete : worker_rank [%d] task %p %d %d\n",workgroup_tag,worker_rank,task.app_ptr,task.task_id,task.task_status);
					fprintf(ioworkgroup,"WORKGROUP [%d] > MPI_Bcast complete : worker_rank [%d] task %p %d %d\n",workgroup_tag,worker_rank,task.app_ptr,task.task_id,task.task_status);
					fflush(ioworkgroup);
				}
				// set workgroup tag
				task.worker_id = n;
		
				if( task.task_status == TASK_DIETAG ){
					return;
				}

				// Create working directory and put relevant *.gin
				if( worker_rank == 0 ){
					mkdir(task.task_iopath,0777);	// mkdir working_directory

					// execute commands -- 01.09.23 temporal added (replace REQ)
					for(int i=0;i<task.cmd_count;i++){

						// Target FileExists Check REQ
						bool fileExists = error_file_exists( task.inputfile_path[i] );
						if( fileExists ){
							fprintf(ioworkgroup,"WORKGROUP [%d]: inptufile %d exists: %s\n",workgroup_tag,i,task.inputfile_path[i]);
							fflush(ioworkgroup);
						}
						else{
							fprintf(ioworkgroup,"WORKGROUP [%d]: inptufile %d NOTexists: %s\n",workgroup_tag,i,task.inputfile_path[i]);
							fflush(ioworkgroup);
						}
						// Call system preset scripts
						system(task.cmd[i]);
					}
				}
				MPI_Barrier(*workgroup_comm);		// need to wait until mkdir / system done otherwise 'chdir' following cannot be done properly

				// get into the working dir
				chdir(task.task_iopath);
				getcwd(cwd,sizeof(cwd));

				if( worker_rank == 0 ){
					fprintf(ioworkgroup,"WORKGROUP [%d]: Task working directory: %s\n",workgroup_tag,cwd);
					fflush(ioworkgroup);
				}
				MPI_Barrier(*workgroup_comm);
			
				/*	* * *
					Launch GULP : extern void gulpklmc( const MPI_Comm*, char*, int*, int* );
				*	* * */
				task.task_status = TASK_EXECUTED;
				task.app_ptr(workgroup_comm,task.task_iopath,&task.task_id,&task.worker_id);

				// get out from the working dir <important> to keep gulpmain from the race condition of getting channel 'gulptmp_*' - wkjee 11 July 2023 added
				chdir(task.task_rootpath);
				getcwd(cwd,sizeof(cwd));

				if( worker_rank == 0 ){
					fprintf(ioworkgroup,"WORKGROUP [%d]: Task master directory: %s\n",workgroup_tag,cwd);
					fflush(ioworkgroup);
				}
				MPI_Barrier(*workgroup_comm);

				task.task_status = TASK_FINISHED;
				//if( worker_rank == 0 ){ printf("after  run / status %d\n",task.task_status); }

				/*
					 set TaskResultEnvelope
				*/
				taskres.task_status = TASK_FINISHED;
				taskres.task_id = task.task_id;
				taskres.worker_id = task.worker_id;

				// send back to Master
				if( worker_rank == 0 ){
					MPI_Send(&taskres,sizeof(TaskResultEnvelope),MPI_CHAR,master_base_rank,taskres.task_status,*base_comm);
					fprintf(ioworkgroup,"WORKGROUP [%d] > MPI_Send complete <result callback to master> : task %p %d %d\n",workgroup_tag,task.app_ptr,task.task_id,task.task_status);
					fflush(ioworkgroup);
				}
				MPI_Barrier(*workgroup_comm);
			}
		}
	}

	// workgroup logger
	for(int n=0;n<workgroup_count;n++){
		if( n == workgroup_tag && worker_rank == 0 ){
			fclose(ioworkgroup);
		}
	}
	// workgroup logger

	return;
}
