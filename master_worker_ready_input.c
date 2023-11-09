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

#include "master_worker_ready_input.h"
#include "subroutines.h"
#include "error.h"
#include "timer.h"

/* * *
 * get next TaskEnvelope
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

/* * *
 * set TaskEnvelope ( instructions to 'workgroup' )
 * used only in this source
 * * */
void set_TaskEnvelope(
    const int task_id,
    const char* app,
    MasterWorkspace* mws,
    TaskEnvelope* task
){
	/* * *
	 * APPLICATION GULP
	 * * */
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
	/* * *
	 * APPLICATION GULP --------------------------------------------------------------------------------------------------------
	 * * */
    
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
//bool master_worker_task_call_master(
bool ready_input_call_master(
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

		fprintf(iomaster,"Error: failed to open a log file for the master workgroup. Terminating all workgroups ...\n");

		// termination message : REFACTORING REQ ----------------------------------------------------------------
		for(int n=0;n<master_tag;n++){

        	TaskEnvelope end_task;
        	end_task.task_status = TASK_DIETAG;
        	MPI_Send(&end_task,sizeof(TaskEnvelope),MPI_CHAR,wgc[n].base_rank,TASK_DIETAG,*base_comm);
			getCurrentDateTime(currentTime);
			fprintf(iomaster," %.30s MPI_Send | completed | master         > workgroup %5d : size %5d : head_procid %6d\n", currentTime,wgc[n].workgroup_tag,wgc[n].workgroup_size,wgc[n].base_rank);
			fflush(iomaster);

		}
		// ------------------------------------------------------------------------------------------------------
		berr = false;
		return berr;
	}//else : continue ...

	/* * *
	   TASK CONFIGURATION
	 * * */
	getCurrentDateTime(currentTime);
	fprintf(iomaster," * * * \n");
	fprintf(iomaster," %.30s Task envelopes setting start",currentTime);
	fprintf(iomaster," * * * \n");

	for(int i=0;i<task_count;i++){
		task_id = i + tfc->task_start;	// syntax -> "A${task_id}.gin"

		// devtmp: set this case as 'application == gulp' ?
		set_TaskEnvelope( task_id, tfc->application, &mws, &(task_array[i]) );
	}

	getCurrentDateTime(currentTime);
	fprintf(iomaster," * * * \n");
	fprintf(iomaster," %.30s Task envelopes setting finish",currentTime);
	fprintf(iomaster," * * * \n");
	fflush(iomaster);

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

		/* pre-set for message sending */
		fprintf(iomaster," >>>>>>>> task send (initial)\n");
		fprintf(iomaster," %.30s MPI_Send | completed\n",currentTime);
		fprintf(iomaster," * message > base_rank: %5d\n",wgc[n].base_rank);
		fprintf(iomaster," | workgroup : %6d | size        : %5d\n",wgc[n].workgroup_tag,wgc[n].workgroup_size);
		fprintf(iomaster," | task_id   : %6d | task_status : %5d\n",task->task_id,task->task_status);
		fprintf(iomaster," * task contents\n");
		fprintf(iomaster," application    : %s\n",task->application);
		fprintf(iomaster," task_outpath   : %s\n",task->task_iopath);
		fprintf(iomaster," * shell commands : %d\n",task->cmd_count);
		for(int i=0;i<task->cmd_count;i++){
		fprintf(iomaster," | %2d | %s\n",i+1,task->cmd[i]);
		}
	}
	fflush(iomaster);
	// Initial task messaging end

	/* * * * *
		task messaging
	 * * * * */
	task = get_next_TaskEnvelope(task_array,task_count,&sent_task_count);

	while( task != NULL ){

		MPI_Recv(&taskres,sizeof(TaskResultEnvelope),MPI_CHAR,MPI_ANY_SOURCE,MPI_ANY_TAG,*base_comm,&status);
		getCurrentDateTime(currentTime);

		/* pre-set for message recving */
		fprintf(iomaster," <<<<<<<< task result recv (task-farming)\n");
		fprintf(iomaster," %.30s MPI_Recv | completed\n",currentTime);
		fprintf(iomaster," * message < base_rank: %5d\n",status.MPI_SOURCE);
		fprintf(iomaster," | workgroup : %6d | size        : %5d\n",wgc[taskres.workgroup_tag].workgroup_tag,wgc[taskres.workgroup_tag].workgroup_size);
		fprintf(iomaster," | task_id   : %6d | task_status : %5d\n",taskres.task_id,taskres.task_status); 
		fprintf(iomaster," * task result\n");
		fprintf(iomaster," | task starts : %.30s | task ends : %.30s\n",taskres.start_t,taskres.end_t);
		fprintf(iomaster," | elapsed_t   : %.8f\n",taskres.elapsed_t);
		// warning handling start
		if( !taskres.inputfile_check ){
		fprintf(iomaster," * warning | inputfile not found, application was not launched\n");
		}
		// warning handling end

		MPI_Send(task,sizeof(TaskEnvelope),MPI_CHAR,status.MPI_SOURCE,TASK_WORKTAG,*base_comm);	// using ... MPI handle ... MPI_Status stauts -> MPI_SOURCE (send back to right previous 'recv' source)
		getCurrentDateTime(currentTime);

		/* pre-set for message sending */
		fprintf(iomaster," >>>>>>>> task send (task-farming)\n");
		fprintf(iomaster," %.30s MPI_Send | completed\n",currentTime);
		fprintf(iomaster," * message > base_rank: %5d\n",wgc[taskres.workgroup_tag].base_rank);
		fprintf(iomaster," | workgroup : %6d | size        : %5d\n",wgc[taskres.workgroup_tag].workgroup_tag,wgc[taskres.workgroup_tag].workgroup_size);
		fprintf(iomaster," | task_id   : %6d | task_status : %5d\n",task->task_id,task->task_status);
		fprintf(iomaster," * task contents\n");
		fprintf(iomaster," application    : %s\n",task->application);
		fprintf(iomaster," task_outpath   : %s\n",task->task_iopath);
		fprintf(iomaster," * shell commands : %d\n",task->cmd_count);
		for(int i=0;i<task->cmd_count;i++){
		fprintf(iomaster," | %2d | %s\n",i+1,task->cmd[i]);
		}
		task = get_next_TaskEnvelope(task_array,task_count,&sent_task_count);
		
		fflush(iomaster);
	}

	// Final Recv : debugging 07.09.23 - HANG occurs : waiting a message from some 'workgroups' never sending message back since they are on running GULP (generating too many Errors, which never end).23
	for(int n=0;n<master_tag;n++){

		MPI_Recv(&taskres,sizeof(TaskResultEnvelope),MPI_CHAR,MPI_ANY_SOURCE,MPI_ANY_TAG,*base_comm,&status);
		getCurrentDateTime(currentTime);

		/* pre-set for message recving */
		fprintf(iomaster," <<<<<<<< task result recv (final)\n");
		fprintf(iomaster," %.30s MPI_Recv | completed\n",currentTime);
		fprintf(iomaster," * message < base_rank: %5d\n",status.MPI_SOURCE);
		fprintf(iomaster," | workgroup : %6d | size        : %5d\n",wgc[taskres.workgroup_tag].workgroup_tag,wgc[taskres.workgroup_tag].workgroup_size);
		fprintf(iomaster," | task_id   : %6d | task_status : %5d\n",taskres.task_id,taskres.task_status); 
		fprintf(iomaster," * task result\n");
		fprintf(iomaster," | task starts : %.30s | task ends : %.30s\n",taskres.start_t,taskres.end_t);
		fprintf(iomaster," | elapsed_t   : %.8f\n",taskres.elapsed_t);

		// warning handling start
		if( !taskres.inputfile_check ){
		fprintf(iomaster," * warning | inputfile not found, application was not launched\n");
		}
		// warning handling end
		fflush(iomaster);
	}

	fprintf(iomaster," * * * * * * * * * * *\n");
	fprintf(iomaster," * Finalising\n");
	fprintf(iomaster," * * * * * * * * * * *\n");
	fflush(iomaster);

	// Termination message
	for(int n=0;n<master_tag;n++){

		TaskEnvelope end_task;
		end_task.task_id = -1;
		end_task.task_status = TASK_DIETAG;

		MPI_Send(&end_task,sizeof(TaskEnvelope),MPI_CHAR,wgc[n].base_rank,TASK_DIETAG,*base_comm);
		getCurrentDateTime(currentTime);

		/* pre-set for message send */
		fprintf(iomaster," >>>>>>>> kill workgroups\n");
		fprintf(iomaster," %.30s MPI_Send | completed\n",currentTime);
		fprintf(iomaster," * message > base_rank: %5d\n",wgc[n].base_rank);
		fprintf(iomaster," | workgroup : %6d | size        : %5d\n",wgc[n].workgroup_tag,wgc[n].workgroup_size);
		fprintf(iomaster," | task_id   : %6d | task_status : %5d\n",end_task.task_id,end_task.task_status);

		//fprintf(iomaster," MPI_Send (DIE-TAG) complete: master -> %d (base-rank)\n",wgc[n].base_rank);
		fflush(iomaster);
	}

	free(task_array);
	fclose(iomaster);

	return berr;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void ready_input_call_workgroups(
	const MPI_Comm* base_comm,
	const MPI_Comm* workgroup_comm,
	const int n_workgroup,
	const int workgroup_tag
){
	bool taskError;

	char currentTime[64];
	const int max_recv_cycle = 9999999;	

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

	char cwd[512];

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

	for(int cycle=0;cycle<max_recv_cycle;cycle++){

		for(int n=0;n<workgroup_count;n++){
			// TaskEnvelope Recv : by each head rank (0) of 'workgroups'
			if( n == workgroup_tag && worker_rank == 0 ){
				/* 
					Note. 07.09.23 : all workgroups will wait here (when all the tasks are done) for 'DIE' message from 'master'.
				*/
				MPI_Recv(&task,sizeof(TaskEnvelope),MPI_CHAR,master_base_rank,MPI_ANY_TAG,*base_comm,&status);
				getCurrentDateTime(currentTime);

				if( status.MPI_TAG == TASK_WORKTAG ){

					fprintf(ioworkgroup," <<<<<<<< task recv\n");// workgroup-tag : %d\n",workgroup_tag);
					fprintf(ioworkgroup," %.30s MPI_Recv | completed\n",currentTime);
					fprintf(ioworkgroup," application      : %s ( %p )\n",task.application,task.app_ptr);
					fprintf(ioworkgroup," task_id          : %d\n",task.task_id);
					fprintf(ioworkgroup," task_status      : %d (work-tag)\n",task.task_status);
					fprintf(ioworkgroup," task_iopath      : %s\n",task.task_iopath);
					fprintf(ioworkgroup," inputfile count  : %d\n",task.inputfile_count);
					fprintf(ioworkgroup," * shell commands : %d\n",task.cmd_count);
					for(int i=0;i<task.cmd_count;i++){
					fprintf(ioworkgroup," | %2d | %s\n",task.cmd_count,task.cmd[i]);
					}
					fflush(ioworkgroup);

				}
				else if( status.MPI_TAG == TASK_DIETAG ){

					fprintf(ioworkgroup," <<<<<<<< kill workgroups\n");// : workgroup-tag : %d\n",workgroup_tag);
					fprintf(ioworkgroup," %.30s MPI_Recv | completed\n",currentTime);
					fprintf(ioworkgroup," task_status      : %d (die-tag)\n",task.task_status);
					fflush(ioworkgroup);
				}
			}
			/* * *
				after identifying task type
			* * */
			if( n == workgroup_tag ){

				if( workgroup_size > 1 ){	// call 'MPI_Bcast()' if workgroup size > 1

					MPI_Bcast(&task,sizeof(TaskEnvelope),MPI_CHAR,0,*workgroup_comm);
					getCurrentDateTime(currentTime);
				
					if( worker_rank == 0 ){
						fprintf(ioworkgroup," %.30s MPI_Bcast| completed - internal task messaging within workgroup\n",currentTime);
						fflush(ioworkgroup);
					}	
				}
				task.workgroup_tag = n;	// used when launching application

				// -------- closing workgroups : if workgroup closure message
				if( task.task_status == TASK_DIETAG ){

					if( worker_rank == 0 ){
						fprintf(ioworkgroup," * * * call return * * *\n");
						fflush(ioworkgroup);		
						fclose(ioworkgroup);		// closing iochannel
					}
					MPI_Barrier(*workgroup_comm);
					return;
				}
				// -------- closing workgroups end

				/* * *
					workgroup in action - task_status != TASK_DIETAG
				* * */

				/* 1. create working directory and check if relevanat file exists */
				if( worker_rank == 0 ){	// instructions by 'head' processor of workgroup
					fprintf(ioworkgroup," * file check\n");
					mkdir(task.task_iopath,0777);	// mkdir working_directory
				}
				
				/* 2. inputfile check */
				for(int i=0;i<task.inputfile_count;i++){

					taskres.inputfile_check = error_file_exists( task.inputfile_path[i] );

					if( taskres.inputfile_check ){
						if( worker_rank == 0 ){
							fprintf(ioworkgroup," | file %2d | found: %s\n",i+1,task.inputfile_path[i]);
							system(task.cmd[i]);	// do shell instruction
						}
					}
					else{
						if( worker_rank == 0 ){
							fprintf(ioworkgroup," | file %2d | NotFoundErr : %s\n",i+1,task.inputfile_path[i]);
						}
						break;
					}
				}

				if( worker_rank == 0 ){
					fprintf(ioworkgroup," * file check end\n");
					fflush(ioworkgroup);
				}
				MPI_Barrier(*workgroup_comm);		// need to wait until mkdir / system done otherwise 'chdir' following cannot be done properly

	/*
		Application Launch Section
	*/
				// workgroup processors : move into the working dir
				chdir(task.task_iopath);
			/* * *
				application launching : e.g., extern void application( const MPI_Comm*, char*, int*, int* );
			* * */
				getCurrentDateTime(taskres.start_t);
				taskres.elapsed_t = get_time();

				if( taskres.inputfile_check ){
					/* * *
						app execution
					* * */
					task.app_ptr(workgroup_comm,task.task_iopath,&task.task_id,&task.workgroup_tag);
				}
	
				// DEBUGGING
				//if( worker_rank == 0 ){
				//	fprintf(ioworkgroup," DEBUGGING: AFTER GULP RUN\n");
				//	fflush(ioworkgroup);
				//}
				//MPI_Barrier(*workgroup_comm);		// need to wait until mkdir / system done otherwise 'chdir' following cannot be done properly
				// DEBUGGING END

				getCurrentDateTime(taskres.end_t);
				taskres.elapsed_t = get_time() - taskres.elapsed_t;

				MPI_Barrier(*workgroup_comm);
			/*  application launching done */

				// get out from the working dir <important> to keep gulpmain from being in race condition of getting channel 'gulptmp_*' - wkjee 11 July 2023 added
				chdir(task.task_rootpath);

			/* * *
				set taskres <TaskResultEnvelope>
			* * **/
				taskres.task_status = TASK_FINISHED;
				taskres.task_id = task.task_id;
				taskres.workgroup_tag = task.workgroup_tag;
				//taskres.inputfile_check -> done above

				/* * *
					error starts (logging)
				* * */
				if( worker_rank == 0 ){

					int error_count = 1;

					fprintf(ioworkgroup," * warnings\n");
					// 1. file check
					if( !taskres.inputfile_check ){
						fprintf(ioworkgroup," | %2d | inputfile not found\n",error_count);
						error_count++;
					}
					fprintf(ioworkgroup," * warnings end\n");
					fflush(ioworkgroup);
				}
				/* * *
					 error end
				* * */

				/* * *
					task result send back > master
				* * */
				if( worker_rank == 0 ){
					MPI_Send(&taskres,sizeof(TaskResultEnvelope),MPI_CHAR,master_base_rank,taskres.task_status,*base_comm);
					getCurrentDateTime(currentTime);

					fprintf(ioworkgroup," >>>>>>>> task result send\n");
					fprintf(ioworkgroup," %.30s MPI_Send | completed\n",currentTime);
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
		MPI_Barrier(*workgroup_comm);
	}
	// workgroup logger end

	return;
}
