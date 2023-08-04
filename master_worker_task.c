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

#include "taskfarm_def.h"	// WorkgroupConfig (struct)
#include "master_worker_task.h"	// function_task (struct) result_pacakge (struct)

#include "subroutines.h"		// subprogram_pi( MPI_Comm* workgroup_comm, int rand_seed )

// reference 2 - gulpklmc<prototype>
//extern void gulpklmc( const MPI_Comm*, char*, int*, int* );
/* subroutine gulpklmc( MPI_comm_klmc, klmc_task_iopath, klmc_task_id, klmc_worker_id ) bind (C,name="gulpklmc")
 * integer,             intent(in) :: MPI_Comm
 * characeter(len=256), intent(in) :: klmc_task_iopath
 * integer,             intent(in) :: klmc_task_id
 * integer,             intent(in) :: klmc_worker_id
 */

/*
	04082023:	1. File generation internal function
*/



function_task* get_next_task( function_task* task_array, const int task_count, int* sent_task_count )
{
	//printf("task_count / sent_task_count : %d / %d\n",task_count,*sent_task_count);
	if( *sent_task_count >= task_count ) return NULL;

	function_task* task = &task_array[*sent_task_count];
	(*sent_task_count)++;

	return task;
}


//void master_worker_task_call_master( const MPI_Comm* base_comm, const WorkgroupConfig* wc, const int n_workgroup, const int task_count )
void master_worker_task_call_master( const MPI_Comm* base_comm, const WorkgroupConfig* wc, const int n_workgroup, const int task_start, const int task_end )
{
	FILE* iomaster = NULL;
	iomaster = fopen("master.log","w");

	const int master_tag = n_workgroup - 1;
 	
	/* access to a head (base_rank) of each workgroup: wc[i].base_rank / i: 0 - master_tag  */

	char systemcmd[1024];

	char root[512];	// const
	char source_dir[512]; // const

	char inputfile_tmp[512];
	char rundir_tmp[512];
	char filetmp[64];
	char dirtmp[64];

	char togulpklmc[512];

	// set root
	getcwd(root,sizeof(root));
	// set source_dir
	strcpy(source_dir,root);
	strcat(source_dir,"/run/");


	// initilise tasks
	int sent_task_count = 0;
	const int task_count = task_end - task_start + 1;
	int task_id;
	function_task* task_array = malloc(task_count*sizeof(function_task));

	for(int i=0;i<task_count;i++){
	
		// task_id = * (in A*.gin)
		task_id = i + task_start;

		// some systemcalls to generate file structure
		// 1. create directories
		// 2. copy gulp input files into the directory

		// mkdir + copying inputfile to the directories
		memset(filetmp,0,sizeof(filetmp));
		memset(dirtmp,0,sizeof(dirtmp));
		sprintf(filetmp,"A%d.gin",task_id);
		sprintf(dirtmp,"A%d",task_id);
		memset(inputfile_tmp,0,sizeof(inputfile_tmp));
		strcpy(inputfile_tmp,root);
		strcat(inputfile_tmp,"/run/");
		strcat(inputfile_tmp,filetmp);						// inputfile_tmp: /path/to/A*.gin
		memset(rundir_tmp,0,sizeof(rundir_tmp));
		strcpy(rundir_tmp,root);
		strcat(rundir_tmp,"/");
		strcat(rundir_tmp,dirtmp);							// rundir_tmp	: /path/to/	(i.e., working directory)

		// 1. setup command (copying)
		memset(togulpklmc,0,sizeof(togulpklmc));
		memset(systemcmd,0,sizeof(systemcmd));
		strcpy(systemcmd,"cp ");
		strcat(systemcmd,inputfile_tmp);
		strcat(systemcmd," ");
		strcpy(togulpklmc,rundir_tmp);
		strcat(togulpklmc,"/gulp_klmc.gin");
		strcat(systemcmd,togulpklmc);
		strcpy(task_array[i].syscmd,systemcmd);				// systemcmd	: cp inputfile_tmp rundir_tmp/gulp_klmc.gin


		// 2. setup command (AXX.gin -> gulp_klmc.gin)
			// memset(task_array[i].task_iopath,0,sizeof(task_array[i].task_iopath));
		memset(task_array[i].task_iopath,' ',sizeof(task_array[i].task_iopath));
		task_array[i].fp = gulpklmc;
		task_array[i].task_id = task_id;
		task_array[i].task_status = TASK_INIT;

		/* ! Note:
			sizeof: 'task_iopath' and 'rundir_tmp' must be in match // setup task working directory <Important!!!> - rundir_tmp -> length 512 - error !
		*/
		//sprintf(task_array[i].task_iopath,"%s",rundir_tmp);
		strcpy(task_array[i].task_iopath,rundir_tmp);		// task_array[i].task_iopath = rundir_tmp
		fprintf(iomaster,"MASTER> working path: %s\n",task_array[i].task_iopath);
		// setup task root path
		strcpy(task_array[i].task_rootpath,root);

	}
	fprintf(iomaster,"=============================================================================\n");
	fprintf(iomaster," Task configuration\n");
	fprintf(iomaster,"=============================================================================\n");
	fflush(iomaster);

	// messaging tasks
	MPI_Status status;
	MPI_Request request;

	function_task* task;
	result_package res;
	
	// Initial task messaging 
	for(int n=0;n<master_tag;n++){
		task = get_next_task(task_array,task_count,&sent_task_count);

		if( task == NULL ){ break; }
		//printf("send_count / task fp / id / status : %d %p %d %d\n",sent_task_count,task->fp,task->task_id,task->task_status);
		MPI_Isend(task,sizeof(function_task),MPI_CHAR,wc[n].base_rank,TF_WORKTAG,*base_comm,&request);
		MPI_Wait(&request,&status);

		fprintf(iomaster,"MASTER> Initial task send > MPI_Isend complete: master -> %d (base-rank) with [ tag, size ] = [ %d, %d ] - task_id: %d \n",wc[n].base_rank,wc[n].workgroup_tag,wc[n].workgroup_size,task->task_id);
	}
	fflush(iomaster);

	task = get_next_task(task_array,task_count,&sent_task_count);

	while( task != NULL ){

		MPI_Recv(&res,sizeof(result_package),MPI_CHAR,MPI_ANY_SOURCE,MPI_ANY_TAG,*base_comm,&status);
		fprintf(iomaster,"MASTER> MPI_Recv complete: from %d - task_id: %d \n",status.MPI_SOURCE,res.task_id);

		// logging 'res'

		MPI_Send(task,sizeof(function_task),MPI_CHAR,status.MPI_SOURCE,TF_WORKTAG,*base_comm);	// using ... MPI handle ... MPI_Status stauts -> MPI_SOURCE (send back to right previous 'recv' source)
		fprintf(iomaster,"MASTER> MPI_Send complete: master -> %d (base-rank) - task_id: %d\n",status.MPI_SOURCE,task->task_id);

		task = get_next_task(task_array,task_count,&sent_task_count);
		
		fflush(iomaster);
	}

	// Final Recv
	for(int n=0;n<master_tag;n++){

		MPI_Recv(&res,sizeof(result_package),MPI_CHAR,MPI_ANY_SOURCE,MPI_ANY_TAG,*base_comm,&status);
		fprintf(iomaster,"MASTER> MPI_Recv complete: from %d - task_id: %d \n",status.MPI_SOURCE,res.task_id);
	}

	// Termination message
	for(int n=0;n<master_tag;n++){

			function_task end_task;
			end_task.task_status = TASK_DIETAG;

			MPI_Send(&end_task,sizeof(function_task),MPI_CHAR,wc[n].base_rank,TF_DIETAG,*base_comm);
			//MPI_Send(0,0,MPI_CHAR,wc[n].base_rank,TF_DIETAG,*base_comm);
			fprintf(iomaster,"MASTER - DIETAG > MPI_Send complete: master -> %d (base-rank)\n",wc[n].base_rank);
	}

	free(task_array);
	fclose(iomaster);

	return;
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

	// recv buffer
	function_task task;
	// result
	result_package res;

	// workgroup logger
	FILE* ioworkgroup = NULL;
	char ioworkgroup_log_files[128];
	sprintf(ioworkgroup_log_files,"workgroup_%d.log",workgroup_tag);

	for(int n=0;n<workgroup_count;n++){
		if( n == workgroup_tag && worker_rank == 0 ){
			ioworkgroup = fopen(ioworkgroup_log_files,"w");
		}
	}
	// workgroup logger

	// iopath control
	char cwd[512];

	for(;;){

		for(int n=0;n<workgroup_count;n++){

			// task recv - head process of each workgroup
			if( n == workgroup_tag && worker_rank == 0 ){
				MPI_Recv(&task,sizeof(function_task),MPI_CHAR,master_base_rank,MPI_ANY_TAG,*base_comm,&status);

				if( status.MPI_TAG == TF_WORKTAG ){
					//printf("WORKGROUP [%d] > MPI_Recv complete : task %p %d %d\n",workgroup_tag,task.fp,task.task_id,task.task_status);
					fprintf(ioworkgroup,"--------------------------------------------------------------------\n");
					fprintf(ioworkgroup,"WORKGROUP [%d] > MPI_Recv complete : task %p %d %d\n",workgroup_tag,task.fp,task.task_id,task.task_status);
				}
				else if( status.MPI_TAG == TF_DIETAG ){
					//printf("WORKGROUP [%d] > MP_Recv DIETAG complete\n",workgroup_tag);
					fprintf(ioworkgroup,"********************************************************************\n");
					fprintf(ioworkgroup,"WORKGROUP [%d] > MPI_Recv complete : task %p %d %d\n",workgroup_tag,task.fp,task.task_id,task.task_status);
				}
			}

			// workgroup interanl bcast - task
			if( n == workgroup_tag ){

				MPI_Bcast(&task,sizeof(function_task),MPI_CHAR,0,*workgroup_comm);

				if( worker_rank == 0 ){
					//printf("WORKGROUP [%d] > MPI_Bcast complete : worker_rank [%d] task %p %d %d\n",workgroup_tag,worker_rank,task.fp,task.task_id,task.task_status);
					fprintf(ioworkgroup,"WORKGROUP [%d] > MPI_Bcast complete : worker_rank [%d] task %p %d %d\n",workgroup_tag,worker_rank,task.fp,task.task_id,task.task_status);
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
					system(task.syscmd);			// copy *.gin
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
				task.fp(workgroup_comm,task.task_iopath,&task.task_id,&task.worker_id);

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

				// setup result;
				res.task_status = TASK_FINISHED;
				res.task_id = task.task_id;
				res.worker_id = task.worker_id;

				// send back to Master
				if( worker_rank == 0 ){
					MPI_Send(&res,sizeof(result_package),MPI_CHAR,master_base_rank,res.task_status,*base_comm);
					fprintf(ioworkgroup,"WORKGROUP [%d] > MPI_Send complete <result callback to master> : task %p %d %d\n",workgroup_tag,task.fp,task.task_id,task.task_status);
				}
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
