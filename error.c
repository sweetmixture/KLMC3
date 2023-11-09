/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <mpi.h>

#include "taskfarm_def.h"
#include "print_message.h"	// for using ANSI_COLOR_* only

/* * *
 * taskfarm_def.c : bool tf_get_taskfarm_configuration()
 * * */

bool error_taskfarm_filenotfound(
	const MPI_Comm* base_comm
){
	bool bret = true;
	int brank;
	
	MPI_Comm_rank(*base_comm,&brank);

	print_stderr( brank, ANSI_COLOR_MAGENTA "Error> " ANSI_COLOR_RESET );
	print_stderr( brank, "It is likely that the taskfarm configuration file " ANSI_COLOR_CYAN "taskfarm.config" ANSI_COLOR_RESET " does not exist ...\n" );
	print_stderr( brank, "otherwise, in " ANSI_COLOR_RED __FILE__ " " ANSI_COLOR_RESET "function: " ANSI_COLOR_GREEN "bool " ANSI_COLOR_RESET "tf_get_taskfarm_configuration() -> debugging may be required\n" );

	bret = false;

	return bret;
}


bool error_taskfarm_configuration(
	const MPI_Comm* base_comm,
	TaskFarmConfiguration* tfc	// IN-OUT
){
	bool bret = true;
	bool bapp = false;
	
	int brank,bsize;
	
	MPI_Comm_rank(*base_comm,&brank);
	MPI_Comm_size(*base_comm,&bsize);

	/* application check: 'application' */
	if( strcmp(&(tfc->application[0]),TF_APPLICATION_GULP) == 0 ){
		bapp = true;
	}
	if( strcmp(&(tfc->application[0]),TF_APPLICATION_FHIAIMS) == 0 ){
		bapp = true;
	}
#ifdef USE_PYTHON
	/* --------------------------------------------
	    08.11.2023
	    If Python used
	   -------------------------------------------- */
	if( strcmp(&(tfc->application[0]),TF_APPLICATION_PYTHON) == 0 ){
		bapp = true;
	}
#endif
	if( !bapp ){ // if still false
		print_stderr( brank, ANSI_COLOR_MAGENTA "Error> " ANSI_COLOR_RESET );
		print_stderr( brank, ANSI_COLOR_RED "application " ANSI_COLOR_RESET "is not specified in file: " ANSI_COLOR_CYAN "taskfarm.config\n" ANSI_COLOR_RESET );
		print_stderr( brank, "possible options for " ANSI_COLOR_RED "application " ANSI_COLOR_RESET "are: (1) gulp and (2) fhiaims\n" );
		bret = false;
	}

	/* task id check: 'task_start', 'task_end', 'num_tasks' */
	if( tfc->task_start < 0 ){
		print_stderr( brank, ANSI_COLOR_MAGENTA "Error> " ANSI_COLOR_RESET );
		print_stderr( brank, "'task_start' is not specified or the given value is wrong, ");
		print_stderr( brank, "please correct " ANSI_COLOR_CYAN "taskfarm.config\n" ANSI_COLOR_RESET );
		bret = false;
	}
	if( tfc->task_end < 0 ){
		print_stderr( brank, ANSI_COLOR_MAGENTA "Error> " ANSI_COLOR_RESET );
		print_stderr( brank, "'task_end' is not specified or the given value is wrong, ");
		print_stderr( brank, "please correct " ANSI_COLOR_CYAN "taskfarm.config\n" ANSI_COLOR_RESET );
		bret = false;
	}

	if( (tfc->task_start >= 0) && (tfc->task_end >= 0) ){
		if( tfc->task_start >= tfc->task_end ){
			print_stderr( brank, ANSI_COLOR_MAGENTA "Error> " ANSI_COLOR_RESET );
			print_stderr( brank, "'task_start' can't be greater than (or equal to) 'task_end', ");
	 		print_stderr( brank, "please correct " ANSI_COLOR_CYAN "taskfarm.config\n" ANSI_COLOR_RESET );
			bret = false;
		}
		else{
			tfc->num_tasks = tfc->task_end - tfc->task_start + 1;
		}	
	}

	/* cpus per workgroup check: 'cpus_per_workgroup' */
	if( tfc->cpus_per_workgroup < 0 ){
		print_stderr( brank, ANSI_COLOR_MAGENTA "Error> " ANSI_COLOR_RESET );
		print_stderr( brank, "'cpus_per_worker' is not specified ");
		print_stderr( brank, "please correct " ANSI_COLOR_CYAN "taskfarm.config\n" ANSI_COLOR_RESET );
		bret = false;
	}
	else{
		if( tfc->cpus_per_workgroup > bsize ){
			char msg[512];
			print_stderr( brank, ANSI_COLOR_MAGENTA "Warning> " ANSI_COLOR_RESET );
			sprintf(msg,"Requested number of cpus, 'cpus_per_worker'(" ANSI_COLOR_RED "%d" ANSI_COLOR_RESET "), is larger than the current number of processors (" ANSI_COLOR_GREEN "%d" ANSI_COLOR_RESET ")\n",tfc->cpus_per_workgroup,bsize);
			print_stderr( brank, msg );
			print_stderr( brank, "In this case, " ANSI_COLOR_GREEN "master " ANSI_COLOR_RESET "processor - and may be only one " ANSI_COLOR_GREEN "worker " ANSI_COLOR_RESET"processor(s) - will be in action. If this is not intended, ");
			print_stderr( brank, "please correct " ANSI_COLOR_CYAN "taskfarm.config " ANSI_COLOR_RESET "or re-check if correct number of cpus is set in the " ANSI_COLOR_CYAN "jobscript" ANSI_COLOR_RESET ".\n" );
		}
	}
	return bret;
}



/* * *
 * taskfarm_def.c : bool tf_config_workgroup()
 * * */

bool error_taskfarm_commsplit_nproc_check(
	const MPI_Comm* base_comm,
	TaskFarmConfiguration* tfc  // IN-OUT
){
	bool bret = true;
	int brank,bsize;
	int omp_num_threads;
	char msg[512];
	char* omp_num_threads_env = getenv("OMP_NUM_THREADS");

	MPI_Comm_rank(*base_comm,&brank);
	MPI_Comm_size(*base_comm,&bsize);


	/* * *
	 * OMP_NUM_THREADS check
	 * * */
	if( omp_num_threads_env != NULL ){
		omp_num_threads = atoi(omp_num_threads_env);
		tfc->omp_num_threads_set = true;
		tfc->omp_num_threads = omp_num_threads;

		if( tfc->omp_num_threads != 1 ){

			print_stderr( brank, ANSI_COLOR_MAGENTA "Warning> " ANSI_COLOR_RESET );
			sprintf(msg, ANSI_COLOR_RED "OMP_NUM_THREADS " ANSI_COLOR_RESET " is not equal to 1. Performance could be reduced.\n");
			print_stderr( brank, msg );
			fflush_channel(NULL);
		}
	}
	else{
		omp_num_threads = -1;	// OMP_NUM_THREADS not set!
		tfc->omp_num_threads_set = false;

		print_stderr( brank, ANSI_COLOR_MAGENTA "Warning> " ANSI_COLOR_RESET );
		sprintf(msg, ANSI_COLOR_RED "OMP_NUM_THREADS " ANSI_COLOR_RESET "is not set. Performance could be reduced.\n");
		print_stderr( brank, msg );
		fflush_channel(NULL);
	}

	/* * *
	 * nproc check
	 * * */
	if( bsize == 1 ){
		print_stderr( brank, ANSI_COLOR_MAGENTA "Error> " ANSI_COLOR_RESET );
		sprintf(msg,"Current number of processors is " ANSI_COLOR_RED "1 " ANSI_COLOR_RESET "(allocated to " ANSI_COLOR_GREEN "master" ANSI_COLOR_RESET "). There are not enough resource to configure " ANSI_COLOR_GREEN "worker " ANSI_COLOR_RESET "processor(s).\n");
		print_stderr( brank, msg );
		fflush_channel(NULL);

		bret = false;
		return bret;
	}

	/* * *
	 * cpu balence check
	 * * */
	if( bsize % tfc->cpus_per_workgroup != 0 ){
	
		print_stderr( brank, ANSI_COLOR_MAGENTA "Warning> " ANSI_COLOR_RESET );
		sprintf(msg,"Number of available processors(" ANSI_COLOR_RED "%d" ANSI_COLOR_RESET ") can't be divided by the requested cpus_per_worker("ANSI_COLOR_GREEN"%d"ANSI_COLOR_RESET").\n",bsize,tfc->cpus_per_workgroup);
		print_stderr( brank, msg );
		print_stderr( brank, "If mutiple nodes are requested, some of processors from different nodes will be allocated to one of the workgroups and it's performance will be reduced significantly. If this is not intended ");
		print_stderr( brank, "please correct " ANSI_COLOR_CYAN "taskfarm.config " ANSI_COLOR_RESET ".\n" );
		fflush_channel(NULL);
	}

	return bret;
}


/* * * * *
 * utilities
 * * * * */

bool error_file_exists(
	const char* filename
){
	if( access(filename,F_OK) == 0 ){
		return true;
	}
	else{
		return false;
	}	
}



