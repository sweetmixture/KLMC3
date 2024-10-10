/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/


#ifndef __MASTER_WORKER_READY_INPUT
#define __MASTER_WORKER_READY_INPUT

#define _LOGFILE_MASTER_ "master.log"

#include <mpi.h>			// using MPI related
#include "taskfarm_def.h"	// using WorkgroupConfig

typedef void (*taskfunction_ptr)(
	const MPI_Comm* ,
	int*
);

typedef void (*app_execute_ptr)(
	const MPI_Comm* ,
	char*,
	int*,
	int*
);

/* TASK ENVELOPE */
typedef struct TaskEnvelope_{

	/*
		application
	*/
	char application[32];
	app_execute_ptr app_ptr;		// application function(or subroutine) function pointer

	/*
		task related
	*/
	int  task_id;
	char task_iopath[512];			// application launch directory
	char task_rootpath[512];		// taskfarm root directory

	/*
		prep commands for launcing 'app_ptr'
	*/
	int  cmd_count;					// number of copy commands for inputfiles
	char cmd[16][1024];				// copy commands
	int  inputfile_count;
	char inputfile_path[16][512];	// in order to check if file exists

	/*
		task status related : will be used later by 'worker'
	*/
	int task_status;	// tell workgroup what to do ? 11.09.23 wkjee - only used when 'workgroup' reads in task type (if this is a task or die-tag)
	int workgroup_tag;	// will be used as a parameter when launching application

}TaskEnvelope;

/* TASK RESULT ENVELOPE */
typedef struct TaskResultEnvelope_{

	int task_status;				// task_status ?
	int task_id;					// which task this was ?
	int workgroup_tag;				// task done by which workgroup ?
	/*
		time
	*/
	char start_t[64];				// task_start time (DateTime)
	char end_t[64];					// task_end   time (DateTime)
	double elapsed_t;
	/*
		error handling
	*/
	bool inputfile_check;

}TaskResultEnvelope;


/* * * * *
 * strcuts for convenient management
 * * * * */

typedef struct MasterWorkspace_{

	/*
		Parameters
	*/
	char root[512];						// Parameter : TaskFarm Root
	char inputsource_dir[512];			// Parameter : InputSource Directory
	
	/*
		Variables ( workspace )
	*/
	char rundir[64];
	char rundir_path[512];

	int  inputfile_count;
	char inputfile[8][64];
	char inputfile_path[8][512];

}MasterWorkspace;

/* * * * *
 * functions
 * * * * */

//bool master_worker_task_call_master(
bool ready_input_call_master(
    const MPI_Comm* base_comm,
    const TaskFarmConfiguration* tfc,
    const WorkgroupConfig* wc
);

//void master_worker_task_call_workgroup(
void ready_input_call_workgroups(
	const MPI_Comm* base_comm,
	const MPI_Comm* workgroup_comm,
	const int n_workgroup,
	const int workgroup_tag
);


#endif
