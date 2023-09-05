/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/


#ifndef __TASKFARM_MASTER_WORKER
#define __TASKFARM_MASTER_WORKER

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
	int task_status;
	int worker_id;

}TaskEnvelope;

/* TASK RESULT ENVELOPE */
typedef struct TaskResultEnvelope_{

	int task_id;					// which task this was?
	int worker_id;					// task done by which 'workder (or workgroup)'
	int task_status;				// task_status

	// * ws [40]
	char start_t[64];				// task_start time (DateTime)
	char end_t[64];					// task_end   time (DateTime)

	double elapsed_t;
	double value;

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

bool master_worker_task_call_master(
    const MPI_Comm* base_comm,
    const TaskFarmConfiguration* tfc,
    const WorkgroupConfig* wc
);

void master_worker_task_call_workgroup(
	const MPI_Comm* base_comm,
	const MPI_Comm* workgroup_comm,
	const int n_workgroup,
	const int workgroup_tag
);


#endif
