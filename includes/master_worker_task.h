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

typedef void (*gulp_taskfunction_ptr)(
	const MPI_Comm* ,
	char*,
	int*,
	int*
);

/* TASK PACKAGE */
typedef struct function_task_{

	// taskfunction_ptr fp;
	taskfunction_ptr tfp;
	gulp_taskfunction_ptr fp;

	int task_id;
	int worker_id;
	char task_iopath[512];			// application launch directory
	char task_rootpath[512];		// taskfarm root directory

	// task info
	char syscmd[1024];				// command to copy *.gin file to working directory: task_iopath

	int system_cmd_cnt;				// number of valid system commands
	char system_cmd[8][1024];		// system command buffer

	int task_status;

}function_task;

/* RESULT PACKAGE */
typedef struct result_package_{

	int task_id;
	int worker_id;
	int task_status;

	char start_t[40];
	char end_t[40];

	double elapsed_t;
	double value;

}result_package;


/* * * * *
 * functions
 * * * * */

void master_worker_task_call_master(
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
