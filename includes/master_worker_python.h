/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:

			launching fully independent Python method/function
*/

#ifndef __MASTER_WORKER_PYTHON
#define __MASTER_WORKER_PYTHON

#define _LOGFILE_MASTER_ "master.log"

#include <mpi.h>			// using MPI related
#include "taskfarm_def.h"	// using WorkgroupConfig

#include "python_interface.h"


/* TASK ENVELOPE - PYTHON */
typedef struct TaskEnvelopePython_{

	/*
		application
	*/
	char application[32];
	//app_execute_ptr app_ptr;		// application function(or subroutine) function pointer

	/*
		Python related
	*/
	char python_method[512];

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

}TaskEnvelopePython;

/* TASK RESULT ENVELOPE */
typedef struct TaskResultEnvelopePython_{

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

}TaskResultEnvelopePython;


/* * * * *
 * strcuts for convenient management
 * * * * */

typedef struct MasterWorkspacePython_{

	/*
		Parameters
	*/
	char root[512];						// Parameter : TaskFarm Root
	char inputsource_dir[512];			// Parameter : InputSource Directory
	
	/*
		Python related
	*/
	char method_name[512];				// Python method/function name

	/*
		Variables ( workspace )
	*/
	char rundir[64];
	char rundir_path[512];

	int  inputfile_count;
	char inputfile[8][64];
	char inputfile_path[8][512];

}MasterWorkspacePython;

/* * * * *
 * functions
 * * * * */

bool ready_input_call_master_python(
    const MPI_Comm* base_comm,
    const TaskFarmConfiguration* tfc,
    const WorkgroupConfig* wc
);

void ready_input_call_workgroups_python(
	const MPI_Comm* base_comm,				// global
	const MPI_Comm* workgroup_comm,			// local
	const int n_workgroup,					// global
	const int workgroup_tag,				// local
	const TaskFarmConfiguration* tfc		// global
);


#endif
