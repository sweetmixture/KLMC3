/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25

        Description:
*/

#ifndef __TASKFARM_DEF
#define __TASKFARM_DEF

// MPI_TAGS: TASK_ : task related
#define TASK_INIT 55
#define TASK_EXECUTED 56
#define TASK_FINISHED 57
#define TASK_DIETAG 666
#define TASK_WORKTAG 777

// file path: TF_ : taskfarm related
#define TF_CONFIG_FILE "taskfarm.config"		// taskfarm configuration file
#define TF_MAIN_FILE   "taskfarm.log"

#include <mpi.h>
#include <stdbool.h>

typedef struct TaskFarmConfiguration_{
	
	/* * *

		mpi related

	* * */
	int bsize;		// global : base rank size
	int brank;		// local  : my rank
	int mrank;		// global : master rank

	/* * *

		taskfarm configuration related

		global	: same for all CPUs
		local	: differ by a chosen CPU

	* * */
	int n_workgroup;		// global : workgroup number = worker-groups(n) + master(1)
	int workgroup_tag;		// local  : my workgroup tag	 : i.e., group tag  I belong to
	int workgroup_size;		// local  : my workgroup size	 : i.e., group size I belong to
	int worker_rank;		// local  : my rank in workgroup : i.e., my rank in the workgroup I belong to

	/* * * 

		task related

	* * */
	char application[64];

	int num_tasks;
	int task_start;
	int task_end;
	int cpus_per_workgroup;

}TaskFarmConfiguration;

typedef struct WorkgroupConfig_{

	/* * *

		Description:

			C struct which saves taskfarm CPU configuration globally. Note taht 'TaskFarmConfiguration' type is local !
			The term 'globally' implies, each CPU will have the same copy of it.

			This struct is used as array in 'main' (taskfarm.c).

			* The length of array is equal to the number of workgroups (i.e., tfc.n_workgroup)

			EXAMPLE>

				WorkgroupConfig[n] : contains information of 'n'th workgroup

					base_size<int>		: BaseComm size
					base_rank<int>		: BaseComm rank -> this is 'PARTICULARLY USEFUL' when setting communication: 'master' <-> 'workergroup'
					workgroup_tag<int>  : workgroup tag of 'n'th workgroup : (simply) = n
					workgroup_size<int> : size (#CPUs)  of 'n'th workgroup

	* * */

	int base_size;			// global
	int base_rank;			// global
	int workgroup_tag;		// global
	int workgroup_size;		// global

}WorkgroupConfig;


/* * *
	functions
* * */

bool tf_get_taskfarm_configuration(
	TaskFarmConfiguration* tfc		// IN-OUT
);

bool tf_config_workgroup(
    const MPI_Comm* base_comm,		// IN
    MPI_Comm* workgroup_comm,		// IN-OUT
    TaskFarmConfiguration* tfc		// IN-OUT
);

bool tf_get_workgroup_config(
	const MPI_Comm* base_comm,				// IN
	const MPI_Comm* workgroup_comm,			// IN
	const TaskFarmConfiguration* tfc, 		// IN
	WorkgroupConfig* workgroup_config		// IN-OUT
);

// general function pointer get library(?) function

#endif
