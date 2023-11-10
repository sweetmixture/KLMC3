/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25

        Description:
*/

#ifndef __TASKFARM_DEF
#define __TASKFARM_DEF

// MPI_TAGS: TASK_ : task related
#define TASK_INIT 		55
#define TASK_EXECUTED 	56
#define TASK_FINISHED 	57
#define TASK_DIETAG 	666
#define TASK_WORKTAG 	777

// file path: TF_ : taskfarm related
#define TF_CONFIG_FILE 			"taskfarm.config"		// taskfarm configuration file
#define TF_MAIN_FILE   			"taskfarm.log"
#define TF_APPLICATION_GULP    	"gulp"
#define TF_APPLICATION_FHIAIMS 	"fhiaims"

#ifdef USE_PYTHON
/* --------------------------------------------
    08.11.2023
    If Python used
   -------------------------------------------- */
#define TF_APPLICATION_PYTHON "python"
#endif

#include <mpi.h>
#include <stdbool.h>

typedef struct TaskFarmConfiguration_{
	
	/* * *

		mpi related

	* * */
	int bsize;								// global : base rank size
	int brank;								// local  : my rank
	int mrank;								// global : master rank

	char proc_name[MPI_MAX_PROCESSOR_NAME];	// local : my process id
	int proc_name_len;						// local : my process id length

	// sys info
	bool omp_num_threads_set;				// global
	int omp_num_threads;					// global

	/* * *

		taskfarm configuration related

		global	: same for all CPUs
		local	: differ by a chosen CPU

	* * */
	int n_workgroup;						// global : workgroup number = worker-groups(n) + master(1)
	int workgroup_tag;						// local  : my workgroup tag	 : i.e., group tag  I belong to
	int workgroup_size;						// local  : my workgroup size	 : i.e., group size I belong to
	int worker_rank;						// local  : my rank in workgroup : i.e., my rank in the workgroup I belong to

	/* * * 

		task related

	* * */
	char application[64];					// global

	int num_tasks;							// global
	int task_start;							// global
	int task_end;							// global
	int cpus_per_workgroup;					// global

#ifdef USE_PYTHON
	/* --------------------------------------------
	    08.11.2023
	    If Python used
		
		the following variables for loading a single
		python method/function from a module
	   -------------------------------------------- */
	char python_module_path[512];			// global:	module path (location of module.py)
	char python_module_name[512];			// global:	name of the 'module' make sure it should not take the extention *.py
	char python_method_name[512];			// global:	name of the method/function within the 'module'
#endif

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
					base_rank<int>		: BaseComm rank -> this is 'PARTICULARLY USEFUL' when setting communications: 'master' <-> 'workergroup'
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
	const MPI_Comm* base_comm,		// IN
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
	WorkgroupConfig* wgc_global				// IN-OUT
);

// general function pointer get library(?) function

#endif
