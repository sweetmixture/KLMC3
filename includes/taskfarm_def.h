/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25

        Description:
*/

#ifndef __TASKFARM_DEF
#define __TASKFARM_DEF

#define TF_WORKTAG 777
#define TF_DIETAG  666
#define TF_CPUS_PER_WORKER 16

#define TF_TRUE   1
#define TF_FALSE  0

// file path
#define TF_CONFIG_FILE "taskfarm.config"

#include <mpi.h>

typedef struct TaskFarmConfiguration_{

	int num_tasks;
	int task_start;
	int task_end;
	int cpus_per_workgroup;

}TaskFarmConfiguration;

typedef struct WorkgroupConfig_{

	int base_size;		// size: of the base workgroup
	int base_rank;		// base_comm rank of the head (rank) of the workgroup ?
	int workgroup_tag;	// tag : of *this workgroup
	int workgroup_size;	// size: of *this workgroup
	int worker_rank;	// rank: of *this workgroup cpu core

}WorkgroupConfig;
// 

int tf_get_taskfarm_configuration( TaskFarmConfiguration* tfc );

int tf_config_workgroup( MPI_Comm* base_comm, MPI_Comm* workgroup_comm, int* workgroup_tag, int* workgroup_size, int* worker_rank, const int n_workers_per_workgroup );

void tf_get_workgroup_config( const MPI_Comm* base_comm, const MPI_Comm* workgroup_comm, WorkgroupConfig* workgroup_config, const int n_workgroup, const int workgroup_tag );


// general function pointer get library(?) function



#endif
