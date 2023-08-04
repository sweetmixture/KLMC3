/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/


#ifndef MASTER_WORKER_TASK
#define MASTER_WORKER_TASK

#include <mpi.h>

#define TASK_INIT     55
#define TASK_EXECUTED 56
#define TASK_FINISHED 57
#define TASK_DIETAG  666

typedef void (*taskfunction_ptr)( const MPI_Comm* , int* );

typedef void (*gulp_taskfunction_ptr)( const MPI_Comm* , char*, int*, int* );

typedef struct function_task_{

	// taskfunction_ptr fp;
	taskfunction_ptr tfp;
	gulp_taskfunction_ptr fp;

	int task_id;
	int worker_id;
	char task_iopath[512];
	char task_rootpath[512];

	// task info
	char syscmd[1024];				// command to copy *.gin file to working directory: task_iopath

	int task_status;

}function_task;

typedef struct result_package_{

	int task_id;
	int worker_id;
	int task_status;

	char start_t[40];
	char end_t[40];

	double elapsed_t;
	double value;

}result_package;


//void master_worker_task_call_master( const MPI_Comm* base_comm, const WorkgroupConfig* wc, const int n_workgroup, const int task_count );
void master_worker_task_call_master( const MPI_Comm* base_comm, const WorkgroupConfig* wc, const int n_workgroup, const int task_start, const int task_end );

void master_worker_task_call_workgroup( const MPI_Comm* base_comm, const MPI_Comm* workgroup_comm, const int n_workgroup, const int workgroup_tag );


#endif
