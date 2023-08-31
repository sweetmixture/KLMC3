/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/

#ifndef __TASKFARM_ERROR
#define __TAKSFARM_ERROR


/* * *
 * taskfarm_def.c
 * * */

#include <mpi.h>
#include "taskfarm_def.h"

bool error_taskfarm_filenotfound(
    const MPI_Comm* base_comm
);

bool error_taskfarm_configuration( 
    const MPI_Comm* base_comm,
    TaskFarmConfiguration* tfc	// IN-OUT
);

bool error_taskfarm_commsplit_nproc_check(
    const MPI_Comm* base_comm,
	TaskFarmConfiguration* tfc  // IN-OUT
);


/* * *
 * utilities
 * * */

bool error_file_exists( 
    const char* filename 
);


#endif
