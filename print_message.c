/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/

#include <stdio.h>
#include "taskfarm_def.h"

void mprint_stderr(
	const TaskFarmConfiguration* tfc,
	const char* msg
)
{
	if( tfc->brank == tfc->mrank ){
		fprintf(stderr,"%s\n",msg);
	}

	return;
}
void wprint_stderr(
	const TaskFarmConfiguration* tfc,
	const char* msg
)
{
	if( tfc->worker_rank == 0 ){
		fprintf(stderr,"%s\n",msg);
	}
	return;
}
