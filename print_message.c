/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/

#include <stdio.h>				// using 'FILE'
#include "taskfarm_def.h"		// using 'TaskFarmConfiguration'

void print_stdout(
	const int mpi_rank,
	const char* msg
){
	if( mpi_rank == 0 ){
		fprintf(stdout,"%s",msg);
	}
}

void print_stderr(
    const int mpi_rank,
    const char* msg
){
    if( mpi_rank == 0 ){
        fprintf(stderr,"%s",msg);
    }
}

void fflush_channel( FILE* channel ){
	if( channel == NULL ){
		fflush(stdout);
		fflush(stderr);
	}
	else{
		fflush(channel);
	}
}

// DEV
void master_print_message(
	const TaskFarmConfiguration* tfc,
	FILE* channel,
	const char* msg
){
	if( tfc->brank == tfc->mrank ){
		if( channel == NULL ){
			fprintf(stderr,"%s",msg);
		}
		else{
			fprintf(channel,"%s",msg);
		}
	}
}

void worker_print_message(
	const TaskFarmConfiguration* tfc,
	FILE* channel,
	const char* msg
){
	if( tfc->worker_rank == 0 ){
		if( channel == NULL ){
			fprintf(stderr,"%s",msg);
		}
		else{
			fprintf(channel,"%s",msg);
		}
	}
}


/*
 *
 * Output decorators: uses in taskfarm.c
 *
 */


void layout_line_stdout(
	const int mpi_rank,
	const char c,
	const int n
){
	if( mpi_rank == 0 ){
		for(int i=0;i<n;i++){
			fprintf(stdout,"%c",c);
		}
		fprintf(stdout,"\n");
	}
}
