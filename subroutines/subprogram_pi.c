/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <mpi.h>

#include <sys/time.h>
#include <time.h>

#include "timer.h"


/* in timer.h

double get_time() {
	struct timeval tv;
	gettimeofday(&tv, NULL);
	return (double)tv.tv_sec + (double)tv.tv_usec / 1000000.0;
}

void getCurrentDateTime(char *dateTimeStr) {
	struct timeval tv;
	gettimeofday(&tv, NULL);

	time_t seconds = tv.tv_sec;
	long int microseconds = tv.tv_usec;

	struct tm* tm = localtime(&seconds);

	int year = tm->tm_year + 1900;
	int month = tm->tm_mon + 1;
	int day = tm->tm_mday;
	int hour = tm->tm_hour;
	int minute = tm->tm_min;
	int second = tm->tm_sec;

	sprintf(dateTimeStr, "%04d-%02d-%02d %02d:%02d:%02d.%06ld", year, month, day, hour, minute, second, microseconds);
}

*/

void subprogram_pi( const MPI_Comm* comm, const int task_id )
{
	int rank, size, i, N, N_local, N_inside = 0, N_total_inside;
	double x, y, pi_local, pi_total, t_start, t_end;
	char start_timestamp[40], end_timestamp[40];

	FILE* fileptr = NULL;
	char file_name[20];

	MPI_Comm_size(*comm, &size);
	MPI_Comm_rank(*comm, &rank);

	const int iter_max = 16;
	int iter;
	double* data = (double*)malloc(iter_max*sizeof(double));

	N = 100000000;  // Number of points to generate
	//N = 999999999;  // Number of points to generate
	N_local = N / size;  // Number of points to generate per process
	//Seed the random number generator based on the rank
	srand(rank + task_id);
	
	t_start = get_time();
	getCurrentDateTime(start_timestamp);

	sprintf(file_name,"task_id_%d",task_id);

	if( rank == 0 ){
		fileptr = fopen(file_name,"w");
	}

	if( rank == 0 ){
		//fprintf(fileptr,"task_id: %d ntasks: %d rank: %d start: %s \n",task_id, size,rank,start_timestamp);
	}

	for(iter = 0; iter < iter_max; iter++ ){

		//if( rank == 0 ) printf("iter: %d\n",iter);

		// Generate N_local random points and count how many fall inside the circle
		for (i = 0; i < N_local; i++) {

			//if( i % 10000000 == 0 ){
			//	if( rank == 0 ) printf("inner i: %d  ntasks: %d\n",i,size); }

			x = (double)rand() / RAND_MAX;
			y = (double)rand() / RAND_MAX;

			if (sqrt(x*x + y*y) <= 1.0) {
				N_inside++;
			}
		}

		// Sum up the number of points inside the circle across all processes
		MPI_Reduce(&N_inside, &N_total_inside, 1, MPI_INT, MPI_SUM, 0, *comm);

		// Calculate the local estimate of pi and sum it up across all processes
		pi_local = 4.0 * (double)N_inside / (double)N_local;
		MPI_Reduce(&pi_local, &pi_total, 1, MPI_DOUBLE, MPI_SUM, 0, *comm);

		data[iter] = pi_total;
		N_inside = 0;
	}

	double res = 0.;
	for(iter = 0; iter < iter_max ; iter++){
		res += data[iter];
	}
	pi_total = res / (double)iter_max;

	t_end = get_time();
	getCurrentDateTime(end_timestamp);

	MPI_Comm_size(*comm,&size);

	if( rank == 0){
		fprintf(fileptr,"{'task_id': %d, 'ntasks': %d, 'timing': {'start': '%s', 'end': '%s', 'elapsed_t': %.6lf }, ",task_id,size,start_timestamp,end_timestamp,t_end - t_start);
	}

	// Output the final estimate of pi from the root process
	if (rank == 0) {
		pi_total /= size;
		fprintf(fileptr," 'result' : %.8lf }\n",pi_total);
	}
	if( rank == 0 ){ fclose(fileptr); }
	free(data);

	MPI_Barrier(*comm);

	return;
}
