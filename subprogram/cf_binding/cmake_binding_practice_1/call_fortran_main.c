#include <stdio.h>
#include <mpi.h>

extern void f_foo( MPI_Comm* , int* );
//extern void f_foo( MPI_Comm* , int );

int main()
{
	int size,rank;
	int task_id = 15;
	MPI_Comm base_comm;

	base_comm = MPI_COMM_WORLD;
	MPI_Init(NULL,NULL);
	MPI_Comm_size(base_comm,&size);
	MPI_Comm_rank(base_comm,&rank);

	// Call Fortran subroutine
	f_foo(&base_comm,&task_id);
	//f_foo(&base_comm,task_id);
	//

	if( rank == 0 ){
		printf("C> task_id: %d\n",task_id);
	}

	MPI_Finalize();

	return 0;
}
