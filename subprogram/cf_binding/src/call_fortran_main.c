#include <stdio.h>
#include <mpi.h>

int main()
{
	int size,rank;
	MPI_Comm base_comm;

	base_comm = MPI_COMM_WORLD;
	MPI_Init(NULL,NULL);
	MPI_Comm_size(base_comm,&size);
	MPI_Comm_rank(base_comm,&rank);

	// Call Fortran subroutine

	//


	MPI_Finalize();

	return 0;
}
