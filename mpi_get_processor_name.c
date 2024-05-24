#include <stdio.h>
#include <mpi.h>
#include <unistd.h>

int main(int argc, char* argv[]) {
  MPI_Init(&argc, &argv);

  int nsize = 0, myrank = 0, name_len = 0;
  char processor_name[MPI_MAX_PROCESSOR_NAME];

  MPI_Comm_size(MPI_COMM_WORLD, &nsize);
  MPI_Comm_rank(MPI_COMM_WORLD, &myrank);

  int num_cpus_per_node;
  num_cpus_per_node = sysconf(_SC_NPROCESSORS_ONLN);
  fprintf(stdout,"%d\n",num_cpus_per_node);

  MPI_Get_processor_name(processor_name, &name_len);
  fprintf(stdout, "%s : This is %d of %d.\n", processor_name, myrank, nsize);
  fflush(stdout);

  MPI_Finalize();
  return 0;
}
