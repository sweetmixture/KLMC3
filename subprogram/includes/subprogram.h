#ifndef _SUBPROGRAM
#define _SUBPROGRAM

#include <mpi.h>

void subprogram_pi( const MPI_Comm* comm, const int pid );


/* FORTRAN */

extern void fortran_subprogram_pi( const MPI_Comm* , int* );
/* subroutine fortran_subprogram_pi( comm, task_id ) bind(C,name="fortran_subprogram_pi")
 * integer(c_int), intent(inout) :: comm, task_id
 */

extern void gulpklmc( const MPI_Comm*, char*, int*, int* );
/* subroutine gulpklmc( MPI_comm_klmc, klmc_task_iopath, klmc_task_id, klmc_worker_id ) bind (C,name="gulpklmc")
 * integer,             intent(in) :: MPI_Comm
 * characeter(len=256), intent(in) :: klmc_task_iopath
 * integer,             intent(in) :: klmc_task_id
 * integer,             intent(in) :: klmc_worker_id
 */


#endif
