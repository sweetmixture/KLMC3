/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:


        * * * LOG

            (1) bool tf_get_taskfarm_configuration()    : read input file
            (2) int tf_config_workgroup()               : mpi communicator split
            (3) void tf_get_workgroup_config()          : after communicator split reset tags ...

            08/23: Refactoring * * *

*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <mpi.h>

#include "taskfarm_def.h"
#include "read_input.h"

#include "error.h"

/* * *
	only use in this source
* * */





/* * *

    Description:

    TaskFarm Configuration

* * */
bool tf_get_taskfarm_configuration(
	const MPI_Comm* base_comm,
	TaskFarmConfiguration* tfc			// IN-OUT
)
{
	int brank, bsize;
	bool berr;
    FILE* fp = NULL;

	MPI_Comm_size(*base_comm,&bsize);
	MPI_Comm_rank(*base_comm,&brank);

	/* * *
	 * Set up MPI related variables
	 * * */
	tfc->bsize = bsize;
	tfc->brank = brank;
	tfc->mrank = bsize - 1;
	// Set up end;

    fp = fopen(TF_CONFIG_FILE,"r");		// READ-IN INITIAL TASKFARM CONFIGURATION 

    if( fp == NULL ){
		/* * *
		 * Error <finder> : configuratio file not found
		 * * */
		berr = error_taskfarm_filenotfound(base_comm);				
        return berr;
    }
    else{

        /* * *
		 * Init taskfarm-configuration
		 * * */
        strcpy(tfc->application, "-1");
        tfc->task_start         = -1;
        tfc->task_end           = -1;
        tfc->cpus_per_workgroup = -1;
        // Init end;

        // 'string' : InputFile Read
        read_input_spatternfinder(fp, "application"    , &(tfc->application[0])     );

        // 'integer': InputFile Read
        read_input_ipatternfinder(fp, "task_start"     , &(tfc->task_start)         );
        read_input_ipatternfinder(fp, "task_end"       , &(tfc->task_end)           );
        read_input_ipatternfinder(fp, "cpus_per_worker", &(tfc->cpus_per_workgroup) );
		/* * * Modification required 12.09.23 : wkjee
			Mode pattern reader
			(1) simply using pre-prepared input files
		* * */


#ifdef USE_PYTHON
/* --------------------------------------------
	08.11.2023
	If Python used
   -------------------------------------------- */
		if( strcmp(tfc->application,"python") == 0 ){
			/* if python is used as application: (1) read python module path / (2) python method/function name */
			read_input_spatternfinder(fp,"python_module_path", &(tfc->python_module_path[0]));
			read_input_spatternfinder(fp,"python_module_name", &(tfc->python_module_name[0]));
			read_input_spatternfinder(fp,"python_method_name", &(tfc->python_method_name[0]));
			// relevant error handling required ?
/*
	! in taskfarm.config file: possible style of input will be ...

	python_module_path /work/e05/e05/wkjee/Software/gulpklmc/CPython
	python_method_name test_module
	python_module_name random_gen
*/
		}
#endif

        fclose(fp);
/* * *
	Final Error Handler
* * */
		/* * *
		 * Error <finder> : possible error in configuration file
		 * * */
		berr = error_taskfarm_configuration(base_comm,tfc);

		return berr;
    }
}

/*
 *
 *  Spliting MPI_Communicator: last cpu core is always dedicated to the 'master'
 *
 */
bool tf_config_workgroup( 
    const MPI_Comm* base_comm,              // IN
    MPI_Comm* workgroup_comm,               // IN-OUT
    TaskFarmConfiguration* tfc              // IN-OUT
)
{
    bool berr = true;
    
	// temporal variables (mpi)
    int brank,bsize; 

	// temporal variables
    int n_workgroup;
    int workgroup_tag;
    int workgroup_size;
    int worker_rank;

    MPI_Comm_size(*base_comm,&bsize);
    MPI_Comm_rank(*base_comm,&brank);
	MPI_Get_processor_name(&(tfc->proc_name[0]),&(tfc->proc_name_len));
	
	/* * *
		OMP_NUM_THREADS	check -> warning
		nproc			check -> error (if avaiable cpus count = 1 : cannot configure workgroups)
		nproc balance	check -> warning
	* * */
	berr = error_taskfarm_commsplit_nproc_check(base_comm,tfc);
	if( !berr ){
		return berr;
	}

    /* * *
        MPI_COMM SPLIT START  
    * * */
    workgroup_tag =  brank / tfc->cpus_per_workgroup;	// Initial set: workgroup-tag for each CPU

    /*
        rank                    => mpi_rank
		tfc->cpus_per_workgroup => number of cpus allocated per workgroup

        EXAMPLE>

        CPU.ID  |   rank  / tfc->cpus_per_workgroup         |   tag
            0   |       0 / 2                               |    0
            1   |       1 / 2                               |    0
            2   |       2 / 2                               |    1
            3   |       3 / 2                               |    1
            4   |       4 / 2                               |    2
            .   |       .                                   |    .
            .   |       .                                   |    .
            9   |       9 / 2                               |    4
           10   |      10 / 2                               |    5
    */    

    /* * *
        calculate: number of workgroups for a given number of CPUs
    * * */
    if( bsize % tfc->cpus_per_workgroup == 0 ){
        n_workgroup = bsize / tfc->cpus_per_workgroup;

		// 11.2023 bugfix: case cpus_per_workgroup == 1
		if( tfc->cpus_per_workgroup > 1 ){
        	n_workgroup++;
		}

        /* case - 1: no residual cpus to be the 'master'
            last cpu in last workgroup -> 'master'          */
    }
    else if( bsize % tfc->cpus_per_workgroup == 1 ){                        
        n_workgroup = bsize / tfc->cpus_per_workgroup;
        n_workgroup++;
        /* case - 2: 1 residual cpu in the last workgroup
            last workgroup -> 'master'                      */
    }
    else{                                                                   
        n_workgroup = bsize / tfc->cpus_per_workgroup + 1; // '+1' means additional workgroup formed by the residual CPUs
        n_workgroup++;
        /* case - 3: >1 residual cpus in the last workgroup
            split last workgroup (workgroup_size-1 / 1) and the last workgroup -> 'master'  */
    }

    /* * *
		final set: workgroup tag
	* * */
    if( ((brank + 1) == bsize) && ((bsize % tfc->cpus_per_workgroup) != 1) ){
        
		/* Explain:
        	((brank + 1) == bsize)							=> if this is 'last CPU'
         	((bsize % tfc->cpus_per_workgroup) != 1)		=> if this is 'last CPU' and sinlge CPU workgroup	*/

		// 11.2023 bugfix: case cpus_per_workgroup == 1
		if( tfc->cpus_per_workgroup > 1 ){
        	workgroup_tag = workgroup_tag + 1;  //			=> +1 to its workgroup_tag, in order to use as 'master'
		}

		// tmp -> this if{} will not work if cpus_per_workgroup == 1
    }

    /* * *
		split: base_comm (=MPI_COMM_WORLD) -> workgroup_comm

		note. processors with same 'tag' (workgroup_tag) carries same 'workgroup_comm'
	* * */
    MPI_Comm_split(*base_comm, workgroup_tag, brank, workgroup_comm);
    MPI_Comm_size(*workgroup_comm,&workgroup_size);
    MPI_Comm_rank(*workgroup_comm,&worker_rank);

    /* * *
		final set: taskframconfig parameters
	* * */
    tfc->n_workgroup = n_workgroup;             // global this include 'workgroup# + 1 (master)'
    tfc->workgroup_tag = workgroup_tag;         // local
    tfc->workgroup_size = workgroup_size;       // local
    tfc->worker_rank = worker_rank;             // local

    return berr;
}

/*
 *  For the split workgroup communicators: configuring the workgroups
 *  - setting tags for workgroup configurations
 *
 *		Trick to access specific processor (rank)
 *
 *			if( workgroup_tag == i && worker_rank == 0 ){
 *				wgc_global[i].base_rank // <WorkgroupConfig arr>
 */
bool tf_get_workgroup_config(
    const MPI_Comm* base_comm,          // IN
    const MPI_Comm* workgroup_comm,     // IN
    const TaskFarmConfiguration* tfc,   // IN
    WorkgroupConfig* wgc_global   // IN-OUT
)
{
    bool berr = true;

    const int base_root = 0;
    const int n_workgroup = tfc->n_workgroup;		// global	// Number of MPI_SubComms : master(1) + workgroups(n) -> 1+n
    const int workgroup_tag = tfc->workgroup_tag;	// local

    int brank,bsize;
    int workgroup_size;
    int worker_rank;

    MPI_Request request;
    MPI_Status status;

    WorkgroupConfig wgc_local;	// local workgroup configuration

    // * * *action starts

    MPI_Comm_size(*base_comm,&bsize);
    MPI_Comm_rank(*base_comm,&brank);
    
    MPI_Comm_size(*workgroup_comm,&workgroup_size);
    MPI_Comm_rank(*workgroup_comm,&worker_rank);

											// at this point!
    wgc_local.base_size = bsize;                  // global
    wgc_local.base_rank = brank;                  // local
    wgc_local.workgroup_tag = workgroup_tag;      // local
    wgc_local.workgroup_size = workgroup_size;    // local

	MPI_Barrier(*base_comm);

	/* * *
		Messaging : from each workgroup
	* * */
    for(int i=1;i<n_workgroup;i++){
        if( workgroup_tag == i && worker_rank == 0 ){
		/* 
			from the 'head_rank(0)' of 'workgroup(i)', send it's information :

				(1) base_size			-> DO REALLY NEED THIS?
				(2) base_rank
				(3) workgroup_tag
				(4) workgroup_size

			to 'base_root' (base_rank = 0 )

			note. message from 'workgroup(i)' is identified by the MPI_Send 'tag(i)'

			1. (3) and (4) could be useful
		*/
            MPI_Send(&wgc_local,sizeof(WorkgroupConfig),MPI_CHAR,base_root,i,*base_comm);
        }
    }

	/* * *
		Receiving : by base_root
	* * */
	for(int i=1;i<n_workgroup;i++){
		if( brank == base_root ){
			/*
				message (wgc_local) from 'head_rank(0)' of 'workgroup(i)' with identifier(i) <MPI_TAG>

				received to 'wgc_global[i]' by base_root (base_rank = 0)
			*/
			MPI_Recv(&wgc_global[i],sizeof(WorkgroupConfig),MPI_CHAR,MPI_ANY_SOURCE,i,*base_comm,&status);
        }
    }

    /* * *
        In-place copying (i=0) : wgc_global[0] <- wgc_local
    * * */
    if ( workgroup_tag == 0 && worker_rank == 0 ){
        wgc_global[0] = wgc_local;
    }
    MPI_Barrier(*base_comm);

    MPI_Bcast(&wgc_global[0],sizeof(WorkgroupConfig)*n_workgroup,MPI_CHAR,base_root,*base_comm);
	/*
		after MPI_Bcast, from any processors (ranks), they can see 'n'th workgroup information :

			(1) base_rank  of the group(n)'s head (sub_rank 0)
			(2) group_tag  of the group(n)
			(3) group_size of the group(n)
	*/

    return berr;
}
