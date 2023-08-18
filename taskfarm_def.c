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

//#include "print_message.h"	// addition 18.08.23

/* * *
	only use in this source
* * */
bool tf_configuration_check(
	const TaskFarmConfiguration* tfc	// IN
);


/* * *

    Description:

    TaskFarm Configuration

* * */
bool tf_get_taskfarm_configuration(
	TaskFarmConfiguration* tfc			// IN-OUT
)
{
	bool if_success = false;

    FILE* fp = NULL;
    bool bntasks          = false;
    bool btask_start      = false;
    bool btask_end        = false;
    bool bcpus_per_worker = false;
    bool bapp_name        = false;

    // Operation Start
    fp = fopen(TF_CONFIG_FILE,"r");

    if( fp == NULL ){

        // Erro message

        // Assertion

        return if_success;
    }
    else{

        // Init taskfarm-configuration
        strcpy(tfc->application," no application specified ");

        tfc->num_tasks = -1;
        tfc->task_start = -1;
        tfc->task_end = -1;
        tfc->cpus_per_workgroup = -1;

        // Init end;

        // 'string' : InputFile Read
        bapp_name        = read_input_spatternfinder( fp, "application"    , &(tfc->application[0])     );

        // 'integer': InputFile Read
        bntasks          = read_input_ipatternfinder( fp, "ntasks"         , &(tfc->num_tasks)          );
        btask_start      = read_input_ipatternfinder( fp, "task_start"     , &(tfc->task_start)         );
        btask_end        = read_input_ipatternfinder( fp, "task_end"       , &(tfc->task_end)           );
        bcpus_per_worker = read_input_ipatternfinder( fp, "cpus_per_worker", &(tfc->cpus_per_workgroup) );


        // devtmp
        //fprintf(stdout," bapp_name        : %s\n", bapp_name        ? "true":"false");

        //fprintf(stdout," bntasks          : %s\n", bntasks          ? "true":"false");
        //fprintf(stdout," btask_start      : %s\n", btask_start      ? "true":"false");
        //fprintf(stdout," btask_end        : %s\n", btask_end        ? "true":"false");
        //fprintf(stdout," bcpus_per_worker : %s\n", bcpus_per_worker ? "true":"false");
        fclose(fp);

        // error check
		//if_success = tf_configuration_check( tfc );

		// DEV
		if_success = true;
        return if_success;
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
    bool if_success = false;
    
    int brank,bsize; 

    int n_workgroup;
    int workgroup_tag;
    int workgroup_size;
    int worker_rank;

    const int n_workers_per_workgroup = tfc->cpus_per_workgroup;

    MPI_Comm_size(*base_comm,&bsize);
    MPI_Comm_rank(*base_comm,&brank);
    
    // Possible Error Check 



    /* * *
        MPI_COMM SPLIT START  
    * * */

    // initial set: workgroup-tag for each CPU
    workgroup_tag =  brank / n_workers_per_workgroup;

    /*
        rank                    => mpi_rank
        n_workers_per_workgroup => number of cpus allocated per workgroup

        EXAMPLE>

        CPU.ID  |   rank  / n_workers_per_workgroup         |   tag
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
    if( bsize % n_workers_per_workgroup == 0 ){
        n_workgroup = bsize / n_workers_per_workgroup;
        n_workgroup++;
        /* case - 1: no residual cpus to be the 'master'
            last cpu in last workgroup -> 'master'          */
    }
    else if( bsize % n_workers_per_workgroup == 1 ){                        
        n_workgroup = bsize / n_workers_per_workgroup;
        n_workgroup++;
        /* case - 2: 1 residual cpu in the last workgroup
            last workgroup -> 'master'                      */
    }
    else{                                                                   
        n_workgroup = bsize / n_workers_per_workgroup + 1; // '+1' means additional workgroup formed by the residual CPUs
        n_workgroup++;
        /* case - 3: >1 residual cpus in the last workgroup
            split last workgroup (workgroup_size-1 / 1) and the last workgroup -> 'master'  */
    }

    // final set: workgroup tag
    if( ((brank + 1) == bsize) && ((bsize % n_workers_per_workgroup) != 1) ){
        
        // ((brank + 1) == bsize)                           => if this is 'last CPU'
        // ((bsize % n_workers_per_workgroup) != 1)         => if this is 'last CPU' and sinlge CPU workgroup
        workgroup_tag = workgroup_tag + 1;  //              => +1 to its workgroup_tag, in order to use as 'master'
    }

    // split: base_comm (=MPI_COMM_WORLD) -> workgroup_comm
    MPI_Comm_split(*base_comm, workgroup_tag, brank, workgroup_comm);
    MPI_Comm_size(*workgroup_comm,&workgroup_size);
    MPI_Comm_rank(*workgroup_comm,&worker_rank);

    // final set: taskframconfig parameters
    tfc->n_workgroup = n_workgroup;             // global this include 'workgroup# + 1 (master)'
    tfc->workgroup_tag = workgroup_tag;         // local
    tfc->workgroup_size = workgroup_size;       // local
    tfc->worker_rank = worker_rank;             // local

// do check required
    if_success = true;

    MPI_Barrier(*base_comm);

    return if_success;
}



/*
 *  For the split workgroup communicators: configuring the workgroups
 *  - setting tags for workgroup configurations
 */
bool tf_get_workgroup_config(
    const MPI_Comm* base_comm,          // IN
    const MPI_Comm* workgroup_comm,     // IN
    const TaskFarmConfiguration* tfc,   // IN
    WorkgroupConfig* workgroup_config   // IN-OUT
)
{
    bool if_success = false;
    // Trick to access specific cpu core
    // if( workgroup_tag == i && worker_rank == 0 ) {
    //     WorkgroupConfig workgroup_config[i].base_rank;
    //
    int base_root = 0;

    int brank,bsize;
    int workgroup_size;
    int worker_rank;
    const int n_workgroup = tfc->n_workgroup;
    const int workgroup_tag = tfc->workgroup_tag;

    MPI_Request request;
    MPI_Status status;

    WorkgroupConfig wgc;

    // action starts

    MPI_Comm_rank(*base_comm,&brank);
    MPI_Comm_size(*base_comm,&bsize);
    
    MPI_Comm_size(*workgroup_comm,&workgroup_size);
    MPI_Comm_rank(*workgroup_comm,&worker_rank);

    wgc.base_size = bsize;                  // global
    wgc.base_rank = brank;                  // local
    wgc.workgroup_tag = workgroup_tag;      // local
    wgc.workgroup_size = workgroup_size;    // local

    /*
        For each workgroup call 'MPI_Send' by it's head rank, send 'WorkgroupConfig' to base_root
        N.B. after this only the base_root knows the taskfarm configuration : 'MPI_TAG' set
    */
    for(int i=1;i<n_workgroup;i++){
        if( workgroup_tag == i && worker_rank == 0 ){
            MPI_Send(&wgc,sizeof(WorkgroupConfig),MPI_CHAR,base_root,i,*base_comm);
            //printf("SEND> workgroup_tag: %d worker_rank: %d / %d (worgroup_size) base_rank: %d\n",workgroup_tag,worker_rank,workgroup_size,brank);
        }
    }

    /*
        Call 'MPI_Recv' by the base_root 'n_workgroup' times to get 'WorkgroupConfig' except the base_root : 'MPI_TAG' set
    */
    for(int i=1;i<n_workgroup;i++){
        if( brank == base_root ){
                MPI_Recv(&workgroup_config[i],sizeof(WorkgroupConfig),MPI_CHAR,MPI_ANY_SOURCE,i,*base_comm,&status);
                //printf("workgroup_tag: %d worker_rank: %d base_rank: %d\n",workgroup_config[i].workgroup_tag,workgroup_config[i].worker_rank,workgroup_config[i].base_rank);
        }
    }

    /*
        in-place copying 'WorkgroupConfig(wgc)'
    */
    if ( workgroup_tag == 0 && worker_rank == 0 ){
        workgroup_config[0] = wgc;
        //MPI_ISend(&wgc,sizeof(WorkgroupConfig),MPI_CHAR,base_root,workgroup_tag,*base_comm);
    }
    MPI_Barrier(*base_comm);
    MPI_Bcast(&workgroup_config[0],sizeof(WorkgroupConfig)*n_workgroup,MPI_CHAR,base_root,*base_comm);

    // DO SOME CHECK 
    if_success = true;

    return if_success;
}


