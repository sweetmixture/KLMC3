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
/*  ^^^^ some reserved names from this header file
 *  TF_CONFIG_FILE          "taskfarm.config"
 *  TF_MAIN_FILE            "taskfarm.log"
 *  TF_APPLICATION_GULP     "gulp"
 *  TF_APPLICATION_FHIAIMS  "fhiaims"
 */
#include "read_input.h"

#include "error.h"

/* * *
 * only use in this source
 * * */


/* * *

    Description:

    TaskFarm Configuration

* * */
bool tf_get_taskfarm_configuration(
const MPI_Comm* base_comm,                  // intent :: IN
TaskFarmConfiguration* tfc                  // intent :: IN-OUT
)
{
    int brank;                // MPI base rank : visible any processors
    int bsize;                // MPI base size : visible any processors
    
    bool berr;
    
    FILE* fp = NULL;          // file-pointer  : taskfarm configuration file
    
    MPI_Comm_size(*base_comm,&bsize);
    MPI_Comm_rank(*base_comm,&brank);
    
/* * *
 * Set up MPI related variables
 *
 * tfc <TaskFarmConfiguration> : ...
 * * */
    tfc->bsize = bsize;
    tfc->brank = brank;
    tfc->mrank = bsize - 1;
    
    
    /* * *
     * Read TF_CONFIG_FILE 
     * * */
    fp = fopen(TF_CONFIG_FILE,"r");    // READ-IN INITIAL TASKFARM CONFIGURATION 

    if( fp == NULL ){
        /* * *
         * Error <finder> : configuratio file not found
         * * */
        berr = error_taskfarm_filenotfound(base_comm);
        return berr;
    }
    else{
    /* * *
     * Read-In & Setting Main Input File
     * * */

        /* * *
         * Init taskfarm-configuration
         * * */
        strcpy(tfc->application, "-1");
        tfc->task_start         = -1;
        tfc->task_end           = -1;
        tfc->cpus_per_workgroup = -1;
        // Init end;

        // application mode <str>        : InputFile Read 
        read_input_spatternfinder(fp, "application"    , &(tfc->application[0])     );

        // task / workgroup config <int> : InputFile Read
        read_input_ipatternfinder(fp, "task_start"     , &(tfc->task_start)         );
        read_input_ipatternfinder(fp, "task_end"       , &(tfc->task_end)           );
        read_input_ipatternfinder(fp, "cpus_per_worker", &(tfc->cpus_per_workgroup) );

        /* [0] GULP Ready-Input mode */
        if( strcmp(tfc->application,"gulp") == 0 ){
            sprintf(&(tfc->algorithm[0]),"Ready-Input");
        }
#ifdef USE_PYTHON
    /* --------------------------------------------
        10.2024 
        GULP algorithm based modes - Template
        mode 1 : Random-Quenching
        mode 2 : Solid-Solution
        mode 3 : Basin-Hoping
        mode 4 : Simulated-Annealing
        mode 5 : Metropolis-MonteCarlo
        mode 6 : Lid-MonteCarlo
        mode 7 : Surface-Cluster Translation
        mode 8 : Genetic-Algorithm
       -------------------------------------------- */
        if( strcmp(tfc->application,"gulp-rq") == 0 ){
            sprintf(&(tfc->algorithm[0]),"Random-Quenching");
            // do some more actions to read extra input files
        }
        if( strcmp(tfc->application,"gulp-ss") == 0 ){
            sprintf(&(tfc->algorithm[0]),"Solid-Solution");
            // do some more actions to read extra input files
        }
        if( strcmp(tfc->application,"gulp-bh") == 0 ){
            sprintf(&(tfc->algorithm[0]),"Basin-Hopping");
            // do some more actions to read extra input files
        }
        if( strcmp(tfc->application,"gulp-sa") == 0 ){
            sprintf(&(tfc->algorithm[0]),"Simulated-Annealing");
            // do some more actions to read extra input files
        }
        if( strcmp(tfc->application,"gulp-mm") == 0 ){
            sprintf(&(tfc->algorithm[0]),"Metropolis-MonteCarlo");
            // do some more actions to read extra input files
        }
        if( strcmp(tfc->application,"gulp-lm") == 0 ){
            sprintf(&(tfc->algorithm[0]),"Lid-MonteCarlo");
            // do some more actions to read extra input files
        }
        if( strcmp(tfc->application,"gulp-sc") == 0 ){
            sprintf(&(tfc->algorithm[0]),"Surface-Cluster Translation");
            // do some more actions to read extra input files
        }
        if( strcmp(tfc->application,"gulp-ga") == 0 ){
            sprintf(&(tfc->algorithm[0]),"Genetic-Algorithm");
            // do some more actions to read extra input files
        }

    /* --------------------------------------------
        08.11.2023
        If Python used
       -------------------------------------------- */
        if( strcmp(tfc->application,"python") == 0 ){
            sprintf(&(tfc->algorithm[0]),"custom single core python : only valid for using single core workgroups");
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

/* --------------------------------------------
 *  Spliting MPI_Communicator
 *  > last cpu core is always dedicated to the 'master'
 * -------------------------------------------- */
bool tf_config_workgroup( 
const MPI_Comm* base_comm,                    // intent :: IN
MPI_Comm* workgroup_comm,                     // intent :: IN-OUT
TaskFarmConfiguration* tfc                    // intent :: IN-OUT
)
{
    bool berr = true;
    
    int bsize;                        // global
    int brank;                        // local

    int n_workgroup;                  // global : workgroup count : number of workgroups
    int workgroup_tag;                // local  : workgroup tag   : ~ MPI color : which workgroup is this?
    int workgroup_size;               // local  : workgroup size  : size of workgroup
    int worker_rank;                  // local  : worker cpu rank : rank of worker in workgroup (i.e., local rank)

    MPI_Comm_size(*base_comm,&bsize);
    MPI_Comm_rank(*base_comm,&brank);
    MPI_Get_processor_name(&(tfc->proc_name[0]),&(tfc->proc_name_len));
    
    /* * *
     * bool error_taskfarm_commsplit_nproc_check()
     *
     * OMP_NUM_THREADS    check -> warning  : if OMP_NUM_THREAD is not equal to 1
     * nproc              check -> error    : if avaiable cpus count = 1 : cannot configure workgroups !
     * nproc balance      check -> warning  : if workgroup allocated across a single node
     * * */
    berr = error_taskfarm_commsplit_nproc_check(base_comm,tfc);
    if( !berr ){
        return berr;
    }

    /* * *
     * STEP1 : get workgroup tag (= mpi_color variable)
     * * */
    workgroup_tag = (brank/tfc->cpus_per_workgroup);
    /* 
        Result: each processor will get own 'tag'

        > Example case : 11 CPUs with 2 workgroups

        brank                   (n)  => local  : mpi_rank
        tfc->cpus_per_workgroup (2)  => global : number of cpus allocated per workgroup

        CPU.ID  |   brank / tfc->cpus_per_workgroup         |   tag
            0   |       0 / 2                               |    0
            1   |       1 / 2                               |    0
            2   |       2 / 2                               |    1
            3   |       3 / 2                               |    1
            4   |       4 / 2                               |    2
            5   |       5 / 2                               |    2
            6   |       6 / 2                               |    3
            7   |       7 / 2                               |    3
            8   |       8 / 2                               |    4
            9   |       9 / 2                               |    4
           10   |      10 / 2                               |    5
    */    

    /* * *
     * STEP2 : get number of workgroups : n_workgroup = number of workgroups + 1 cpu allocated to master 
     * * */
    if( bsize % tfc->cpus_per_workgroup == 0 ){

        n_workgroup = bsize / tfc->cpus_per_workgroup;

        if( tfc->cpus_per_workgroup > 1 ){
            n_workgroup++;
        }
        /* case - 1: no residual cpus to be the 'master'
         * last cpu in last workgroup -> 'master'
         */
    }
    else if( bsize % tfc->cpus_per_workgroup == 1 ){                        
        n_workgroup = bsize / tfc->cpus_per_workgroup;
        n_workgroup++;
        /* case - 2: 1 residual cpu in the last workgroup
         * set last workgroup to master
         */
    }
    else{                                                                   
        n_workgroup = bsize / tfc->cpus_per_workgroup + 1; // '+1' means additional workgroup formed by the residual CPUs
        n_workgroup++;
        /* case - 3: >1 residual cpus in the last workgroup
         * split last workgroup (workgroup_size-1 / 1) and the last workgroup -> 'master'
         */
    }

    /* * *
     * STEP3: set workgroup tag (mpi-color; identifier)
     * * */
    if( ((brank + 1) == bsize) && ((bsize % tfc->cpus_per_workgroup) != 1) ){
        
        /* Explain:
         * [1] ((brank + 1) == bsize)                          => if this is 'last CPU'
         * [2] ((bsize % tfc->cpus_per_workgroup) != 1)        => if this is 'last CPU' and sinlge CPU workgroup
         */

        /* exceptional case if cpus_per_workgroup is not .eq. 1
         */
        if( tfc->cpus_per_workgroup > 1 ){
            workgroup_tag = workgroup_tag + 1;  //            => +1 to its workgroup_tag, in order to use as 'master'
        }
    }

    /* Example

       for 9 cpus, using 5 cpus per workgroup case

       final expected cpu configuration :

       STEP1:

           CPU-ID   0   1   2   3   4   5   6   7   8
            tag     0   0   0   0   0   1   1   1   1

       STEP2:

           n_workgroup = 3 (2+1)

       STEP3:

           CPU-ID   0   1   2   3   4   5   6   7   8
            tag     0   0   0   0   0   1   1   1   2
                    ^^^^^^^^^^^^^^^^^   ^^^^^^^^^   ^
                    Group 0             Group 1     Group 2 (master)
     */

    /* * *
     * STEP4: base MPI communicator split, i.e., base_comm -> multiple workgroup_comm : idenifier, workgroup_tag
     * note. processors with same 'tag' (workgroup_tag) carries same 'workgroup_comm'
     * * */
    MPI_Comm_split(*base_comm, workgroup_tag, brank, workgroup_comm);
    MPI_Comm_size(*workgroup_comm,&workgroup_size);
    MPI_Comm_rank(*workgroup_comm,&worker_rank);

    /* * *
     * STEP5: final set: taskframconfig parameters
     * * */
    tfc->n_workgroup = n_workgroup;             // global : workgroup(n) + master(1)
    tfc->workgroup_tag = workgroup_tag;         // local  : owner cpu only
    tfc->workgroup_size = workgroup_size;       // local  : owner cpu only
    tfc->worker_rank = worker_rank;             // local  : owner cpu only

    return berr;
}

/* --------------------------------------------
 * for split workgroup communicators, configuring the workgroups
 * > setting identifier (workgroup_tag) for workgroup configurations
 *
 * support accessing specific processor (based on rank of base-communicator)
 * > example, accessing rank 0 cpu of workgroup with identifier, 'i'
 * ...
 *   if( workgroup_tag == i && worker_rank == 0 ){
 *       wgc_global[i].base_rank // <WorkgroupConfig arr>
 *       ...
 *   }
 * ...
 * -------------------------------------------- */
bool tf_get_workgroup_config(
    const MPI_Comm* base_comm,                   // intent :: IN
    const MPI_Comm* workgroup_comm,              // intent :: IN
    const TaskFarmConfiguration* tfc,            // intent :: IN
    WorkgroupConfig* wgc_global                  // intent :: IN-OUT
)
{
    bool berr = true;

    const int base_root = 0;
    const int n_workgroup = tfc->n_workgroup;        // global : number of workgroups : workgroups(n) + master(1) = n + 1
    const int workgroup_tag = tfc->workgroup_tag;    // local  : workgroup identifier

    int bsize;
    int brank;
    int workgroup_size;
    int worker_rank;

    MPI_Request request;
    MPI_Status status;

    WorkgroupConfig wgc_local;    // local : workspace

/* * *
 * configuration start
 * * */
    MPI_Comm_size(*base_comm,&bsize);
    MPI_Comm_rank(*base_comm,&brank);
    
    MPI_Comm_size(*workgroup_comm,&workgroup_size);
    MPI_Comm_rank(*workgroup_comm,&worker_rank);

    /* * *
     * set local workgroup configuration
     * * */
    wgc_local.base_size = bsize;                  // global : size of base communicator
    wgc_local.base_rank = brank;                  // local  : rank of base communicator
    wgc_local.workgroup_size = workgroup_size;    // local  : workgroup size where this cpu belongs to
    wgc_local.workgroup_tag = workgroup_tag;      // local  : workgroup tag  where this cpu belongs to

    /* Example

       for 9 cpus, using 5 cpus per workgroup case

       what saved in 'wgc_local' of each cpu is:

       global_cpu_rank  |   0    1    2    3    4    5    6    7    8
       base_size        |   9    9    9    9    9    9    9    9    9
       base_rank        |   0    1    2    3    4    5    6    7    8
       workgroup_size   |   5    5    5    5    5    3    3    3    1
       workgroup_tag    |   0    0    0    0    0    1    1    1    2
                            ^^^^^^^^^^^^^^^^^^^^^    ^^^^^^^^^^^    ^
                            Group(0)                 Group(1)       Group(2) - master
     */

    MPI_Barrier(*base_comm);

    /* * *
     * STEP1 : messaging local workgroup configuration to base communicator root
     * * */
    for(int i=1;i<n_workgroup;i++){
        if( workgroup_tag == i && worker_rank == 0 ){
        /* 
            from the 'head_rank(0)' of 'workgroup(i)', send it's information :

                (1) base_size            -> DO REALLY NEED THIS?
                (2) base_rank
                (3) workgroup_tag
                (4) workgroup_size

            to 'base_root' (base_rank = 0 )

            note. message from 'workgroup(i)' is identified by the MPI_Send 'tag(i)'

            1. (3) and (4) could be useful
        */
            MPI_Send(&wgc_local,sizeof(WorkgroupConfig),MPI_CHAR,base_root,i,*base_comm);

        /* Example

           for 9 cpus, using 5 cpus per workgroup case

                                                         ↓ this imformation is sent (i=1)
                                                                        ↓ this imformation is sent (i=2)
           global_cpu_rank  |   0    1    2    3    4    5    6    7    8
           base_size        |   9    9    9    9    9    9    9    9    9
           base_rank        |   0    1    2    3    4    5    6    7    8
           workgroup_size   |   5    5    5    5    5    3    3    3    1
           workgroup_tag    |   0    0    0    0    0    1    1    1    2
                                ^^^^^^^^^^^^^^^^^^^^^    ^^^^^^^^^^^    ^
                                Group(0)                 Group(1)       Group(2) - master
         */
        }
    }

    /* * *
     * STEP2: message collecting by base_root
     * * */
    for(int i=1;i<n_workgroup;i++){
        if( brank == base_root ){
            /*
                message (wgc_local) from 'head_rank(0)' of 'workgroup(i)' with identifier(i) <MPI_TAG>

                received to 'wgc_global[i]' by base_root (base_rank = 0)
            */
            MPI_Recv(&wgc_global[i],sizeof(WorkgroupConfig),MPI_CHAR,MPI_ANY_SOURCE,i,*base_comm,&status);

        /* Example

           for 9 cpus, using 5 cpus per workgroup case

                                ↓ 'x' are unknow before the in-place copy in STEP3
           wgc_global i     |   0    1    2
           base_size        |   x    9    9
           base_rank        |   x    5    8
           workgroup_size   |   x    3    1
           workgroup_tag    |   x    1    2
                                    [1]  [2]

           [1] workgroup(1) information : e.g., base_rank 5 refers to, in the workgroup(1), workgroup master rank, and with workgroup tag 1
           [2] workgroup(2) information : 
         */
        }
    }

    /* * *
     * STEP3: in-place copy (i=0) : wgc_global[0] <- wgc_local & MPI_Bcast()
     * * */
    if ( workgroup_tag == 0 && worker_rank == 0 ){
        wgc_global[0] = wgc_local;
    }
    /* Example

       for 9 cpus, using 5 cpus per workgroup case

       after in-place copy

       wgc_global i     |   0    1    2
       base_size        |   9    9    9
       base_rank        |   0    5    8
       workgroup_size   |   5    3    1
       workgroup_tag    |   0    1    2
                           [0]  [1]  [2]

      ! only known by the base_root cpu until this point
     */
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
