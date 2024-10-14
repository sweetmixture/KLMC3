/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25

        Description:
*/

#ifndef __TASKFARM_DEF
#define __TASKFARM_DEF

/* * *
 * MPI tags : getting task type
 * * */
#define TASK_INIT        55
#define TASK_EXECUTED    56
#define TASK_FINISHED    57
#define TASK_DIETAG      666
#define TASK_WORKTAG     777

/* * *
 * some default file names
 * * */
#define TF_CONFIG_FILE          "taskfarm.config"      // taskfarm configuration file
#define TF_MAIN_FILE            "taskfarm.log"
/* * *
 * some default application names
 * * */
#define TF_APPLICATION_GULP     "gulp"
#define TF_APPLICATION_FHIAIMS  "fhiaims"

#ifdef USE_PYTHON
/* --------------------------------------------
    08.11.2023
    If Python used
   -------------------------------------------- */
#define TF_APPLICATION_PYTHON   "python"
#endif

#include <mpi.h>
#include <stdbool.h>
/* * *
 * For reading comments:
 *
 *  'global' : visible by all processor, i.e., every processors keep the same value
 *  'local'  : visible only by owner processor, i.e., values different by processors
 * * */

/* --------------------------------------------
 * TaskFarmConfiguration
 * saving MPI / task-farm configuration
 * 
 * -------------------------------------------- */
typedef struct TaskFarmConfiguration_{
/* * *
 * MPI related variables
 * * */
    int  bsize;                             // global : base size
    int  brank;                             // local  : base rank 
    int  mrank;                             // global : master processor rank

    char proc_name[MPI_MAX_PROCESSOR_NAME]; // local  : processor id
    int  proc_name_len;                     // local  : processor id length
/* * *
 * System related
 * * */
    bool omp_num_threads_set;               // global : if OMP set
    int  omp_num_threads;                   // global : OMP thread count
/* * *
 * KLMC3 : master - worker configuration related
 * * */
    int n_workgroup;                        // global : workgroup number = worker-groups(n) + master(1)
    int workgroup_tag;                      // local  : my workgroup tag    : i.e., group tag  I belong to
    int workgroup_size;                     // local  : my workgroup size    : i.e., group size I belong to
    int worker_rank;                        // local  : my rank in workgroup : i.e., my rank in the workgroup I belong to
/* * * 
 * task related
 * * */
    char application[64];                   // global
 
    int num_tasks;                          // global
    int task_start;                         // global
    int task_end;                           // global
    int cpus_per_workgroup;                 // global

#ifdef USE_PYTHON
/* --------------------------------------------
    08.2023
    If Python used
    
    Following variables for loading a single
    python method/function from a module
   -------------------------------------------- */
    char python_module_path[512];            // global : module path (location of module.py)
    char python_module_name[512];            // global : name of the 'module' make sure it should not take the extention *.py
    char python_method_name[512];            // global : name of the method/function within the 'module'

/* --------------------------------------------
   10.2023
   For providing generic KLMC features
   -------------------------------------------- */
    // NotImplemented
#endif
}TaskFarmConfiguration;





/* --------------------------------------------
 * WorkgroupConfig
 * saving MPI / task-farm configuration
 *
 * -------------------------------------------- */
typedef struct WorkgroupConfig_{

   /* * *

      Description:

         C struct saving taskfarm CPU configuration globally : 'globally' implies, each CPU will have the same copy of it
         c.f. previous 'TaskFarmConfiguration' member variables are either local or global

         This struct is used as array in 'main' (taskfarm.c).

         * The length of array is equal to the number of workgroups (i.e., tfc.n_workgroup)

         EXAMPLE>

            WorkgroupConfig[n] : contains information of 'n'th workgroup

               base_size<int>      : BaseComm size
               base_rank<int>      : BaseComm rank -> this is 'PARTICULARLY USEFUL' when setting communications: 'master' <-> 'workergroup'
               workgroup_tag<int>  : workgroup tag of 'n'th workgroup : (simply) = n
               workgroup_size<int> : size (#CPUs)  of 'n'th workgroup

     more details : see taskfarm_def.c : tf_get_workgroup_config()
   * * */

    int base_size;                 // global
    int base_rank;                 // global
    int workgroup_tag;             // global
    int workgroup_size;            // global

}WorkgroupConfig;


/* * *
   functions
* * */

bool tf_get_taskfarm_configuration(
   const MPI_Comm* base_comm,        // IN
   TaskFarmConfiguration* tfc        // IN-OUT
);

bool tf_config_workgroup(
    const MPI_Comm* base_comm,       // IN
    MPI_Comm* workgroup_comm,        // IN-OUT
    TaskFarmConfiguration* tfc       // IN-OUT
);

bool tf_get_workgroup_config(
   const MPI_Comm* base_comm,        // IN
   const MPI_Comm* workgroup_comm,   // IN
   const TaskFarmConfiguration* tfc, // IN
   WorkgroupConfig* wgc_global       // IN-OUT
);

// general function pointer get library(?) function

#endif
