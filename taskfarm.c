/*
    Author: Woongkyu Jee / woong.jee.16@ucl.ac.uk
    Affiliation: University College London
    Date: 2023.05.25 - 

    Description:
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <mpi.h>

#include "taskfarm_def.h"

// Types of 'master-worker' implementation
#include "master_worker_ready_input.h"                // default: 07.23

#ifdef USE_PYTHON
#include "master_worker_python.h"                    // python single processor interface: 11.23


/*
 * Using KLMC Built-in Capabilities 10.2024 wkjee
 */

#endif

#include "print_message.h"
#include "timer.h"

// develop check only
#include "unit_test.h"

int main(int argc, char* argv[])
{
    char   msg[512];                              // message buffer for any
    char   currentTime[64];                       // humand readable time

    double start_t;                               // KLMC start   time (internal)
    double end_t;                                 // KLMC end     time (internal)
    double total_elapsed_t;                       // KLMC elapsed time

    bool   berr;

    int    brank;                                 // MPI base rank : visible any processors
    int    bsize;                                 // MPI base size : visible any processors

    MPI_Comm BaseComm;                            // define base communicator : visible any processor
    BaseComm = MPI_COMM_WORLD;                    //

    MPI_Init(&argc,&argv);
    MPI_Comm_size(BaseComm,&bsize);
    MPI_Comm_rank(BaseComm,&brank);

    // * * * configuring BaseComm <MPI_Comm> done

    MPI_Comm WorkgroupComm;                       // define workgroup communicator  : visible only by workgroup processors
    TaskFarmConfiguration tfc;                    // generic resource configuration : MPI based > see 'taskfarm_def.h'

    /* * * * *
     * 1 STEP : READ INPUT
     * * * * */
    berr = tf_get_taskfarm_configuration( &BaseComm, &tfc );
    /*  function 'tf_get_taskfarm_configuration()' is responsible for reading-in generic inputs
     */

    MPI_Barrier(BaseComm);

    if( !berr ){ // taskfarm configure failed
        exit(1);
    }
    else{
        // PRINT <stdout> : info from 'taskfarm.config'
        print_stdout(tfc.brank,"\n");
        print_stdout(tfc.brank," KLMC3 Parallel Extension Template 0.1\n");
        print_stdout(tfc.brank,"\n");
        print_stdout(tfc.brank," Woongkyu Jee, University College London / woong.jee.16@ucl.ac.uk\n");
        print_stdout(tfc.brank," Date: 2023.05 - \n");
        print_stdout(tfc.brank,"\n");
        layout_line_stdout(tfc.brank,'-',80);
        print_stdout(tfc.brank,"\n");
        print_stdout(tfc.brank," TaskFarm Configuration\n");
        print_stdout(tfc.brank,"\n");
        sprintf(msg," Application: %s ",tfc.application); print_stdout(tfc.brank,msg);
        if( strcmp(tfc.application,"gulp") == 0 ){
            print_stdout(tfc.brank,"6.1.2\n");
        }
        else if( strcmp(tfc.application,"fhiaims") == 0 ){
            print_stdout(tfc.brank,"11.23\n");
        }
#ifdef USE_PYTHON
/* --------------------------------------------
    08.11.2023
    If Python used
   -------------------------------------------- */
        else if( strcmp(tfc.application,"python") == 0 ){
            print_stdout(tfc.brank,"\n");
            sprintf(msg," Python Module Path : %s\n",tfc.python_module_path); print_stdout(tfc.brank,msg);
            sprintf(msg," Python Module Name : %s\n",tfc.python_module_name); print_stdout(tfc.brank,msg);
            sprintf(msg," Python Method      : %s\n",tfc.python_method_name); print_stdout(tfc.brank,msg);
        }
#endif
        print_stdout(tfc.brank,"\n");
        sprintf(msg," Request number of tasks         :       %d\n",tfc.num_tasks); print_stdout(tfc.brank,msg);
        sprintf(msg," task start index                :       %d\n",tfc.task_start); print_stdout(tfc.brank,msg);
        sprintf(msg," task end   index                :       %d\n",tfc.task_end); print_stdout(tfc.brank,msg);
        sprintf(msg," Requested CPUs per workgroup    :       %d\n",tfc.cpus_per_workgroup); print_stdout(tfc.brank,msg);
        print_stdout(tfc.brank,"\n");
        sprintf(msg," Total number of CPUs (resource) :       %d\n",bsize); print_stdout(tfc.brank,msg);
        print_stdout(tfc.brank,"\n");
        layout_line_stdout(tfc.brank,'-',80);
        fflush_channel(NULL);
    }
    MPI_Barrier(BaseComm);

    /* * * * *  deprecated 29.08.23 ( set in 'tf_get_taskfarm_configuration()'
     * Stop using 'brank' -- replaced --> tfc.brank
     * Stop using 'bsize' -- replaced --> tfc.bsize
     * Stop using 'mrank' -- replaced --> tfc.mrank
     * * * * */

    /* * * * *
     * 2 STEP : MPI COMMUNICATOR SPLITTING : note. processors with same 'tag' (workgroup_tag) carries same 'workgroup_comm'
     * * * * */
    berr = tf_config_workgroup(&BaseComm, &WorkgroupComm, &tfc );
    MPI_Barrier(BaseComm);

    if( !berr ){ // taskfarm configure failed
        exit(1);
    }
    else{
        // PRINT <stdout> : taskfarm cpu configurations
        print_stdout(tfc.brank,"\n");
        sprintf(msg," CPU Configuration < MPI_Comm_split done >\n"); print_stdout(tfc.brank,msg);
        print_stdout(tfc.brank,"\n");
        sprintf(msg," number of workgroups : %d",tfc.n_workgroup); print_stdout(tfc.brank,msg);
        sprintf(msg," (last workgroup is always dedicated to 'master')\n"); print_stdout(tfc.brank,msg);
        print_stdout(tfc.brank,"\n");
        fflush_channel(NULL);
        MPI_Barrier(BaseComm);

        char all_msg[tfc.bsize][sizeof(msg)];

        if( tfc.brank != tfc.mrank ){
            sprintf(msg," node %12s | group %6d | procid %6d | group_procid %6d\n", tfc.proc_name,tfc.workgroup_tag,brank,tfc.worker_rank);
        }
        else{
            sprintf(msg," node %12s | group %6d | procid %6d | group_procid %6d > master\n", tfc.proc_name,tfc.workgroup_tag,brank,tfc.worker_rank);
        }
        MPI_Barrier(BaseComm);

        MPI_Allgather(msg,sizeof(msg),MPI_CHAR,all_msg,sizeof(msg),MPI_CHAR,BaseComm);    // collecting messages from each processors ( rank )

        for(int i=0;i<tfc.bsize;i++){
            print_stdout(tfc.brank,all_msg[i]);
            fflush_channel(NULL);
        }
        print_stdout(tfc.brank,"\n");
        layout_line_stdout(tfc.brank,'-',80);
        fflush_channel(NULL);
    }
    MPI_Barrier(BaseComm);


    /* * *
     New parameter setting: workgroup config saver    -> 08.23 - only used by 'master': send messages to 'head' (subrank = 0) of each workgroup - 'base_rank' 
     * * */
    WorkgroupConfig wgc_global[tfc.n_workgroup];

    /* * * * *
     * 3 STEP : WORKGROUP CONFIGURATION SETTING
     * * * * */
    berr = tf_get_workgroup_config(&BaseComm,&WorkgroupComm,&tfc,&wgc_global[0]);    // make sure this is done before calling master()/workgroup()
    MPI_Barrier(BaseComm);

    if( !berr ){ // berr is always 'true' in this version 08.23
        exit(1);
    }

/* * * * * * * * * * * * * * * * *
 * TASK FARM CONFIGURATION DONE
 * * * * * * * * * * * * * * * * */

    /* * *
     * timing start
     * * */
    start_t = get_time();
    getCurrentDateTime(currentTime);

    // PRINT <stdout>
    print_stdout(tfc.brank,"\n");
    sprintf(msg," TaskFarm starts at : %s\n",currentTime); print_stdout(tfc.brank,msg);
    print_stdout(tfc.brank,"\n");
    layout_line_stdout(tfc.brank,'-',80);
    fflush_channel(NULL);
    MPI_Barrier(BaseComm);

    /* * * * * * * * * * * * * * * * * * * * *
     * TASK FARM MAIN START
     * * * * * * * * * * * * * * * * * * * * *
     * cases using Python supported sampler
     * (0) Ready Input                 RI : read prepared GULP input files from disk 07/23 wkjee full feature
     * (1) Random Quenching            RQ : NotImplemented
     * (2) Solid-Solution              SS : NotImplemented
     * (3) Basin-Hoping                BH : NotImplemented
     * (4) Simulated-Annealing         SA : NotImplemented
     * (5) Metropolis MonteCarlo       MM : NotImplemented
     * (6) Lid MonteCarlo              LM : NotImplemented
     * (7) Surface-Cluster translation SC : NotImplemented
     * (8) Genetic Algorithm           GA : NotImplemented
     * (9) Python Interface            PI : NotImplemented
     * * * * * * * * * * * * * * * * * * * * */

    print_stdout(tfc.brank,"\n");
    sprintf(msg," Algorithm : %s\n",tfc.algorithm); print_stdout(tfc.brank,msg);
    print_stdout(tfc.brank,"\n");
    layout_line_stdout(tfc.brank,'-',80);
    fflush_channel(NULL);
    MPI_Barrier(BaseComm);

    if( tfc.brank == tfc.mrank ){
        // mode 0 : Ready Input - default
        if( strcmp(tfc.application,"gulp") == 0 ){
            berr = ready_input_call_master( &BaseComm, &tfc, &wgc_global[0] );
        }
#ifdef USE_PYTHON
/* * * * * * * * * * * * * * * * * * * * *
 * 10.24 wkjee
 * Master cases using Python supported sampler - Template
 * mode 1 : Random-Quenching
 * mode 2 : Solid-Solution
 * mode 3 : Basin-Hoping
 * mode 4 : Simulated-Annealing
 * mode 5 : Metropolis-MonteCarlo
 * mode 6 : Lid-MonteCarlo
 * mode 7 : Surface-Cluster Translation
 * mode 8 : Genetic-Algorithm
 * * * * * * * * * * * * * * * * * * * * */
        if ( strcmp(tfc.application,"gulp-rq") == 0 ){
            //berr = solidsolution_call_master( &BaseComm, &tfc, &wgc_global[0] );
        }
        if ( strcmp(tfc.application,"gulp-ss") == 0 ){
            //berr = solidsolution_call_master( &BaseComm, &tfc, &wgc_global[0] );
        }
        if ( strcmp(tfc.application,"gulp-bh") == 0 ){
            //berr = solidsolution_call_master( &BaseComm, &tfc, &wgc_global[0] );
        }
        if ( strcmp(tfc.application,"gulp-sa") == 0 ){
            //berr = solidsolution_call_master( &BaseComm, &tfc, &wgc_global[0] );
        }
        if ( strcmp(tfc.application,"gulp-mm") == 0 ){
            //berr = solidsolution_call_master( &BaseComm, &tfc, &wgc_global[0] );
        }
        if ( strcmp(tfc.application,"gulp-lm") == 0 ){
            //berr = solidsolution_call_master( &BaseComm, &tfc, &wgc_global[0] );
        }
        if ( strcmp(tfc.application,"gulp-sc") == 0 ){
            //berr = solidsolution_call_master( &BaseComm, &tfc, &wgc_global[0] );
        }
        if ( strcmp(tfc.application,"gulp-ga") == 0 ){
            //berr = solidsolution_call_master( &BaseComm, &tfc, &wgc_global[0] );
        }

/* * *
 * Python single core - DEV ONLY
 * * */
        // mode 9 : Python only
        if( strcmp(tfc.application,"python") == 0 ){
            berr = ready_input_call_master_python( &BaseComm, &tfc, &wgc_global[0] );
        }
#endif
    } // -------------------------------------------------------------------------------- MASTER 
    else{
        // mode 0 : Ready Input
        if( strcmp(tfc.application,"gulp") == 0 ){
            ready_input_call_workgroups( &BaseComm, &WorkgroupComm, tfc.n_workgroup, tfc.workgroup_tag );
        }
#ifdef USE_PYTHON
/* * * * * * * * * * * * * * * * * * * * *
 * 10.24 wkjee
 * Worker cases using Python supported sampler - Template
 * mode 1 : Random-Quenching
 * mode 2 : Solid-Solution
 * mode 3 : Basin-Hoping
 * mode 4 : Simulated-Annealing
 * mode 5 : Metropolis-MonteCarlo
 * mode 6 : Lid-MonteCarlo
 * mode 7 : Surface-Cluster Translation
 * mode 8 : Genetic-Algorithm
 * * * * * * * * * * * * * * * * * * * * */
        if( strcmp(tfc.application,"gulp-rq") == 0 ){
            //solidsolution_call_workgroups( &BaseComm, &WorkgroupComm, tfc.n_workgroup, tfc.workgroup_tag );
        }
        if( strcmp(tfc.application,"gulp-ss") == 0 ){
            //solidsolution_call_workgroups( &BaseComm, &WorkgroupComm, tfc.n_workgroup, tfc.workgroup_tag );
        }
        if( strcmp(tfc.application,"gulp-bh") == 0 ){
            //solidsolution_call_workgroups( &BaseComm, &WorkgroupComm, tfc.n_workgroup, tfc.workgroup_tag );
        }
        if( strcmp(tfc.application,"gulp-sa") == 0 ){
            //solidsolution_call_workgroups( &BaseComm, &WorkgroupComm, tfc.n_workgroup, tfc.workgroup_tag );
        }
        if( strcmp(tfc.application,"gulp-mm") == 0 ){
            //solidsolution_call_workgroups( &BaseComm, &WorkgroupComm, tfc.n_workgroup, tfc.workgroup_tag );
        }
        if( strcmp(tfc.application,"gulp-lm") == 0 ){
            //solidsolution_call_workgroups( &BaseComm, &WorkgroupComm, tfc.n_workgroup, tfc.workgroup_tag );
        }
        if( strcmp(tfc.application,"gulp-sc") == 0 ){
            //solidsolution_call_workgroups( &BaseComm, &WorkgroupComm, tfc.n_workgroup, tfc.workgroup_tag );
        }
        if( strcmp(tfc.application,"gulp-ga") == 0 ){
            //solidsolution_call_workgroups( &BaseComm, &WorkgroupComm, tfc.n_workgroup, tfc.workgroup_tag );
        }
/* * *
 * Python single core - DEV ONLY
 * * */
        // mode 9 : Python only
        if( strcmp(tfc.application,"python") == 0 ){
            ready_input_call_workgroups_python( &BaseComm, &WorkgroupComm, tfc.n_workgroup, tfc.workgroup_tag, &tfc );
        }
#endif
    } // -------------------------------------------------------------------------------- WORKER
    /* * * * *
     * TASK FARM MAIN END
     * * * * */
    MPI_Barrier(BaseComm);

    /* * *
        timing end
    * * */
    end_t = get_time();
    getCurrentDateTime(currentTime);

    /* * *
        finalising taskfarm
    * * */
    print_stdout(tfc.brank,"\n");
    sprintf(msg," TaskFarm ends at   : %s\n",currentTime); print_stdout(tfc.brank,msg);
    sprintf(msg," Elapsed time (s)   : %.8lf\n",end_t - start_t); print_stdout(tfc.brank,msg);
    print_stdout(tfc.brank,"\n");
    layout_line_stdout(tfc.brank,'-',80);
    fflush_channel(NULL);
    MPI_Barrier(BaseComm);

    MPI_Comm_free(&WorkgroupComm);
    MPI_Finalize();

    return 0;
}
