/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/

#ifndef __TASKFARM_PRINT_MESSAGE
#define __TAKSFARM_PRINT_MESSAGE

#include "taskfarm_def.h"

void mprint_stderr(
    const TaskFarmConfiguration* tfc,
    const char* msg
);

void wprint_stderr(
    const TaskFarmConfiguration* tfc,
    const char* msg
);


#endif
