/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:

*/

#ifndef __TASKFARM_READ_INPUT
#define __TASKFARM_READ_INPUT

#include <stdbool.h>

bool read_input_ipatternfinder( FILE* fp, const char* pattern, int* ret );

bool read_input_spatternfinder( FILE* fp, const char* pattern, char* ret );

#endif
