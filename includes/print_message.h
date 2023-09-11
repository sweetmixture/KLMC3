/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/

#ifndef __TASKFARM_PRINT_MESSAGE
#define __TASKFARM_PRINT_MESSAGE

#include <stdio.h>
#include <string.h>

#define __FILENAME__ (strrchr(__FILE__, '/') ? strrchr(__FILE__, '/') + 1 : __FILE__)

#define ANSI_COLOR_RED      "\x1b[31m"
#define ANSI_COLOR_GREEN    "\x1b[32m"
#define ANSI_COLOR_YELLOW   "\x1b[33m"
#define ANSI_COLOR_BLUE     "\x1b[34m"
#define ANSI_COLOR_MAGENTA  "\x1b[35m"
#define ANSI_COLOR_CYAN     "\x1b[36m"
#define ANSI_COLOR_RESET    "\x1b[0m"

#include "taskfarm_def.h"

void print_stdout(
	const int mpi_rank,
	const char* mgs
);

void print_stderr(
    const int mpi_rank,
    const char* msg
);

void fflush_channel(
	FILE* channel
);

void master_print_message(
    const TaskFarmConfiguration* tfc,
    FILE* channel,
    const char* msg
);

void worker_print_message(
    const TaskFarmConfiguration* tfc,
    FILE* channel,
    const char* msg
);


/*
 *
 * Output decorators: uses in taskfarm.c
 *
 */

void layout_line_stdout(
    const int mpi_rank,
    const char c,
	const int n
);

#endif
