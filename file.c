/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/

#include "file.h"

bool file_exists(
    const char* filename
){
    if( access(filename,F_OK) == 0 ){
        return true;
    }
    else{
        return false;
    }
}



