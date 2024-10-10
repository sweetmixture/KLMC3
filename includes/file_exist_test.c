#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>

bool error_file_exists(
    const char* filename
){
    if( access(filename,F_OK) == 0 ){
        return true;
    }
    else{
        return false;
    }
}


int main(){

	char file[512];
	strcpy(file,"/mnt/lustre/a2fs-work2/work/e05/e05/wkjee/Software/gulpklmc/klmc3_tf_interface.update.04082023/KLMC3.0823.update/_gnu_build/dev2/run/A19.gi");
	//strcpy(file,"/work/e05/e05/wkjee/Software/gulpklmc/klmc3_tf_interface.update.04082023/KLMC3.0823.update/_gnu_build/dev2/run/A19.gin");

	bool ifEx = error_file_exists( file );

	if( ifEx ){
		printf("file exists\n");
	}
	else{
		printf("file NotFound\n");
	}


	return 0;
}
