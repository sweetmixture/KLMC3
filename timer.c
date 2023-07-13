/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:

	07/23 : added 
*/


#include <sys/time.h>
#include <time.h>
                        
double get_time() {     
        struct timeval tv;
        gettimeofday(&tv, NULL);
        return (double)tv.tv_sec + (double)tv.tv_usec / 1000000.0;
}
                        
void getCurrentDateTime(char *dateTimeStr) {
        struct timeval tv;
        gettimeofday(&tv, NULL);

        time_t seconds = tv.tv_sec;
        long int microseconds = tv.tv_usec;

        struct tm* tm = localtime(&seconds);
                
        int year = tm->tm_year + 1900;
        int month = tm->tm_mon + 1;
        int day = tm->tm_mday;
        int hour = tm->tm_hour;
        int minute = tm->tm_min;
        int second = tm->tm_sec;
        
        sprintf(dateTimeStr, "%04d-%02d-%02d %02d:%02d:%02d.%06ld", year, month, day, hour, minute, second, microseconds);
	// length of 'dataTimeStr'[64]
}               
        

