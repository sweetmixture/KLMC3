/*
        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
        Affiliation:    University College London
        Date:           2023.05.25 - 

        Description:
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

/* * *
	only use in this source
* * */

bool isInteger( const char* str ){
	char *endptr;
	strtol(str,&endptr,10); // base '10' integer conversion
	return *endptr == '\0'; // condition conversion success
}

/* * *

	functions in 'read_input.h'

* * */

// <int> pattern finder
bool read_input_ipatternfinder( FILE* fp, const char* pattern, int* ret ){

	bool if_success = false;
	char  line[1024];
	char* token = NULL;

	int line_number = 0;

	fseek( fp, 0, SEEK_SET );
	
	// 1. read line
	while( fgets(line,sizeof(line),fp) != NULL ){

		line_number++;
		token = strtok(line," \t\n");

		// 2. for the line, find pattern
		while( token != NULL ){

			// * special case: if commented line -> move to the next
			if( strcmp(token,"#") == 0 ){
				break;
			}

			// 2 - 1: token pattern check
			if( strcmp(token,pattern) == 0 ){
				token = strtok(NULL," \t\n");

				// token NULL
				if( token == NULL ){

					// error message?

					// dev tmp
					printf("ipattern expected following arg not found after: %s\n",pattern);

					return if_success; // false
				}
				// token !NULL
				else{
					// token !<int>
					if( !isInteger(token) ){

						// dev tmp
						printf("ipattern expected following arg is not an integer: %s\n",pattern);

						return if_success;
					}
					// token <int>
					else{
						*ret = (int)strtol(token,NULL,10);
						if_success = true;
						return if_success;
					}
				}
			}

			// 2 - 2: move to next token in the line
			token = strtok(NULL," \t\n");
		}
	}

	return if_success;
}


// <char*> pattern finder
bool read_input_spatternfinder( FILE* fp, const char* pattern, char* ret ){

	bool if_success = false;
	char  line[1024];
	char* token = NULL;

	int line_number = 0;

	fseek( fp, 0, SEEK_SET );

	// 1. read line
	while( fgets(line,sizeof(line),fp) != NULL ){

		line_number++;
		token = strtok(line," \t\n");
	
		// 2. for the line, find pattern
		while( token != NULL ){

			// * special case: if commented line -> move to the next
			if( strcmp(token,"#") == 0 ){
				break;
			}

			// 2 - 1: token pattern check
			if( strcmp(token,pattern) == 0 ){
				token = strtok(NULL," \t\n");

				// token NULL
				if( token == NULL ){

					// error message?

					// dev tmp
					printf("spattern: expected following arg not found after: %s\n",pattern);

					return if_success; // false
				}
				// token !NULL
				else{

					memset(ret,0,sizeof(*ret));
					strcpy(ret,token);
					if_success = true;
					return if_success;
				}
			}

			// 2 - 2: move to next token in the line
			token = strtok(NULL," \t\n");
		}
	}

	return if_success;
}
