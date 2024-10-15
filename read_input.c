/*
		Author:			Woongkyu Jee / woong.jee.16@ucl.ac.uk
		Affiliation:	University College London
		Date:			2023.05.25 - 

		Description:
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include <ctype.h>

/* * *
	Utilities 
* * */

bool isInteger( const char* str ){
	char *endptr;
	
	while(isspace((unsigned char)*str)) str++; // skip heading whitespace
	if(*str == '+' || *str == '-') str++; // allow optional leading signs
	strtol(str,&endptr,10); // base '10' integer conversion

	return *endptr == '\0'; // condition conversion success, (i.e. if conversion is successful, enptr will be moved to the end, as '\0'
}

bool isFloat(const char* str){
	char *endptr;
	
	while(isspace((unsigned char)*str)) str++; // skip heading whitespace
	if(*str == '+' || *str == '-') str++; // allow optional leading signs
	strtod(str, &endptr); // strtod will parse the number, including scientific notation
	
	return *str != '\0' && *endptr == '\0';
}	

bool convertToInteger(const char* str, int* outValue){
	char* endptr;

	while(isspace((unsigned char)*str)) str++;
	if(*str == '+' || *str == '-') str++;

	int value = (int)strtol(str,&endptr,10);

	if(*str != '\0' && *endptr == '\0' ){
		*outValue = value;
		return true;
	} else {
		return false;
	}
}
bool convertToFloat(const char* str, double* outValue){
	char *endptr;

	while(isspace((unsigned char)*str)) str++;
	if(*str == '+' || *str == '-') str++;

	double value = strtod(str, &endptr);

	if(*str != '\0' && *endptr == '\0'){
		*outValue = value;
		return true;
	}else{
		return false;
	}
}	

/* * *
	functions in 'read_input.h'
* * */

// <int> pattern finder
void read_input_ipatternfinder(
	FILE* fp,						// IN
	const char* pattern,			// IN
	int* ret						// IN-OUT
){

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
					// supposed to print error message?
					// dev tmp
					// printf("ipattern expected following arg not found after: %s\n",pattern);
					return;
				}
				// token !NULL
				else{
					// token !<int>
					if( !isInteger(token) ){
						// dev tmp
						// printf("ipattern expected following arg is not an integer: %s\n",pattern);
						return;
					}
					// token <int>
					else{
						*ret = (int)strtol(token,NULL,10);
						return;
					}
				}
			}
			// 2 - 2: move to next token in the line
			token = strtok(NULL," \t\n");
		}
	}
	return;
}


// <char*> pattern finder
void read_input_spatternfinder( FILE* fp, const char* pattern, char* ret ){

	char  line[1024];
	char* token = NULL;

	int line_number = 0;

	fseek( fp, 0, SEEK_SET ); // back to the file front

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
					// printf("spattern: expected following arg not found after: %s\n",pattern);
					return;
				}
				// token !NULL
				else{

					memset(ret,0,sizeof(*ret));
					strcpy(ret,token);
					return;
				}
			}

			// 2 - 2: move to next token in the line
			token = strtok(NULL," \t\n");
		}
	}

	return;
}


/* * * * * *
 KLMC Output Extractor? 23.05.2024 WKJEE
 * * * * * */
