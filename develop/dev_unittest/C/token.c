#include <stdio.h>
#include <string.h>

int main() {

    FILE* fp = fopen("fruit.txt","r");
    char line[100];
    char *token;

    while(fgets(line,sizeof(line),fp) != NULL){

	token = strtok(line," \t\n");
	while( token != NULL ){

		if( strcmp(token,"#") == 0 ){
			break;
		}
		printf("%s\n",token);

		if( strcmp(token,"banana") == 0 ){
			printf("%s\n",token);
		}

	

		token = strtok(NULL, " \t\n");
	}
    }


    fclose(fp);

    return 0;
}
