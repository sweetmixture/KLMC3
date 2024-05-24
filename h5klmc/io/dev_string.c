#include <stdio.h>
#include <string.h>

/*
char* strstr( const char* haystack, const char* needle ); // return needle else NULL
*/


// Function to check if a substring exists within a string
int contains(const char *str, const char *substr) {
    return strstr(str, substr) != NULL;
}

int main() {
    const char *string = "This is a test string containing abc and other characters.";
    const char *substring = "abcd";

	printf("%s\n",contains(string, substring));
	if ( strstr(string, substring) == NULL ){
		printf("NULL\n");
	}

    if (contains(string, substring)) {
        printf("The substring \"%s\" is found in the string.\n", substring);
    } else {
        printf("The substring \"%s\" is not found in the string.\n", substring);
    }

    return 0;
}

