#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>

bool isFloat(const char* str) {
    char *endptr;

    // Skip leading whitespace
    while (isspace((unsigned char)*str)) str++;

    // Allow optional sign
    if (*str == '+' || *str == '-') str++;

    // strtod will parse the number, including scientific notation
    strtod(str, &endptr);

    // Check if the end pointer is at the end of the string
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

bool convertToFloat(const char* str, double* outValue) {
    char *endptr;

    // Skip leading whitespace
    while (isspace((unsigned char)*str)) str++;

    // Allow optional sign
    if (*str == '+' || *str == '-') str++;

    double value = strtod(str, &endptr);

    // Check if the end pointer is at the end of the string
    if (*str != '\0' && *endptr == '\0') {
        *outValue = value;
        return true;
    } else {
        return false;
    }
}

int main() {
    const char *test1 = "+1.312e-5";
    const char *test2 = "-1.323E-6";
    const char *test3 = "1.321";
    const char *test4 = "123";
    const char *test5 = "abc";

    double value;

    printf("\"%s\" is a float: %s\n", test1, isFloat(test1) ? "true" : "false");
    if (convertToFloat(test1, &value)) {
        printf("Converted value: %f\n", value);
    } else {
        printf("Failed to convert \"%s\" to a float.\n", test1);
    }

    printf("\"%s\" is a float: %s\n", test2, isFloat(test2) ? "true" : "false");
    if (convertToFloat(test2, &value)) {
        printf("Converted value: %f\n", value);
    } else {
        printf("Failed to convert \"%s\" to a float.\n", test2);
    }

    printf("\"%s\" is a float: %s\n", test3, isFloat(test3) ? "true" : "false");
    if (convertToFloat(test3, &value)) {
        printf("Converted value: %f\n", value);
    } else {
        printf("Failed to convert \"%s\" to a float.\n", test3);
    }

    printf("\"%s\" is a float: %s\n", test4, isFloat(test4) ? "true" : "false");
    if (convertToFloat(test4, &value)) {
        printf("Converted value: %f\n", value);
    } else {
        printf("Failed to convert \"%s\" to a float.\n", test4);
    }

    printf("\"%s\" is a float: %s\n", test5, isFloat(test5) ? "true" : "false");
    if (convertToFloat(test5, &value)) {
        printf("Converted value: %f\n", value);
    } else {
        printf("Failed to convert \"%s\" to a float.\n", test5);
    }

	char* str = "asdfasdfasfd";
    char* endptr;

    while(isspace((unsigned char)*str)) str++; // skip heading whitespace
    if(*str == '+' || *str == '-') str++; // allow optional leading signs

	printf(" ----------------\n");
	printf(" * try conversion\n");
    strtol(str,&endptr,10); // base '10' integer conversion

	if( *endptr == '\0' ){
		printf("succuess\n");
	} else {
		printf("fail\n");
	}
	
/*
    const char *test3 = "1.321";
    const char *test4 = "123";
*/
	int ivalue;
	if (convertToInteger(test4,&ivalue)) {	// success
		printf("Converted value: %i\n", ivalue);
	} else {
		printf("Failed to convert \"%s\" to a Integer.\n", test4);
	}
	if (convertToInteger(test3,&ivalue)) { // fail (number with decimal places)
		printf("Converted value: %i\n", ivalue);
	} else {
		printf("Failed to convert \"%s\" to a Integer.\n", test4);
	}

   return 0;
}







