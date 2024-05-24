#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

void splitString(const char *str) {
    size_t len = strlen(str);
    char element[3] = {0};  // buffer to hold element symbols
    char number[10] = {0};  // buffer to hold numbers
    int element_index = 0;
    int number_index = 0;

    for (size_t i = 0; i < len; i++) {
        if (isalpha(str[i])) {
            if (number_index > 0) {
                number[number_index] = '\0';
                printf("%s\n", number);
                number_index = 0;
            }
            if (isupper(str[i])) {
                if (element_index > 0) {
                    element[element_index] = '\0';
                    printf("%s\n", element);
                }
                element_index = 0;
            }
            element[element_index++] = str[i];
        } else if (isdigit(str[i])) {
            if (element_index > 0) {
                element[element_index] = '\0';
                printf("%s\n", element);
                element_index = 0;
            }
            number[number_index++] = str[i];
        }
    }
    if (element_index > 0) {
        element[element_index] = '\0';
        printf("%s\n", element);
    }
    if (number_index > 0) {
        number[number_index] = '\0';
        printf("%s\n", number);
    }
}

int main() {
    const char *input = "Na40Mn40Fe32P72O288";
    splitString(input);
    return 0;
}

