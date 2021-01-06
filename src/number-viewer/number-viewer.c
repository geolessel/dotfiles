#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "number-viewer.h"

void printWelcome(void);

int main(void) {
    char buffer[BUFFER_SIZE];
    char running = TRUE;
    
    printWelcome();

    while (running) {
        printf(PROMPT);
        fgets(buffer, BUFFER_SIZE, stdin);
        if (strcmp(buffer, "q\n") == 0) {
            running = FALSE;
            break;
        } else {
            uint64_t number;

            if (buffer[0] == '0' && buffer[1] == 'x') {
                number = htoll(buffer);
            } else {
                number = atoll(buffer);
            }

            toBinaryString(buffer, number);
            printf("%5s | %llu\n", "DEC", number);
            printf("%5s | 0x%llX\n", "HEX", number);
            printf("%5s | %s\n", "BIN", buffer);
            printf("\n");
        }
    }

    printf("\n");
}

void printWelcome(void) {
    printf("\n");
    printf("*         NUMBER VIEWER\n");
    printf("* ===================================\n");
    printf("*     123 for decimal numbers\n");
    printf("*   0x123 for hexidecimal numbers\n");
    printf("*       q to quit\n");
    printf("\n");
}
