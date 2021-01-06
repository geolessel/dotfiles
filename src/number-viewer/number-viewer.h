#ifndef __NUMBER_VIEWER_H_
#define __NUMBER_VIEWER_H_

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdint.h>

#define FALSE 0
#define TRUE  1
#define PROMPT ("number> ")
#define BUFFER_SIZE 1023

/*
** Convert an arbitrarily-long decimal number
** into a binary string.
*/
void toBinaryString(char modifiedString[], uint64_t const number) {
    int i, byte;
    uint64_t maxByte = 1;
    int stringIndex = 0;
    uint64_t max;

    if (number >= UINT64_MAX)
        maxByte = 8;
    else {
        while (1) {
            max = (uint64_t) (1ull << (maxByte * 8ull)) - 1ull;
            if (max >= number || (maxByte > 0 && max == 0)) {
                break;
            }
            maxByte++;
        }
    }

    for (byte = maxByte; byte > 0; byte--) {
        for (i = 0; i < 8; i++) {

            // 111111
            // 54321098 76543210
            // 00000001 00000000

            uint64_t mask = (uint64_t) 1llu << ((byte * 8) - 1 - i);
            if (number & mask)
                modifiedString[stringIndex++] = '1';
            else
                modifiedString[stringIndex++] = '0';
        }

        modifiedString[stringIndex++] = ' ';
    }

    modifiedString[stringIndex++] = '\0';
}

/*
** Convert a hex string into a long long
*/
uint64_t htoll(char const string[]) {
    uint64_t acc = 0;
    unsigned int i;
    unsigned int raiseBy = strlen(string) - 3; // -3 to strip 0x and \n
    
    // start at 2 to skip the "0x"
    for (i = 2; string[i] != '\0' && string[i] != '\n'; i++) {
        int adder = 0;

        if (string[i] >= 'A' && string[i] <= 'F') {
            adder = string[i] - 55;
        } else if (string[i] >= 'a' && string[i] <= 'f') {
            adder = string[i] - 87;
        } else {
            adder = string[i] - 48;
        }

        acc += (adder * (uint64_t)pow(16, --raiseBy));
    }

    return acc;
}

#endif // __NUMBER_VIEWER_H_
