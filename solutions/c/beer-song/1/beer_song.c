#include "beer_song.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_LINE_COUNT (299)

void recite(uint8_t start_bottles, uint8_t take_down, char** song) {
    bool underflow = false;
    if (take_down > start_bottles) {
        take_down = start_bottles;
        underflow = true;
    }

    size_t out_index = 0;
    for (uint8_t bottles = start_bottles; bottles > start_bottles - take_down;
         --bottles) {
        const char* ending = bottles == 1 ? "" : "s";
        snprintf(song[out_index++], MAX_LINE_COUNT,
                 "%d bottle%s of beer on the wall, %d bottle%s of beer.",
                 bottles, ending, bottles, ending);
        if (bottles == 1) {
            snprintf(song[out_index++], MAX_LINE_COUNT,
                     "Take it down and pass it around, "
                     "no more bottles of beer on the wall.");
        } else {
            snprintf(song[out_index++], MAX_LINE_COUNT,
                     "Take one down and pass it around, "
                     "%d bottle%s of beer on the wall.",
                     bottles - 1, bottles == 2 ? "" : "s");
        }
        out_index++;  // empty line
    }
    if (underflow) {
        snprintf(
            song[out_index++], MAX_LINE_COUNT,
            "No more bottles of beer on the wall, no more bottles of beer.");
        snprintf(song[out_index++], MAX_LINE_COUNT,
                 "Go to the store and buy some more, "
                 "99 bottles of beer on the wall.");
    }
}
