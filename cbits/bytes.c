#include <stdio.h>
#include <string.h>
#include "Rts.h"

int _memcmp(char *a, 
            size_t aoff,
            char *b, 
            size_t boff,
            size_t n) {
    a += aoff;
    b += boff;
    if (a == b) {
        return 0;
    } else {
        return memcmp(a, b, n);
    }
}

int is_byte_array_pinned(unsigned char* p){
    return Bdescr((StgPtr)p)->flags & (BF_PINNED | BF_LARGE);
}
