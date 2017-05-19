#include <stdio.h>
#include <string.h>
#include "Rts.h"

void reverse(unsigned char *dest, unsigned char *src, size_t n) {
    src += n-1;
    while (n-- != 0) *dest++ = *src--;
}

void intersperse(unsigned char *dest,
                 unsigned char *src,
                 size_t offset,
                 size_t len,
                 unsigned char c) {
    src += offset;
    while (len > 1) {
        *dest++ = *src++;
        *dest++ = c;
        len--;
    }
    if (len == 1)
        *dest = *src;
}

int _memcmp(const void *a, 
            size_t aoff,
            const void *b, 
            size_t boff,
            size_t n) {
    return memcmp(a + aoff, b + boff, n);
}

int is_byte_array_pinned(unsigned char* p){
    return Bdescr(p)->flags & BF_PINNED;
}
