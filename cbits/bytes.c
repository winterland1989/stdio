#include <stdio.h>

void reverse(unsigned char *q, size_t *p, size_t n) {
    p += n-1;
    while (n-- != 0) *q++ = *p--;
}
