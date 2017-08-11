#include <string.h>
#include "Rts.h"

int hs_memcmp(char *a, size_t aoff, char *b, size_t boff, size_t n);

size_t hs_memchr(char *a, size_t aoff, char b, size_t n);

int is_byte_array_pinned(unsigned char* p);
