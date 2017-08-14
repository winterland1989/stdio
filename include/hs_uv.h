#include <stddef.h>

typedef struct {
   size_t    event_counter;
   size_t*   event_queue;
   char**    buffer_table;
   size_t*   buffer_size_table;
   ssize_t*   result_table;          
} hs_loop_data;

