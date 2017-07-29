#include <stddef.h>

typedef struct {
   size_t    event_counter;
   size_t*   event_queue;
   char**    read_buffer_table;
   size_t*   read_buffer_size_table;
   char**    write_buffer_table;
   size_t*   write_buffer_size_table;
   size_t*   result_table;          
} hs_loop_data;

