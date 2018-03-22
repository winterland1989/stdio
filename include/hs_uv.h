#include <uv.h>

int uv_translate_sys_error(int sys_errno);

typedef struct {
   size_t    event_counter;
   size_t*   event_queue;
   char**    buffer_table;
   size_t*   buffer_size_table;
   ssize_t*  result_table;          
} hs_loop_data;

uv_loop_t* hs_uv_loop_init(size_t siz);
uv_loop_t* hs_uv_loop_resize(uv_loop_t* loop, size_t siz);
void hs_uv_loop_close(uv_loop_t* loop);

uv_handle_t* hs_uv_handle_alloc(uv_handle_type typ);
void hs_uv_handle_free(uv_handle_t* handle);
void hs_uv_handle_close(uv_handle_t* handle);

uv_handle_t* hs_uv_req_alloc(uv_req_type typ);
void hs_uv_req_free(uv_req_t* req);

int hs_uv_read_start(uv_stream_t* stream);
int hs_uv_write(uv_write_t* req, uv_stream_t* handle);


int hs_uv_tcp_open(uv_tcp_t* handle, uv_os_sock_t sock);
int hs_uv_tcp_connect(uv_connect_t* req, uv_tcp_t* handle, const struct sockaddr* addr);


int hs_uv_timer_wake_start(uv_timer_t* handle, uint64_t timeout);
int hs_uv_async_wake_init(uv_loop_t* loop, uv_async_t* async);
