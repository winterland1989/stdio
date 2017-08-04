#include <uv.h>
#include <stdio.h>
#include "uv_hs.h"
#include <assert.h>
#include <stdlib.h>


/********************************************************************************/

// initialize a loop with its data to give slot size. return NULL on fail.
uv_loop_t* hs_loop_init(size_t siz){

    uv_loop_t* loop = malloc(sizeof(uv_loop_t));
    uv_loop_init(loop);

    hs_loop_data* loop_data = malloc(sizeof(hs_loop_data));
    if (loop_data == NULL) return NULL; 
        
    size_t* event_queue = malloc(siz*sizeof(size_t));
    if (event_queue == NULL) return NULL;

    char** read_buffer_table = malloc(siz*sizeof(char*));
    if (read_buffer_table == NULL) return NULL;

    size_t* read_buffer_size_table = malloc(siz*sizeof(size_t));
    if (read_buffer_size_table == NULL) return NULL;

    char** write_buffer_table = malloc(siz*sizeof(char*));
    if (write_buffer_table == NULL) return NULL; 

    size_t* write_buffer_size_table = malloc(siz*sizeof(size_t));
    if (write_buffer_size_table == NULL) return NULL;

    size_t* result_table = malloc(siz*sizeof(size_t));
    if (result_table == NULL) return NULL;

    loop_data->event_queue             = event_queue;
    loop_data->read_buffer_table       = read_buffer_table;
    loop_data->read_buffer_size_table  = read_buffer_size_table;
    loop_data->write_buffer_table      = write_buffer_table;
    loop_data->write_buffer_size_table = write_buffer_size_table;
    loop_data->result_table            = result_table;

    loop->data = loop_data;
    return loop;
}

// resize a loop's data to given slot size, return NULL on fail.
uv_loop_t* hs_loop_resize(uv_loop_t* loop, size_t siz){

    hs_loop_data* loop_data = loop->data;
    size_t* event_queue_new             = realloc(loop_data->event_queue, (siz*sizeof(size_t)));
    if (event_queue_new == NULL) return NULL; 
    char** read_buffer_table_new        = realloc(loop_data->read_buffer_table, (siz*sizeof(char*)));
    if (read_buffer_table_new == NULL) return NULL; 
    size_t* read_buffer_size_table_new  = realloc(loop_data->read_buffer_size_table, (siz*sizeof(size_t)));
    if (read_buffer_size_table_new == NULL) return NULL; 
    char** write_buffer_table_new       = realloc(loop_data->write_buffer_table, (siz*sizeof(char*)));
    if (write_buffer_table_new == NULL) return NULL; 
    size_t* write_buffer_size_table_new = realloc(loop_data->write_buffer_size_table, (siz*sizeof(size_t)));
    if (write_buffer_size_table_new == NULL) return NULL; 
    size_t* result_table_new            = realloc(loop_data->result_table, (siz*sizeof(size_t)));
    if (result_table_new == NULL) return NULL; 

    loop_data->event_queue             = event_queue_new;
    loop_data->read_buffer_table       = read_buffer_table_new;
    loop_data->read_buffer_size_table  = read_buffer_size_table_new;
    loop_data->write_buffer_table      = write_buffer_table_new;
    loop_data->write_buffer_size_table = write_buffer_size_table_new;
    loop_data->result_table            = result_table_new;

    return loop;
}

// This function release all the memory related to a uv_loop_t, it could block.
void hs_loop_close(uv_loop_t* loop){
    while(uv_loop_close(loop) == UV_EBUSY);

    hs_loop_data* loop_data = loop->data;
    free(loop);
    free(loop_data->event_queue);
    free(loop_data->read_buffer_table);
    free(loop_data->read_buffer_size_table);
    free(loop_data->write_buffer_table);
    free(loop_data->write_buffer_size_table);
    free(loop_data->result_table);
}

// Initialize a uv_handle_t with give type, return NULL on fail.
uv_handle_t* hs_handle_init(uv_handle_type typ){
    return malloc(uv_handle_size(typ));
}

void hs_free_handle_callback(uv_handle_t* handle){
    free(handle);
}

// Close a uv_handle_t, free its memory.
void hs_handle_close(uv_handle_t* handle){
    uv_close(handle, hs_free_handle_callback);
}


/********************************************************************************/

void hs_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf){
    size_t slot = (size_t)handle->data;
    hs_loop_data* loop_data = handle->loop->data;
    buf->base = loop_data->read_buffer_table[slot];      // fetch buffer from buffer table
    buf->len = loop_data->read_buffer_size_table[slot];  // we ignore suggested_size completely
}

void hs_read_cb (uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf){
    size_t slot = (size_t)stream->data;
    hs_loop_data* loop_data = stream->loop->data;
    loop_data->result_table[slot] += nread;                        // save the read result
    loop_data->event_queue[loop_data->event_counter] = slot; // push the slot to event queue
    loop_data->event_counter += 1;
    uv_read_stop(stream);
}

int hs_read_start(uv_stream_t* stream){
    return uv_read_start(stream, hs_alloc_cb ,hs_read_cb);
}

void hs_write_cb(uv_write_t* req, int status){
    size_t slot = (size_t)req->data;
    hs_loop_data* loop_data = req->handle->loop->data;
    loop_data->event_queue[loop_data->event_counter] = -slot;   // push the slot to event queue, negative is write
    loop_data->event_counter += 1;
    free(req);
}

int hs_write(uv_stream_t* handle){
    uv_write_t* req = malloc(sizeof(uv_write_t));
    uv_buf_t* buf = malloc(sizeof(uv_buf_t));
    hs_loop_data* loop_data = handle->loop->data;
    size_t slot = (size_t)handle->data;

    req->data = handle->data;
    buf->base = loop_data->write_buffer_table[slot];
    buf->len = loop_data->write_buffer_size_table[slot];

    uv_write(req, handle, buf, 1, hs_write_cb);
}


/* on windows uv_tcp_open doesn't work propery for sockets that are not
 * connected or accepted by libuv because the lack of some state initialization,
 * so we do it by manually set those flags
 *
 * referenes:   https://github.com/libuv/libuv/issues/397
 *              https://github.com/libuv/libuv/pull/1150
 */
#if defined(mingw32_HOST_OS)
#define UV_HANDLE_READING                       0x00000100
#define UV_HANDLE_BOUND                         0x00000200
#define UV_HANDLE_LISTENING                     0x00000800
#define UV_HANDLE_CONNECTION                    0x00001000
#define UV_HANDLE_READABLE                      0x00008000
#define UV_HANDLE_WRITABLE                      0x00010000

void uv_connection_init(uv_stream_t* handle){
  handle->flags |= UV_HANDLE_CONNECTION;
  handle->stream.conn.write_reqs_pending = 0;
  (&handle->read_req)->type = UV_READ;                                                        \
  (&handle->read_req)->u.io.overlapped.Internal = 0;  /* SET_REQ_SUCCESS() */ 
  handle->read_req.event_handle = NULL;
  handle->read_req.wait_handle = INVALID_HANDLE_VALUE;
  handle->read_req.data = handle;
  handle->stream.conn.shutdown_req = NULL;
}

int hs_tcp_open_win32(uv_tcp_t* handle, uv_os_sock_t sock) {
  int r = uv_tcp_open(handle, sock);
  if (r == 0) {
    uv_connection_init((uv_stream_t*)handle);
    handle->flags |= UV_HANDLE_BOUND | UV_HANDLE_READABLE | UV_HANDLE_WRITABLE;
  }

  return r;
}
#endif

void hs_connection_cb(uv_stream_t* server, int status){
    if (status == 0){
        size_t slot = (size_t)server->data;
        hs_loop_data* loop_data = server->loop->data;
        loop_data->result_table[slot] = status;                  // TODO get the errno and pass to ghc
        loop_data->event_queue[loop_data->event_counter] = slot; // push the slot to event queue
        loop_data->event_counter += 1;
    }
}

int hs_listen(uv_stream_t* stream, int backlog){
    uv_listen(stream, backlog, hs_connection_cb);
}

/********************************************************************************/

void hs_poll_cb(uv_poll_t* handle, int status, int events){
    if (status == 0){
        size_t slot = (size_t)handle->data;
        hs_loop_data* loop_data = handle->loop->data;
        loop_data->result_table[slot] = events;                  // TODO get the errno and pass to ghc
        loop_data->event_queue[loop_data->event_counter] = slot; // push the slot to event queue
        loop_data->event_counter += 1;
    }
}

int hs_poll_start(uv_poll_t* handle, int events){
    uv_poll_start(handle, events, hs_poll_cb);
}

/********************************************************************************/

void uv_timer_cb_stop_loop(uv_timer_t* handle){
    uv_stop(handle->loop);
}

int hs_timer_start_stop_loop(uv_timer_t* handle, uint64_t timeout){
    return uv_timer_start(handle, uv_timer_cb_stop_loop, timeout, 0);
}

/********************************************************************************/

int hs_async_init_no_callback(uv_loop_t* loop, uv_async_t* async){
    return uv_async_init(loop, async, NULL);
}


void hs_fs_cb(uv_fs_t* req){
    hs_loop_data* d = req->loop->data;
}

int uv_fs_open_hs(uv_loop_t* loop, uv_fs_t* req, const char* path, int flags, int mode){
    uv_fs_open(loop, req, path, flags, mode, hs_fs_cb);
}
