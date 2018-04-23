#include <uv.h>
#include "hs_uv.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

/********************************************************************************/

// initialize a loop with its data to give slot size. return NULL on fail.
uv_loop_t* hs_uv_loop_init(size_t counter){
    int r;

    uv_loop_t* loop = malloc(sizeof(uv_loop_t));

    if ( uv_loop_init(loop) < 0 ) {
        free(loop);
        return NULL;
    } else {
        loop->data = (void*)counter;
        return loop;
    }
}
void hs_uv_handle_free(uv_handle_t* handle) { free(handle); };

void hs_uv_walk_close_cb(uv_handle_t* handle, void* arg){
    uv_close(handle, hs_uv_handle_free);
}

// This function close all the handles live on that loop and the loop itself,
// then release all the memory.
// https://stackoverflow.com/questions/25615340/closing-libuv-handles-correctly
//
void hs_uv_loop_close(uv_loop_t* loop){
    uv_stop(loop);
    uv_walk(loop, hs_uv_walk_close_cb, NULL);
    uv_run(loop, UV_RUN_NOWAIT);
    while(uv_loop_close(loop) == UV_EBUSY);
    free(loop);
}

/********************************************************************************/

// Initialize a uv_handle_t with give type, and attach a new hs_context_data struct to its data field, return NULL on fail.
uv_handle_t* hs_uv_handle_alloc(uv_handle_type typ){
    hs_context_data * data = malloc(sizeof(hs_context_data ));
    if (data == NULL) return NULL;

    uv_handle_t* handle = malloc(uv_handle_size(typ));
    if (handle == NULL) return NULL;

    handle->data = data;
    return handle;
}

// Close a uv_handle_t, free its memory.
void hs_uv_handle_close(uv_handle_t* handle){
    free(handle->data);
    uv_close(handle, hs_uv_handle_free);
}

// Get handle's OS file
int hs_uv_fileno(uv_handle_t* handle){
    uv_os_fd_t fd;
    int r;
    r = uv_fileno(handle, &fd);
    if (r < 0) { return r; } else { return (int)fd; }
}

// Initialize a uv_req_t with give type, return NULL on fail.
uv_req_t* hs_uv_req_alloc(uv_req_type typ){
    hs_context_data * data = malloc(sizeof(hs_context_data ));
    if (data == NULL) return NULL;

    uv_req_t* req = malloc(uv_req_size(typ));
    if (req == NULL) return NULL;

    req->data = data;
    return req;
}

// free uv_req_t's memory.
void hs_uv_req_free(uv_req_t* req){
    free(req->data);
    free(req);
}


/********************************************************************************/

void hs_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf){
    hs_context_data* data = (hs_context_data*)handle->data;
    buf->base = data->buffer;      // fetch buffer_table from buffer_table table
    buf->len = data->buffer_siz;  // we ignore suggested_size completely
}

void hs_read_cb (uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf){
    if (nread != 0) {
        hs_context_data* data = (hs_context_data*)stream->data;
        data->buffer_siz = nread;                   // save the read result_table,
                                                    // > 0 in case of success, < 0 otherwise.
        uv_read_stop(stream);
        hs_try_putmvar(data->cap, data->mvar);
    }
}

int hs_uv_read_start(uv_stream_t* stream){
    return uv_read_start(stream, hs_alloc_cb, hs_read_cb);
}

void hs_write_cb(uv_write_t* req, int status){
    hs_context_data* data = (hs_context_data*)req->data;
    data->buffer_siz = (ssize_t)status;                   // 0 in case of success, < 0 otherwise.
    hs_try_putmvar(data->cap, data->mvar);
}

int hs_uv_write(uv_write_t* req, uv_stream_t* handle){
    hs_context_data* data = (hs_context_data*)req->data;

    // on windows this struct is captured by WSASend
    // on unix this struct is copied by libuv's uv_write
    // so it's safe to allocate it on stack
    uv_buf_t buf = { 
        .base = data->buffer,
        .len = data->buffer_siz
    };
    
    return uv_write(req, handle, &buf, 1, hs_write_cb);    // we never use writev: we do our own
                                                           // user-space buffering in haskell.
}

/********************************************************************************/

/* on windows uv_tcp_open doesn't work propery for sockets that are not
 * connected or accepted by libuv because the lack of some state initialization,
 * so we do it by manually set those flags
 *
 * referenes:   https://github.com/libuv/libuv/issues/397
 *              https://github.com/libuv/libuv/pull/1150
 */
#if defined(_WIN32)
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

int hs_uv_tcp_open(uv_tcp_t* handle, int sock) {
  int r = uv_tcp_open(handle, (uv_os_sock_t)sock);
  if (r == 0) {
    uv_connection_init((uv_stream_t*)handle);
    handle->flags |= UV_HANDLE_BOUND | UV_HANDLE_READABLE | UV_HANDLE_WRITABLE;
  }
  return r;
}
#else
int hs_uv_tcp_open(uv_tcp_t* handle, int sock) {
  return uv_tcp_open(handle, (uv_os_sock_t)sock);
}
#endif

void hs_connect_cb(uv_connect_t* req, int status){
    size_t slot = (size_t)req->data;
    hs_context_data* data = (hs_context_data*)req->data;

    data->buffer_siz = status;                  // 0 in case of success, < 0 otherwise.
    hs_try_putmvar(data->cap, data->mvar);
}

int hs_uv_tcp_connect(uv_connect_t* req, uv_tcp_t* handle, const struct sockaddr* addr){
    return uv_tcp_connect(req, handle, addr, hs_connect_cb);
}

int32_t hs_uv_accept(uv_stream_t* server) {
    if (server->accepted_fd == -1)
        return UV_EAGAIN;

    int32_t fd = (int32_t)server->accepted_fd;
    server->accepted_fd = -1;

    return fd;

}

void hs_uv_listen_resume(uv_stream_t* stream){
    uv__io_start(stream->loop, &stream->io_watcher, POLLIN);
}

void hs_listen_cb(uv_stream_t* server, int status){
    hs_context_data* data = (hs_context_data*)server->data;
    int32_t* accepte_buf = (int32_t*)data->buffer;
    if (accepte_buf != NULL) {
        if (status < 0){ // 0 in case of success, < 0 otherwise.
            accepte_buf[data->buffer_siz] = (int32_t)status;
        } else {
            accepte_buf[data->buffer_siz] = hs_uv_accept(server);
        }
        data->buffer_siz = data->buffer_siz + 1;         
    }
}

int hs_uv_listen(uv_stream_t* stream, int backlog){
    return uv_listen(stream, backlog, hs_listen_cb);
}

void hs_accept_check_cb(uv_check_t* check){
    hs_context_data* check_data= (hs_context_data*)check->data;
    uv_stream_t* server=(uv_stream_t*)check_data->buffer;
    hs_context_data* server_data=(hs_context_data*)server->data;

    if (server_data->buffer_siz > 0 && server_data->buffer != NULL){
        server_data->buffer = NULL;
        hs_try_putmvar(check_data->cap, check_data->mvar);
        uv__io_stop(server->loop, &server->io_watcher, POLLIN);
    }
}

int hs_uv_accept_check_init(uv_loop_t* loop, uv_check_t* check, uv_handle_t* server){
    int r = uv_check_init(loop, check);
    hs_context_data* data = (hs_context_data*)check->data;
    data->buffer = (char*)server;    // we link server to the buffer field
    if (r < 0) return r;
    return uv_check_start(check, hs_accept_check_cb);
}


/********************************************************************************/

// a empty callback for wake up uv_run
void uv_timer_wake_cb(uv_timer_t* handle){}

// a timer handler whose sole purpose is to break current uv_run from other thread
// this is used with none-threaded GHC rts.
int hs_uv_timer_wake_start(uv_timer_t* handle, uint64_t timeout){
    return uv_timer_start(handle, uv_timer_wake_cb, timeout, timeout);
}

// a async handler whose sole purpose is to break current uv_run from other thread
// this is used with multi-threaded GHC rts.
int hs_uv_async_wake_init(uv_loop_t* loop, uv_async_t* async){
    return uv_async_init(loop, async, NULL);
}
