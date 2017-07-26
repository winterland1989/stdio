#include <uv.h>
#include <stdio.h>
#include "uv_hs.h"
#include <assert.h>


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


int hs_timer_start_no_callback(uv_timer_t* handle, uint64_t timeout){
    return uv_timer_start(handle, NULL, timeout, 0);
}

void uv_timer_cb_stop_loop(uv_timer_t* handle){
    uv_stop(handle->loop);
}

int hs_timer_start_stop_loop(uv_timer_t* handle, uint64_t timeout){
    return uv_timer_start(handle, uv_timer_cb_stop_loop, timeout, 0);
}

int hs_async_init_no_callback(uv_loop_t* loop, uv_async_t* async){
    return uv_async_init(loop, async, NULL);
}

void dummy_signal_cb(uv_signal_t* handle, int signum){
    uv_signal_stop(handle);
}

int hs_signal_start_no_callback(uv_signal_t* signal, int signum){
    return uv_signal_start(signal, dummy_signal_cb, signum);
}

void hs_fs_cb(uv_fs_t* req){
    hs_loop_data* d = req->loop->data;
}

int uv_fs_open_hs(uv_loop_t* loop, uv_fs_t* req, const char* path, int flags, int mode){
    uv_fs_open(loop, req, path, flags, mode, hs_fs_cb);
}
