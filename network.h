#pragma once

int net_server(const char *hostname, unsigned port, int udp, const char *keyfile, const char *certfile);
int net_accept(stream *str);
int net_connect(const char *hostname, unsigned port, int udp, int nodelay);
void net_set_nonblocking(stream *str);

void *net_enable_ssl(int fd, const char *hostname, int server, int level, const char *certfile);
size_t net_read(void *ptr, size_t len, stream *str);
int net_getline(char **lineptr, size_t *n, stream *str);
size_t net_write(const void *ptr, size_t nbytes, stream *str);
void net_close(stream *str);
