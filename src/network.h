#pragma once

extern int net_server(const char *hostname, unsigned port, int udp, const char *keyfile, const char *certfile);
extern int net_connect(const char *hostname, unsigned port, int udp, int nodelay);

extern int net_domain_server(const char *name, int udp);
extern int net_domain_connect(const char *name, int udp);

extern int net_accept(stream *str);
extern void net_set_nonblocking(stream *str);

extern void *net_enable_ssl(int fd, const char *hostname, int server, int level, const char *certfile);
extern size_t net_read(void *ptr, size_t len, stream *str);
extern int net_getline(char **lineptr, size_t *n, stream *str);
extern int net_getc(stream *str);
extern size_t net_write(const void *ptr, size_t nbytes, stream *str);
extern void net_close(stream *str);
