#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <errno.h>

#ifdef _WIN32
#include <winsock2.h>
#define close closesocket
#define ioctl ioctlsocket
#ifdef errno
#undef errno
#endif
#define errno WSAGetLastError()
#ifdef EWOULDBLOCK
#undef EWOULDBLOCK
#endif
#define EWOULDBLOCK WSAEWOULDBLOCK
#else
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <unistd.h>
#endif

#if USE_SSL
#include "openssl/err.h"
#include "openssl/ssl.h"
#endif

#define DEFAULT_CIPHERS "HIGH:!aNULL" /* EECDH+AESGCM:EDH+AESGCM:EECDH+AES256:EDH+AES256 */

#include "internal.h"
#include "network.h"

#if USE_SSL
static int g_ctx_use_cnt = 0;
static SSL_CTX *g_ctx = NULL;
#endif

int net_connect(const char *hostname, unsigned port, int udp, int nodelay, int nonblock)
{
	struct addrinfo hints, *result, *rp;
	int fd, status;

	memset(&hints, 0, sizeof(struct addrinfo));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = udp ? SOCK_DGRAM : SOCK_STREAM;
    hints.ai_flags = hostname ? 0 : AI_PASSIVE;
    char svc[20];
    sprintf(svc, "%u", port);

	if ((status = getaddrinfo(hostname, svc, &hints, &result)) != 0)
		return -1;

	for (rp = result; rp != NULL; rp = rp->ai_next) {
		fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);

		if (fd == -1)
		   continue;

        int flag = 1;
        setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char *)&flag, sizeof(flag));
        int flag2 = 1;
        setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, (char *)&flag2, sizeof(flag2));

		if (connect(fd, rp->ai_addr, rp->ai_addrlen) != -1)
			break;

		close(fd);
	}

	freeaddrinfo(result);

	if (rp == NULL)
		return -1;

	struct linger l;
	l.l_onoff = 0;
	l.l_linger = 1;
	setsockopt(fd, SOL_SOCKET, SO_LINGER, (char*)&l, sizeof(l));
	int flag = 1;
	setsockopt(fd, SOL_SOCKET, SO_KEEPALIVE, (char*)&flag, sizeof(flag));
	flag = nodelay;
	setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (char*)&flag, sizeof(flag));

	if (nonblock) {
		unsigned long flag = 1;
		ioctl(fd, FIONBIO, &flag);
	}

	return fd;
}

int net_server(const char *hostname, unsigned port, int udp, int nonblock, const char *keyfile, const char *certfile)
{
	struct addrinfo hints, *result, *rp;
	int fd, status;

	memset(&hints, 0, sizeof(struct addrinfo));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = udp ? SOCK_DGRAM : SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;
    char svc[20];
    sprintf(svc, "%u", port);

	if ((status = getaddrinfo(NULL, svc, &hints, &result)) != 0) {
		perror("getaddrinfo");
		return -1;
	}

	for (rp = result; rp != NULL; rp = rp->ai_next) {
		fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);

		if (fd == -1)
		   continue;

        int flag = 1;
        setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char *)&flag, sizeof(flag));
        int flag2 = 1;
        setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, (char *)&flag2, sizeof(flag2));

		if (bind(fd, rp->ai_addr, rp->ai_addrlen) == 0)
			break;

		close(fd);
	}

	freeaddrinfo(result);

	if (rp == NULL)
		return -1;

	if (nonblock) {
		unsigned long flag = 1;
		ioctl(fd, FIONBIO, &flag);
	}

	if (udp)
		return fd;

#if USE_SSL
	if (keyfile) {
		if (!g_ctx_use_cnt++) {
			g_ctx = SSL_CTX_new(TLS_server_method());
			//SSL_CTX_set_options(g_ctx, SSL_OP_NO_SSLv3|SSL_OP_NO_SSLv2|SSL_OP_CIPHER_SERVER_PREFERENCE);
			//SSL_CTX_set_cipher_list(g_ctx, DEFAULT_CIPHERS);
		}

		if (!SSL_CTX_use_PrivateKey_file(g_ctx, keyfile, SSL_FILETYPE_PEM)) {
			printf("SSL load private key failed: %s\n", keyfile);
			close(fd);
			return 0;
		}

		if (!SSL_CTX_use_certificate_file(g_ctx, !certfile?keyfile:certfile, SSL_FILETYPE_PEM)) {
			printf("SSL load certificate failed: %s\n", !certfile?keyfile:certfile);
			close(fd);
			return 0;
		}

		SSL_CTX_load_verify_locations(g_ctx, !certfile?keyfile:certfile, NULL);
		SSL_CTX_set_default_verify_paths(g_ctx);
	}
#endif

	listen(fd, -1);
	return fd;
}

int net_accept(stream *str)
{
	struct sockaddr_in addr = {0};
	socklen_t len = 0;
	int fd = accept(fileno(str->fp), (struct sockaddr*)&addr, &len);

	if ((fd == -1) && ((errno == EWOULDBLOCK) || (errno == EAGAIN)))
		return -1;

	struct linger l;
	l.l_onoff = 0;
	l.l_linger = 1;
	setsockopt(fd, SOL_SOCKET, SO_LINGER, (char*)&l, sizeof(l));
	int flag = 1;
	setsockopt(fd, SOL_SOCKET, SO_KEEPALIVE, (char*)&flag, sizeof(flag));
	flag = str->nodelay;
	setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (char*)&flag, sizeof(flag));
	return fd;
}

void net_set_nonblocking(stream *str)
{
	unsigned long flag = 1;
	ioctl(fileno(str->fp), FIONBIO, &flag);
}

#if USE_SSL
void *net_enable_ssl(int fd, const char *hostname, int is_server, int level, const char *certfile)
{
	if (!g_ctx_use_cnt++) {
		g_ctx = SSL_CTX_new(is_server?TLS_server_method():TLS_client_method());
		//SSL_CTX_set_options(g_ctx, SSL_OP_NO_SSLv3|SSL_OP_NO_SSLv2);
		//SSL_CTX_set_cipher_list(g_ctx, DEFAULT_CIPHERS);
	}

	SSL *ssl = SSL_new(g_ctx);
	SSL_set_ssl_method(ssl, is_server?TLS_server_method():TLS_client_method());
	//SSL_set_mode(ssl, SSL_MODE_AUTO_RETRY);
	//SSL_set_verify(ssl, SSL_VERIFY_NONE, 0);

	if (!is_server && certfile) {
		if (!SSL_CTX_use_certificate_file(g_ctx, certfile, SSL_FILETYPE_PEM)) {
			printf("SSL load certificate failed\n");
			close(fd);
			return NULL;
		}

		SSL_CTX_set_default_verify_paths(g_ctx);
		int level = 0;

		if (level > 0)
			SSL_set_verify(ssl, SSL_VERIFY_PEER|SSL_VERIFY_FAIL_IF_NO_PEER_CERT, 0);
	}

	SSL_set_fd(ssl, fd);

	if (is_server) {
		if (SSL_accept(ssl) == -1) {
			fprintf(stderr, "SSL_accept failed\n");
			ERR_print_errors_fp(stderr);
			SSL_free(ssl);
			return NULL;
		}
	} else {
        SSL_set_tlsext_host_name(ssl, hostname);

		if (SSL_connect(ssl) <= 0) {
			fprintf(stderr, "SSL_connect failed\n");
			ERR_print_errors_fp(stderr);
			SSL_free(ssl);
			return NULL;
		}
	}

	return ssl;
}

size_t ssl_write(const void *ptr, size_t nbytes, stream *str)
{
	return SSL_write((SSL*)str->sslptr, ptr, nbytes);
}

size_t ssl_read(void *ptr, size_t len, stream *str)
{
	char *dst = ptr;

	while (len && str->srclen) {
		*dst++ = *str->src++;
		str->srclen--;
		len--;
	}

	if (dst != ptr) {
		return dst - (char*)ptr;
	}

	return SSL_read((SSL*)str->sslptr, ptr, len);
}

int ssl_getline(char **lineptr, size_t *n, stream *str)
{
	if (!*lineptr)
		*lineptr = malloc(*n=1024);

	char *dst = *lineptr;
	size_t dstlen = *n;
	int done = 0;

	while (!done) {
		if (str->srclen <= 0) {
			int rlen = SSL_read((SSL*)str->sslptr, str->srcbuf, STREAM_BUFLEN);

			if (rlen <= 0)
				return -1;

			str->srcbuf[rlen] = '\0';
			str->src = str->srcbuf;
			str->srclen = rlen;
		}

		while (str->srclen-- > 0) {
			int ch = *str->src++;
			*dst++ = ch;

			if (dstlen-- <= 1) {
				size_t savelen = dst - *lineptr;
				*n *= 2;
				*lineptr = realloc(*lineptr, *n);
				dst = *lineptr + savelen;
				dstlen = *n - savelen;
			}

			if (ch == '\n') {
				*dst = '\0';
				done = 1;
				break;
			}
		}
	}

	return dst - *lineptr;
}

void ssl_close(stream *str)
{
	SSL_shutdown((SSL*)str->sslptr);
	SSL_free((SSL*)str->sslptr);

	if (!--g_ctx_use_cnt)
		SSL_CTX_free(g_ctx);
}
#endif
