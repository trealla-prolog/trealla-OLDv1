#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <ctype.h>
#include <wctype.h>
#include <wchar.h>

/*
 * This allows supplying a getter function...
 */

extern int xgetc_utf8(int(*fn)(), void*);

/*
 *  These relate to similar stdc functions...
 */

static inline int isspace_utf8(int ch) { return iswspace(ch); }
static inline int isalpha_utf8(int ch) { return iswalpha(ch); }
static inline int isalnum_utf8(int ch) { return iswalnum(ch); }
static inline int isupper_utf8(int ch) { return iswupper(ch); }
static inline int islower_utf8(int ch) { return iswlower(ch); }
static inline int toupper_utf8(int ch) { return towupper(ch); }
static inline int tolower_utf8(int ch) { return towlower(ch); }

static inline int getc_utf8(FILE *fp) { return xgetc_utf8(fgetc, fp); }
static inline int fgetc_utf8(FILE *fp) { return xgetc_utf8(fgetc, fp); }

extern size_t strlen_utf8(const char *s);						// returns #chars
extern size_t substrlen_utf8(const char *s, const char *end);	// returns #chars
extern const char *strchr_utf8(const char *s, int ch);
extern const char *strrchr_utf8(const char *s, int ch);

extern int readc_utf8(int fd, int *ch);

/*
 *  These just get/put a memory buffer...
 */

extern int get_char_utf8(const char **src);
extern int peek_char_utf8(const char *src);
extern int put_char_utf8(char *dst, int ch);					// returns #bytes
extern int put_char_bare_utf8(char *dst, int ch);				// returns #bytes
extern int put_len_utf8(int ch);								// returns #bytes
extern bool is_char_utf8(const char *src);
extern size_t len_char_utf8(const char *src);					// returns #bytes
