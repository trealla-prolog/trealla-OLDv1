#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <math.h>
#include <float.h>
#include <assert.h>

#ifdef _WIN32
#define snprintf _snprintf
#endif

#include "internal.h"
#include "builtins.h"
#include "network.h"
#include "utf8.h"

#ifndef DBL_DECIMAL_DIG
#define DBL_DECIMAL_DIG DBL_DIG
#endif

static int needs_quote(module *m, const char *src, size_t srclen)
{
	if (!strcmp(src, ",") || !strcmp(src, ".") || !strcmp(src, "|"))
		return 1;

	if (!*src || isupper(*src) || isdigit(*src))
		return 1;

	if (!strcmp(src, "[]") || !strcmp(src, "!"))
		return 0;

	if (get_op(m, src, NULL, NULL, 0))
		return 0;

	while (srclen--) {
		int ch = get_char_utf8(&src);

		if ((iscntrl(ch) || isspace(ch) || ispunct(ch)) && (ch != '_'))
			return 1;

		if ((ch == '\'') || (ch == '"') || (ch == '`'))
			return 1;
	}

	return 0;
}

static size_t _sprint_int(char *dst, size_t size, int_t n, int base)
{
	const char *save_dst = dst;

	if ((n / base) > 0)
		dst += _sprint_int(dst, size, n / base, base);

	int n2 = n % base;

	if (n2 > 9) {
		n2 -= 10;
		n2 += 'A';
	} else
		n2 += '0';

	if (size) *dst++ = (char)n2; else dst++;
	return dst - save_dst;
}

size_t sprint_int(char *dst, size_t size, int_t n, int base)
{
	const char *save_dst = dst;

	if ((n < 0) && (base == 10)) {
		if (size) *dst++ = '-'; else dst++;
		n = llabs((long long)n);
	}

	if (n == 0) {
		if (size) *dst++ = '0'; else dst++;
		if (size) *dst = '\0';
		return dst - save_dst;
	}

	dst += _sprint_int(dst, size, n, base);
	if (size) *dst = '\0';
	return dst - save_dst;
}

static size_t formatted(char *dst, size_t dstlen, const char *src, size_t srclen)
{
	extern const char *g_escapes;
	extern const char *g_anti_escapes;
	size_t len = 0;

	while (srclen--) {
		int ch = *src++;
		const char *ptr = strchr(g_escapes, ch);

		if (ch && ptr) {
			if (dstlen) {
				*dst++ = '\\';
				*dst++ = g_anti_escapes[ptr-g_escapes];
			}

			len += 2;
		} else if (ch < ' ') {
			if (dstlen) {
				*dst++ = '\\';
				*dst++ = 'x';
			}

			size_t n = snprintf(dst, dstlen, "%d", ch);
			len += n;
			if (dstlen) dst += n;

			if (dstlen)
				*dst++ = '\\';

			len += 3;
		} else {
			if (dstlen)
				*dst++ = ch;

			len++;
		}
	}

	if (dstlen)
		*dst = '\0';

	return len;
}

static size_t plain(char *dst, size_t dstlen, const char *src, size_t srclen)
{
	size_t len = 0;

	while (srclen--) {
		int ch = *src++;

		if (dstlen)
			*dst++ = ch;

		len++;
	}

	if (dstlen)
		*dst = '\0';

	return len;
}

size_t write_canonical_to_buf(query *q, char *dst, size_t dstlen, cell *c, idx_t c_ctx, int running, int depth)
{
	char *save_dst = dst;

	if (depth > MAX_DEPTH) {
		if (depth > 64)
			dst += snprintf(dst, dstlen, "...");

		q->cycle_error = true;
		return dst - save_dst;
	}

	if (is_rational(c)) {
		if (((c->flags & FLAG_HEX) || (c->flags & FLAG_BINARY))) {
			dst += snprintf(dst, dstlen, "%s0x", c->val_num<0?"-":"");
			dst += sprint_int(dst, dstlen, c->val_num, 16);
		} else if ((c->flags & FLAG_OCTAL) && !running) {
			dst += snprintf(dst, dstlen, "%s0o", c->val_num<0?"-":"");
			dst += sprint_int(dst, dstlen, c->val_num, 8);
		} else if (c->val_den != 1) {
			if (q->m->flag.rational_syntax_natural) {
				dst += sprint_int(dst, dstlen, c->val_num, 10);
				dst += snprintf(dst, dstlen, "%s", "/");
				dst += sprint_int(dst, dstlen, c->val_den, 10);
			} else {
				dst += sprint_int(dst, dstlen, c->val_num, 10);
				dst += snprintf(dst, dstlen, "%s", " rdiv ");
				dst += sprint_int(dst, dstlen, c->val_den, 10);
			}
		} else
			dst += sprint_int(dst, dstlen, c->val_num, 10);

		return dst - save_dst;
	}

	if (is_float(c) && (c->val_flt == M_PI)) {
		dst += snprintf(dst, dstlen, "%s", "3.141592653589793");
		return dst - save_dst;
	} else if (is_float(c) && (c->val_flt == M_E)) {
		dst += snprintf(dst, dstlen, "%s", "2.718281828459045");
		return dst - save_dst;
	} else if (is_float(c)) {
		char tmpbuf[256];
		sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG, c->val_flt);
		const char *ptr = strchr(tmpbuf, '.');

		if (ptr && (strlen(ptr+1) > 1))
			sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG, c->val_flt);

		if (!strchr(tmpbuf, '.'))
			strcat(tmpbuf, ".0");

		dst += snprintf(dst, dstlen, "%s", tmpbuf);
		return dst - save_dst;
	}

	if (is_variable(c) && ((1ULL << c->var_nbr) & q->nv_mask)) {
		dst += snprintf(dst, dstlen, "'$VAR'(%u)", q->nv_start + count_bits(q->nv_mask, c->var_nbr));
		return dst - save_dst;
	}

	if (is_string(c)) {
		int cnt = 0;
		cell *l = c;

		while (is_list(l)) {
			if (cnt > 64) {
				dst += snprintf(dst, dstlen, "...");
				return dst - save_dst;
			}

			cell *h = LIST_HEAD(l);
			l = LIST_TAIL(l);
			h->flags &= ~FLAG_STRING;

			if (!cnt++)
				alloc_list(q, h);
			else
				append_list(q, h);
		}

		c = end_list(q);
	}

	const char *src = GET_STR(c);
	int dq = 0, quote = !is_variable(c) && needs_quote(q->m, src, LEN_STR(c));
	if (is_string(c)) dq = quote = 1;
	dst += snprintf(dst, dstlen, "%s", quote?dq?"\"":"'":"");
	dst += formatted(dst, dstlen, src, LEN_STR(c));
	dst += snprintf(dst, dstlen, "%s", quote?dq?"\"":"'":"");

	if (!is_structure(c))
		return dst - save_dst;

	idx_t arity = c->arity;
	dst += snprintf(dst, dstlen, "(");

	for (c++; arity--; c += c->nbr_cells) {
		cell *tmp = running ? deref(q, c, c_ctx) : c;
		dst += write_canonical_to_buf(q, dst, dstlen, tmp, q->latest_ctx, running, depth+1);

		if (arity)
			dst += snprintf(dst, dstlen, ",");
	}

	dst += snprintf(dst, dstlen, ")");
	return dst - save_dst;
}

static char *varformat(unsigned nbr)
{
	static char tmpbuf[80];
	char *dst = tmpbuf;
	dst += sprintf(dst, "%c", 'A'+nbr%26);
	if ((nbr/26) > 0) sprintf(dst, "%u", nbr/26);
	return tmpbuf;
}

size_t write_term_to_buf(query *q, char *dst, size_t dstlen, cell *c, idx_t c_ctx, int running, int cons, int depth)
{
	char *save_dst = dst;

	if (depth > MAX_DEPTH) {
		if (depth > 64)
			dst += snprintf(dst, dstlen, "...");

		q->cycle_error = true;
		return dst - save_dst;
	}

	if (is_rational(c)) {
		if (((c->flags & FLAG_HEX) || (c->flags & FLAG_BINARY)) && (running <= 0)) {
			dst += snprintf(dst, dstlen, "%s0x", c->val_num<0?"-":"");
			dst += sprint_int(dst, dstlen, c->val_num, 16);
		} else if ((c->flags & FLAG_OCTAL) && !running) {
			dst += snprintf(dst, dstlen, "%s0o", c->val_num<0?"-":"");
			dst += sprint_int(dst, dstlen, c->val_num, 8);
		} else if (c->val_den != 1) {
			if (q->m->flag.rational_syntax_natural) {
				dst += sprint_int(dst, dstlen, c->val_num, 10);
				dst += snprintf(dst, dstlen, "/");
				dst += sprint_int(dst, dstlen, c->val_den, 10);
			} else {
				dst += sprint_int(dst, dstlen, c->val_num, 10);
				dst += snprintf(dst, dstlen, " rdiv ");
				dst += sprint_int(dst, dstlen, c->val_den, 10);
			}
		} else
			dst += sprint_int(dst, dstlen, c->val_num, 10);

		return dst - save_dst;
	}

	if (is_float(c) && (c->val_flt == M_PI)) {
		dst += snprintf(dst, dstlen, "%s", "3.141592653589793");
		return dst - save_dst;
	} else if (is_float(c) && (c->val_flt == M_E)) {
		dst += snprintf(dst, dstlen, "%s", "2.718281828459045");
		return dst - save_dst;
	} else if (is_float(c)) {
		char tmpbuf[256];
		sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG-1, c->val_flt);
		const char *ptr = strchr(tmpbuf, '.');

		if (ptr && (strlen(ptr+1) > 1))
			sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG, c->val_flt);

		if (!strchr(tmpbuf, '.'))
			strcat(tmpbuf, ".0");

		dst += snprintf(dst, dstlen, "%s", tmpbuf);
		return dst - save_dst;
	}

	int is_chars_list = scan_is_chars_list(q, c, c_ctx, 0);

	if (is_chars_list) {
		cell *l = c;
		dst += snprintf(dst, dstlen, "%s", "\"");

		while (is_list(l)) {
			cell *h = LIST_HEAD(l);
			cell *c = deref(q, h, c_ctx);
			dst += formatted(dst, dstlen, GET_STR(c), LEN_STR(c));
			l = LIST_TAIL(l);
			l = deref(q, l, c_ctx);
			c_ctx = q->latest_ctx;
		}

		dst += snprintf(dst, dstlen, "%s", "\"");
		return dst - save_dst;
	}

	// FIXME make non-recursive

	const char *src = GET_STR(c);
	int print_list = 0;

	while (is_iso_list(c)) {
		if (q->max_depth && (depth >= q->max_depth)) {
			dst += snprintf(dst, dstlen, "%s", "...");
			return dst - save_dst;
		}

		cell *head = LIST_HEAD(c);

		if (!cons)
			dst += snprintf(dst, dstlen, "%s", "[");

		head = running ? deref(q, head, c_ctx) : head;
		idx_t head_ctx = q->latest_ctx;
		int parens = is_structure(head) && (!strcmp(GET_STR(head), ",") || !strcmp(GET_STR(head), ";") || !strcmp(GET_STR(head), "->") || !strcmp(GET_STR(head), "-->"));
		if (parens) dst += snprintf(dst, dstlen, "%s", "(");
		dst += write_term_to_buf(q, dst, dstlen, head, head_ctx, running, 0, depth+1);
		if (parens) dst += snprintf(dst, dstlen, "%s", ")");
		cell *tail = LIST_TAIL(c);
		tail = running ? deref(q, tail, c_ctx) : tail;
		c_ctx = q->latest_ctx;

		if (is_literal(tail) && !is_structure(tail)) {
			src = GET_STR(tail);

			if (strcmp(src, "[]")) {
				dst += snprintf(dst, dstlen, "%s", "|");
				dst += write_term_to_buf(q, dst, dstlen, tail, c_ctx, running, 1, depth+1);
			}
		} else if (is_iso_list(tail)) {
			dst += snprintf(dst, dstlen, "%s", ",");
			c = tail;
			print_list++;
			cons = 1;
			continue;
		} else if (is_string(tail)) {
			cell *l = tail;

			while (is_list(l)) {
				dst += snprintf(dst, dstlen, "%s", ",");
				cell *h = LIST_HEAD(l);
				dst += formatted(dst, dstlen, GET_STR(h), LEN_STR(h));
				l = LIST_TAIL(l);
			}

			print_list++;
		} else {
			dst += snprintf(dst, dstlen, "%s", "|");
			dst += write_term_to_buf(q, dst, dstlen, tail, c_ctx, running, 1, depth+1);
		}

		if (!cons || print_list)
			dst += snprintf(dst, dstlen, "%s", "]");

		return dst - save_dst;
	}

	int optype = (c->flags & OP_FX) | (c->flags & OP_FY) | (c->flags & OP_XF) |
		(c->flags & OP_YF) | (c->flags & OP_XFX) |
		(c->flags & OP_YFX) | (c->flags & OP_XFY);

	if (q->ignore_ops || !optype) {
		int quote = ((running <= 0) || q->quoted) && !is_variable(c) && needs_quote(q->m, src, LEN_STR(c));
		int dq = 0, braces = 0, parens = 0;
		if (is_string(c)) dq = quote = 1;
		if (q->quoted < 0) quote = 0;
		if (c->arity && !strcmp(src, "{}")) braces = 1;
		dst += snprintf(dst, dstlen, "%s", !braces&&quote?dq?"\"":"'":"");

		if (q->quoted && get_op(q->m, src, NULL, NULL, 0) && strcmp(src, "|"))
			parens = 1;

		if (parens)
			dst += snprintf(dst, dstlen, "%s", "(");

		if (running && is_variable(c) && ((1ULL << c->var_nbr) & q->nv_mask)) {
			dst += snprintf(dst, dstlen, "%s", varformat(q->nv_start + count_bits(q->nv_mask, c->var_nbr)));
			return dst - save_dst;
		}

		if (running && is_variable(c)
			/*&& ((c_ctx != q->st.curr_frame) || is_fresh(c) || (running > 0))*/) {
			frame *g = GET_FRAME(c_ctx);
			slot *e = GET_SLOT(g, c->var_nbr);
			idx_t slot_nbr = e - q->slots;
			dst += snprintf(dst, dstlen, "_%u", (unsigned)slot_nbr);
			return dst - save_dst;
		}

		int len_str = LEN_STR(c);

		if (braces)
			;
		else if (quote) {
			if ((running < 0) && is_blob(c) && (len_str > 128))
				len_str = 128;

			dst += formatted(dst, dstlen, src, LEN_STR(c));

			if ((running < 0) && is_blob(c) && (len_str == 128))
				dst += snprintf(dst, dstlen, "%s", "...");
		} else
			dst += plain(dst, dstlen, src, LEN_STR(c));

		dst += snprintf(dst, dstlen, "%s", !braces&&quote?dq?"\"":"'":"");

		if (parens)
			dst += snprintf(dst, dstlen, "%s", ")");

		if (is_structure(c) && !is_string(c)) {
			idx_t arity = c->arity;
			dst += snprintf(dst, dstlen, "%s", braces?"{":"(");

			for (c++; arity--; c += c->nbr_cells) {
				cell *tmp = running ? deref(q, c, c_ctx) : c;
				idx_t tmp_ctx = q->latest_ctx;
				int parens = 0;

				if (!braces && is_literal(tmp)) {
					const char *s = GET_STR(tmp);

					if (!strcmp(s, ",") || !strcmp(s, ";") ||
						!strcmp(s, "->") || !strcmp(s, ":-") ||
						!strcmp(s, "-->"))
						parens = 1;
				}

				if (parens)
					dst += snprintf(dst, dstlen, "%s", "(");

				dst += write_term_to_buf(q, dst, dstlen, tmp, tmp_ctx, running, 0, depth+1);

				if (parens)
					dst += snprintf(dst, dstlen, "%s", ")");

				if (arity)
					dst += snprintf(dst, dstlen, "%s", ",");
			}

			dst += snprintf(dst, dstlen, "%s", braces?"}":")");
		}
	} else if ((c->flags & OP_XF) || (c->flags & OP_YF)) {
		cell *lhs = c + 1;
		lhs = running ? deref(q, lhs, c_ctx) : lhs;
		idx_t lhs_ctx = q->latest_ctx;
		dst += write_term_to_buf(q, dst, dstlen, lhs, lhs_ctx, running, 0, depth+1);
		dst += snprintf(dst, dstlen, "%s", src);
	} else if ((c->flags & OP_FX) || (c->flags & OP_FY)) {
		cell *rhs = c + 1;
		rhs = running ? deref(q, rhs, c_ctx) : rhs;
		idx_t rhs_ctx = q->latest_ctx;
		int space = isalpha_utf8(peek_char_utf8(src)) || !strcmp(src, ":-") || !strcmp(src, "\\+");
		int parens = is_structure(rhs) && !strcmp(GET_STR(rhs), ",");
		dst += snprintf(dst, dstlen, "%s", src);
		if (space && !parens) dst += snprintf(dst, dstlen, "%s", " ");
		if (parens) dst += snprintf(dst, dstlen, "%s", "(");
		dst += write_term_to_buf(q, dst, dstlen, rhs, rhs_ctx, running, 0, depth+1);
		if (parens) dst += snprintf(dst, dstlen, "%s", ")");
	} else {
		cell *lhs = c + 1;
		cell *rhs = lhs + lhs->nbr_cells;
		lhs = running ? deref(q, lhs, c_ctx) : lhs;
		idx_t lhs_ctx = q->latest_ctx;
		rhs = running ? deref(q, rhs, c_ctx) : rhs;
		idx_t rhs_ctx = q->latest_ctx;
		int my_prec = get_op(q->m, GET_STR(c), NULL, NULL, 0);
		int lhs_prec1 = is_literal(lhs) ? get_op(q->m, GET_STR(lhs), NULL, NULL, 0) : 0;
		int lhs_prec2 = is_literal(lhs) && !lhs->arity ? get_op(q->m, GET_STR(lhs), NULL, NULL, 0) : 0;
		int rhs_prec1 = is_literal(rhs) ? get_op(q->m, GET_STR(rhs), NULL, NULL, 0) : 0;
		int rhs_prec2 = is_literal(rhs) && !rhs->arity ? get_op(q->m, GET_STR(rhs), NULL, NULL, 0) : 0;
		int parens = 0;//depth && strcmp(src, ",") && strcmp(src, "is") && strcmp(src, "->");
		int lhs_parens = lhs_prec1 > my_prec;
		lhs_parens |= lhs_prec2;
		if (parens || lhs_parens) dst += snprintf(dst, dstlen, "%s", "(");
		dst += write_term_to_buf(q, dst, dstlen, lhs, lhs_ctx, running, 0, depth+1);
		if (lhs_parens) dst += snprintf(dst, dstlen, "%s", ")");
		int space = isalpha_utf8(peek_char_utf8(src)) || !strcmp(src, ":-") || !strcmp(src, "-->") || !strcmp(src, "=..") || !*src;
		if (space && !parens) dst += snprintf(dst, dstlen, "%s", " ");
		dst += snprintf(dst, dstlen, "%s", src);
		if (!*src) space = 0;
		if (space && !parens) dst += snprintf(dst, dstlen, "%s", " ");
		int rhs_parens = rhs_prec1 > my_prec;
		rhs_parens |= rhs_prec2;
		if (rhs_parens) dst += snprintf(dst, dstlen, "%s", "(");
		dst += write_term_to_buf(q, dst, dstlen, rhs, rhs_ctx, running, 0, depth+1);
		if (parens || rhs_parens) dst += snprintf(dst, dstlen, "%s", ")");
	}

	return dst - save_dst;
}

char *write_term_to_strbuf(query *q, cell *c, idx_t c_ctx, int running)
{
	size_t len = write_term_to_buf(q, NULL, 0, c, c_ctx, running, 0, 0);
	char *buf = malloc(len+10);
	ensure(buf);
	write_term_to_buf(q, buf, len+1, c, c_ctx, running, 0, 0);
	return buf;
}

void write_canonical_to_stream(query *q, stream *str, cell *c, idx_t c_ctx, int running, int depth)
{
	size_t len = write_canonical_to_buf(q, NULL, 0, c, c_ctx, running, depth);

	if (q->cycle_error) {
		running = 0;
		len = write_canonical_to_buf(q, NULL, 0, c, c_ctx, running, depth);
	}

	char *dst = malloc(len+1);
	ensure(dst);
	write_canonical_to_buf(q, dst, len+1, c, c_ctx, running, depth);
	q->cycle_error = false;
	const char *src = dst;

	while (len) {
		size_t nbytes = net_write(src, len, str);

		if (feof(str->fp)) {
			q->error = true;
			return;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
}

void write_canonical(query *q, FILE *fp, cell *c, idx_t c_ctx, int running, int depth)
{
	size_t len = write_canonical_to_buf(q, NULL, 0, c, c_ctx, running, depth);

	if (q->cycle_error) {
		running = 0;
		len = write_canonical_to_buf(q, NULL, 0, c, c_ctx, running, depth);
	}

	char *dst = malloc(len+1);
	ensure(dst);
	write_canonical_to_buf(q, dst, len+1, c, c_ctx, running, depth);
	q->cycle_error = false;
	const char *src = dst;

	while (len) {
		size_t nbytes = fwrite(src, 1, len, fp);

		if (feof(fp)) {
			q->error = true;
			return;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
}

void write_term_to_stream(query *q, stream *str, cell *c, idx_t c_ctx, int running, int cons, int depth)
{
	size_t len = write_term_to_buf(q, NULL, 0, c, c_ctx, running, cons, depth);

	if (q->cycle_error) {
		running = 0;
		len = write_term_to_buf(q, NULL, 0, c, c_ctx, running, cons, depth);
	}

	char *dst = malloc(len+1);
	ensure(dst);
	write_term_to_buf(q, dst, len+1, c, c_ctx, running, cons, depth);
	q->cycle_error = false;
	const char *src = dst;

	while (len) {
		size_t nbytes = net_write(src, len, str);

		if (feof(str->fp)) {
			q->error = true;
			return;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
}

void write_term(query *q, FILE *fp, cell *c, idx_t c_ctx, int running, int cons, int depth)
{
	size_t len = write_term_to_buf(q, NULL, 0, c, c_ctx, running, cons, depth);

	if (q->cycle_error) {
		running = 0;
		len = write_term_to_buf(q, NULL, 0, c, c_ctx, running, cons, depth);
	}

	char *dst = malloc(len+1);
	ensure(dst);
	write_term_to_buf(q, dst, len+1, c, c_ctx, running, cons, depth);
	q->cycle_error = false;
	const char *src = dst;

	while (len) {
		size_t nbytes = fwrite(src, 1, len, fp);

		if (feof(fp)) {
			q->error = true;
			return;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
}
