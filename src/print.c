#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <math.h>
#include <float.h>
#include <inttypes.h>

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

bool needs_quote(module *m, const char *src, size_t srclen)
{
	if (!strcmp(src, ",") || !strcmp(src, ".") || !strcmp(src, "|"))
		return true;

	if (!*src || isupper(*src) || isdigit(*src) || (*src == '_') || (*src == '$'))
		return true;

	if (!strcmp(src, "{}") || !strcmp(src, "[]") || !strcmp(src, "!") || !strcmp(src, "¡"))
		return false;

	if (get_op(m, src, NULL, false))
		return false;

	while (srclen--) {
		int ch = get_char_utf8(&src);

		if (!isalnum(ch) && (ch != '_'))
			return true;
	}

	return false;
}

static size_t sprint_int_(char *dst, size_t size, int_t n, int base)
{
	const char *save_dst = dst;
	if ((n / base) > 0)
		dst += sprint_int_(dst, size, n / base, base);

	int n2 = n % base;

	if (n2 > 9) {
		n2 -= 10;
		n2 += 'A';
	} else
		n2 += '0';

	if (size) *dst++ = n2; else dst++;
	return dst - save_dst;
}

size_t sprint_int(char *dst, size_t size, int_t n, int base)
{
	const char *save_dst = dst;

	if ((n < 0) && (base == 10)) {
		if (size) *dst++ = '-'; else dst++;

		// NOTE: according to the man pages...
		//
		//		"Trying to take the absolute value of
		// 		the most negative integer is not defined."
		//

#if USE_INT32
		if (n == INT32_MIN)
#else
		if (n == INT64_MIN)
#endif
			n = imaxabs(n+1) - 1;
		else
			n = imaxabs(n);
	}

	if (n == 0) {
		if (size) *dst++ = '0'; else dst++;
		if (size) *dst = '\0';
		return dst - save_dst;
	}

	dst += sprint_int_(dst, size, n, base);
	if (size) *dst = '\0';
	return dst - save_dst;
}

size_t formatted(char *dst, size_t dstlen, const char *src, size_t srclen, bool dq)
{
	extern const char *g_escapes;
	extern const char *g_anti_escapes;
	size_t len = 0;

	while (srclen--) {
		int ch = *src++;
		const char *ptr = ch != ' ' ? strchr(g_escapes, ch) : NULL;

		if (ch && ptr) {
			if (dstlen) {
				*dst++ = '\\';
				*dst++ = g_anti_escapes[ptr-g_escapes];
			}

			len += 2;
		} else if (!dq && (ch == '\'')) {
			if (dstlen) {
				*dst++ = '\'';
				*dst++ = ch;
			}

			len += 2;
		} else if (ch == (dq?'"':'\'')) {
			if (dstlen) {
				*dst++ = '\\';
				*dst++ = ch;
			}

			len += 2;
		} else if (ch < ' ') {
			if (dstlen) {
				*dst++ = '\\';
				*dst++ = 'x';
			}

			size_t n = snprintf(dst, dstlen, "%u", ch);
			len += n;
			if (dstlen) dst += n;

			if (dstlen)
				*dst++ = '\\';

			len += 3;
		} else if (ch == '\\') {
			if (dstlen) {
				*dst++ = '\\';
				*dst++ = ch;
			}

			len += 2;
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

static size_t plain(char *dst, size_t dstlen, const char *src, size_t srclen, __attribute__((unused)) bool dq)
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

static void reformat_float(char *tmpbuf)
{
	char tmpbuf2[256];
	strcpy(tmpbuf2, tmpbuf);
	const char *src = tmpbuf2;
	char *dst = tmpbuf;

	if (*src == '-')
		*dst++ = *src++;

	while (isdigit(*src))
		*dst++ = *src++;

	if (*src != '.') {
		*dst++ = '.';
		*dst++ = '0';
	}

	while (*src)
		*dst++ = *src++;

	*dst = '\0';
}

static int find_binding(query *q, idx_t var_nbr, idx_t var_ctx)
{
	const frame *g = GET_FRAME(q->st.curr_frame);
	const slot *e = GET_SLOT(g, 0);

	for (idx_t i = 0; i < g->nbr_vars; i++, e++) {
		if (!is_variable(&e->c))
			continue;

		if (e->ctx != var_ctx)
			continue;

		if (e->c.var_nbr == var_nbr)
			return i;
	}

	return ERR_IDX;
}

static uint8_t s_mask1[MAX_ARITY] = {0}, s_mask2[MAX_ARITY] = {0};

static unsigned count_non_anons(const uint8_t *mask, unsigned bit)
{
	unsigned bits = 0;

	for (unsigned i = 0; i < bit; i++) {
		if (mask[i])
			bits++;
	}

	return bits;
}

ssize_t print_canonical_to_buf(query *q, char *dst, size_t dstlen, cell *c, idx_t c_ctx, int running, unsigned depth)
{
	if (!depth && !dst && !dstlen) {
		fake_numbervars(q, c, c_ctx, 0);
		memset(s_mask1, 0, MAX_ARITY);
		memset(s_mask2, 0, MAX_ARITY);
		q->nv_start = -1;
	}

	char *save_dst = dst;

	if (depth > MAX_DEPTH)
		return -1;

	if (is_rational(c)) {
		if (((c->flags & FLAG_HEX) || (c->flags & FLAG_BINARY))) {
			dst += snprintf(dst, dstlen, "%s0x", c->val_num<0?"-":"");
			dst += sprint_int(dst, dstlen, c->val_num, 16);
		} else if ((c->flags & FLAG_OCTAL) && !running) {
			dst += snprintf(dst, dstlen, "%s0o", c->val_num<0?"-":"");
			dst += sprint_int(dst, dstlen, c->val_num, 8);
		} else if (c->val_den != 1) {
			if (q->flag.rational_syntax_natural) {
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
		dst += snprintf(dst, dstlen, "3.141592653589793");
		return dst - save_dst;
	}

	if (is_float(c) && (c->val_flt == M_E)) {
		dst += snprintf(dst, dstlen, "2.718281828459045");
		return dst - save_dst;
	}

	if (is_float(c)) {
		char tmpbuf[256];
		sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG, c->val_flt);
		const char *ptr = strchr(tmpbuf, '.');

		if (ptr && (strlen(ptr+1) > 1))
			sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG, c->val_flt);

		reformat_float(tmpbuf);
		dst += snprintf(dst, dstlen, "%s", tmpbuf);
		return dst - save_dst;
	}

	idx_t var_nbr = 0;

	if (is_variable(c)
		&& (running>0) && (q->nv_start == -1)
		&& ((var_nbr = find_binding(q, c->var_nbr, c_ctx)) != ERR_IDX)) {

		for (unsigned i = 0; i < MAX_ARITY; i++) {
			if (q->nv_mask[i])
				break;

			var_nbr--;
		}

#if 1
		if (!dstlen) {
			if (!(s_mask1[var_nbr]))
				s_mask1[var_nbr] = 1;
			else
				s_mask2[var_nbr] = 1;
		}

		unsigned nbr = count_non_anons(s_mask2, var_nbr);

		char ch = 'A';
		ch += nbr % 26;
		unsigned n = (unsigned)nbr / 26;

		if (dstlen && !(s_mask2[var_nbr]))
			dst += snprintf(dst, dstlen, "%s", "_");
		else if (nbr < 26)
			dst += snprintf(dst, dstlen, "%c", ch);
		else
			dst += snprintf(dst, dstlen, "%c%u", ch, n);
#else
		dst += snprintf(dst, dstlen, "_V%d", var_nbr);
#endif

		return dst - save_dst;
	}

	if (is_variable(c) && (running>0)) {
		frame *g = GET_FRAME(c_ctx);
		slot *e = GET_SLOT(g, c->var_nbr);
		idx_t slot_nbr = e - q->slots;
		dst += snprintf(dst, dstlen, "_%u", (unsigned)slot_nbr);
		return dst - save_dst;
	}

	if (is_string(c)) {
		int cnt = 0;
		cell *l = c;

		LIST_HANDLER(l);

		while (is_list(l)) {
			if ((cnt > 256) && (running < 0)) {
				dst += snprintf(dst, dstlen, "|...");
				return dst - save_dst;
			}

			cell *h = LIST_HEAD(l);
			l = LIST_TAIL(l);
			h->flags &= ~FLAG_STRING;

			if (!cnt++)
				allocate_list_on_heap(q, h);
			else
				append_list(q, h);
		}

		c = end_list(q);
	}

	const char *src = GET_STR(c);
	int dq = 0, quote = !is_variable(c) && needs_quote(q->m, src, LEN_STR(c));
	if (is_string(c)) dq = quote = 1;
	dst += snprintf(dst, dstlen, "%s", quote?dq?"\"":"'":"");

	if (quote || q->quoted)
		dst += formatted(dst, dstlen, src, LEN_STR(c), dq);
	else
		dst += plain(dst, dstlen, src, LEN_STR(c), dq);

	dst += snprintf(dst, dstlen, "%s", quote?dq?"\"":"'":"");

	if (!is_structure(c))
		return dst - save_dst;

	idx_t arity = c->arity;
	dst += snprintf(dst, dstlen, "(");

	for (c++; arity--; c += c->nbr_cells) {
		cell *tmp = running ? deref(q, c, c_ctx) : c;
		ssize_t res = print_canonical_to_buf(q, dst, dstlen, tmp, q->latest_ctx, running, depth+1);
		if (res < 0) return -1;
		dst += res;

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

ssize_t print_term_to_buf(query *q, char *dst, size_t dstlen, cell *c, idx_t c_ctx, int running, int cons, unsigned depth)
{
	char *save_dst = dst;

	if (depth > MAX_DEPTH)
		return -1;

	if (is_rational(c)) {
		if (((c->flags & FLAG_HEX) || (c->flags & FLAG_BINARY))) {
			dst += snprintf(dst, dstlen, "%s0x", c->val_num<0?"-":"");
			dst += sprint_int(dst, dstlen, c->val_num, 16);
		} else if (c->val_den != 1) {
			if (q->flag.rational_syntax_natural) {
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

#if USE_GMP
	if (is_bignum(c)) {
		dst += gmp_snprintf(dst, dstlen, "%Zd", &c->val_mpz);
		return dst - save_dst;
	}
#endif

	if (is_float(c) && (c->val_flt == M_PI)) {
		dst += snprintf(dst, dstlen, "3.141592653589793");
		return dst - save_dst;
	}

	if (is_float(c) && (c->val_flt == M_E)) {
		dst += snprintf(dst, dstlen, "2.718281828459045");
		return dst - save_dst;
	}

	if (is_float(c)) {
		char tmpbuf[256];
		sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG-1, c->val_flt);
		const char *ptr = strchr(tmpbuf, '.');

		if (ptr && (strlen(ptr+1) > 1))
			sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG, c->val_flt);

		reformat_float(tmpbuf);
		dst += snprintf(dst, dstlen, "%s", tmpbuf);
		return dst - save_dst;
	}

	int is_chars_list = scan_is_chars_list(q, c, c_ctx, 0);

	if (is_chars_list) {
		cell *l = c;
		dst += snprintf(dst, dstlen, "%s", "\"");
		LIST_HANDLER(l);

		while (is_list(l)) {
			cell *h = LIST_HEAD(l);
			cell *c = deref(q, h, c_ctx);
			dst += formatted(dst, dstlen, GET_STR(c), LEN_STR(c), false);
			l = LIST_TAIL(l);
			l = deref(q, l, c_ctx);
			c_ctx = q->latest_ctx;
		}

		dst += snprintf(dst, dstlen, "%s", "\"");
		return dst - save_dst;
	}

	// FIXME make non-recursive

	const char *src = GET_STR(c);
	unsigned print_list = 0, cnt = 0;

	while (is_iso_list(c)) {
		if (cnt++ > MAX_DEPTH) {
			return ~(dst - save_dst);
		}

		if (q->max_depth && (depth >= q->max_depth) && (running < 0)) {
			dst += snprintf(dst, dstlen, "%s", "|...");
			return dst - save_dst;
		}

		LIST_HANDLER(c);

		cell *head = LIST_HEAD(c);

		if (!cons)
			dst += snprintf(dst, dstlen, "%s", "[");

		head = running ? deref(q, head, c_ctx) : head;
		idx_t head_ctx = q->latest_ctx;
		bool special_op = (!strcmp(GET_STR(head), ",")
			|| !strcmp(GET_STR(head), ";")
			|| !strcmp(GET_STR(head), "->")
			|| !strcmp(GET_STR(head), "*->")
			|| !strcmp(GET_STR(head), "-->"));
		int parens = is_structure(head) && special_op;
		if (parens) dst += snprintf(dst, dstlen, "%s", "(");
		ssize_t res = print_term_to_buf(q, dst, dstlen, head, head_ctx, running, 0, depth+1);
		if (res < 0) return -1;
		dst += res;
		if (parens) dst += snprintf(dst, dstlen, "%s", ")");

		cell *tail = LIST_TAIL(c);
		tail = running ? deref(q, tail, c_ctx) : tail;
		c_ctx = q->latest_ctx;

		if (is_literal(tail) && !is_structure(tail)) {
			src = GET_STR(tail);

			if (strcmp(src, "[]")) {
				dst += snprintf(dst, dstlen, "%s", "|");
				ssize_t res = print_term_to_buf(q, dst, dstlen, tail, c_ctx, running, 1, depth+1);
				if (res < 0) return -1;
				dst += res;
			}
		} else if (is_iso_list(tail)) {
			dst += snprintf(dst, dstlen, "%s", ",");
			c = tail;
			print_list++;
			cons = 1;
			continue;
		} else if (is_string(tail)) {
			cell *l = tail;
			LIST_HANDLER(l);

			while (is_list(l)) {
				dst += snprintf(dst, dstlen, "%s", ",");
				cell *h = LIST_HEAD(l);
				dst += formatted(dst, dstlen, GET_STR(h), LEN_STR(h), false);
				l = LIST_TAIL(l);
			}

			print_list++;
		} else {
			dst += snprintf(dst, dstlen, "%s", "|");
			ssize_t res = print_term_to_buf(q, dst, dstlen, tail, c_ctx, running, 1, depth+1);
			if (res < 0) return -1;
			dst += res;
		}

		if (!cons || print_list)
			dst += snprintf(dst, dstlen, "%s", "]");

		return dst - save_dst;
	}

	int optype = GET_OP(c);

	if (q->ignore_ops || !optype || !c->arity) {
		int quote = ((running <= 0) || q->quoted) && !is_variable(c) && needs_quote(q->m, src, LEN_STR(c));
		int dq = 0, braces = 0, parens = 0;
		if (is_string(c)) dq = quote = 1;
		if (q->quoted < 0) quote = 0;
		if ((c->arity == 1) && is_literal(c) && !strcmp(src, "{}")) braces = 1;

		if (running && is_literal(c) && !strcmp(src, "$VAR") && q->numbervars && is_integer(c+1)) {
			unsigned var_nbr = ((c+1)->val_num) - q->nv_start;
			dst += snprintf(dst, dstlen, "%s", varformat(var_nbr));
			return dst - save_dst;
		}

		if (running && is_variable(c) && q->variable_names) {
			cell *l = q->variable_names;
			idx_t l_ctx = q->variable_names_ctx;
			LIST_HANDLER(l);

			while (is_list(l)) {
				cell *h = LIST_HEAD(l);
				h = deref(q, h, l_ctx);
				cell *name = h+1;
				cell *val = h+2;

				if (!strcmp(GET_STR(val), GET_STR(c))) {
					dst += snprintf(dst, dstlen, "%s", GET_STR(name));
					return dst - save_dst;
				}

				l = LIST_TAIL(l);
				l = deref(q, l, l_ctx);
				l_ctx = q->latest_ctx;
			}
		}

		dst += snprintf(dst, dstlen, "%s", !braces&&quote?dq?"\"":"'":"");

		if (parens)
			dst += snprintf(dst, dstlen, "%s", "(");

		if (running && is_variable(c)
			&& ((c_ctx != q->st.curr_frame) || is_fresh(c) || (running > 0))) {
			frame *g = GET_FRAME(c_ctx);
			slot *e = GET_SLOT(g, c->var_nbr);
			dst += snprintf(dst, dstlen, "_%u", (unsigned)(e - q->slots));
			return dst - save_dst;
		}

		int len_str = LEN_STR(c);

		if (braces)
			;
		else if (quote || q->quoted) {
			if ((running < 0) && is_blob(c) && (len_str > 256))
				len_str = 256;

			dst += formatted(dst, dstlen, src, LEN_STR(c), dq);

			if ((running < 0) && is_blob(c) && (len_str == 256))
				dst += snprintf(dst, dstlen, "%s", "|...");
		} else
			dst += plain(dst, dstlen, src, LEN_STR(c), false);

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
						!strcmp(s, "*->") || !strcmp(s, "-->"))
						parens = 1;
				}

				if (parens)
					dst += snprintf(dst, dstlen, "%s", "(");

				ssize_t res = print_term_to_buf(q, dst, dstlen, tmp, tmp_ctx, running, 0, depth+1);
				if (res < 0) return -1;
				dst += res;

				if (parens)
					dst += snprintf(dst, dstlen, "%s", ")");

				if (arity)
					dst += snprintf(dst, dstlen, "%s", ",");
			}

			dst += snprintf(dst, dstlen, "%s", braces?"}":")");
		}
	} else if (IS_XF(c) || IS_YF(c)) {
		cell *lhs = c + 1;
		lhs = running ? deref(q, lhs, c_ctx) : lhs;
		idx_t lhs_ctx = q->latest_ctx;
		ssize_t res = print_term_to_buf(q, dst, dstlen, lhs, lhs_ctx, running, 0, depth+1);
		if (res < 0) return -1;
		dst += res;
		dst += snprintf(dst, dstlen, "%s", src);
	} else if (IS_FX(c) || IS_FY(c)) {
		cell *rhs = c + 1;
		rhs = running ? deref(q, rhs, c_ctx) : rhs;
		idx_t rhs_ctx = q->latest_ctx;
		int space = isalpha_utf8(peek_char_utf8(src)) || !strcmp(src, ":-") || !strcmp(src, "\\+");
		space += !strcmp(src, "-") && is_rational(rhs) && (rhs->val_num < 0);
		if (!strcmp(src, "-") && !is_rational(rhs)) dst += snprintf(dst, dstlen, "%s", " ");
		int parens = is_structure(rhs) && !strcmp(GET_STR(rhs), ",");
		dst += snprintf(dst, dstlen, "%s", src);
		if (space && !parens) dst += snprintf(dst, dstlen, "%s", " ");
		if (parens) dst += snprintf(dst, dstlen, "%s", "(");
		ssize_t res = print_term_to_buf(q, dst, dstlen, rhs, rhs_ctx, running, 0, depth+1);
		if (res < 0) return -1;
		dst += res;
		if (parens) dst += snprintf(dst, dstlen, "%s", ")");
	} else {
		cell *lhs = c + 1;
		cell *rhs = lhs + lhs->nbr_cells;
		lhs = running ? deref(q, lhs, c_ctx) : lhs;
		idx_t lhs_ctx = q->latest_ctx;
		rhs = running ? deref(q, rhs, c_ctx) : rhs;
		idx_t rhs_ctx = q->latest_ctx;
		int my_prec = get_op(q->m, GET_STR(c), NULL, false);
		int lhs_prec1 = is_literal(lhs) ? get_op(q->m, GET_STR(lhs), NULL, false) : 0;
		int lhs_prec2 = is_literal(lhs) && !lhs->arity ? get_op(q->m, GET_STR(lhs), NULL, false) : 0;
		int rhs_prec1 = is_literal(rhs) ? get_op(q->m, GET_STR(rhs), NULL, false) : 0;
		int rhs_prec2 = is_literal(rhs) && !rhs->arity ? get_op(q->m, GET_STR(rhs), NULL, false) : 0;
		//printf("\n*** c=%s prec=%d\n", GET_STR(c), my_prec);
		//printf("*** lhs=%s prec=%d\n", GET_STR(lhs), lhs_prec1);
		//printf("*** rhs=%s prec=%d\n", GET_STR(rhs), rhs_prec1);
		int parens = 0;//depth && strcmp(src, ",") && strcmp(src, "is") && strcmp(src, "->");
		int lhs_parens = lhs_prec1 > my_prec;
		lhs_parens |= lhs_prec2;
		if (parens || lhs_parens) dst += snprintf(dst, dstlen, "%s", "(");
		ssize_t res = print_term_to_buf(q, dst, dstlen, lhs, lhs_ctx, running, 0, depth+1);
		if (res < 0) return -1;
		dst += res;
		if (lhs_parens) dst += snprintf(dst, dstlen, "%s", ")");
		int space = isalpha_utf8(peek_char_utf8(src)) || !strcmp(src, ":-")
			|| !strcmp(src, "-->") || !strcmp(src, "->") || !strcmp(src, "*->")
			|| !strcmp(src, "=~=") || !strcmp(src, "=..")
			|| !strcmp(src, "=>")|| !strcmp(src, "?=")
			|| !*src;
		if (space && !parens) dst += snprintf(dst, dstlen, "%s", " ");

		dst += snprintf(dst, dstlen, "%s", src);
		if (!*src) space = 0;
		space += is_rational(rhs) && (rhs->val_num < 0);
		if (space && !parens) dst += snprintf(dst, dstlen, "%s", " ");

		int rhs_parens = rhs_prec1 > my_prec;
		rhs_parens |= rhs_prec1 && lhs_prec1 && (rhs_prec1 != lhs_prec1);
		rhs_parens |= rhs_prec2;
		if (rhs_parens) dst += snprintf(dst, dstlen, "%s", "(");
		res = print_term_to_buf(q, dst, dstlen, rhs, rhs_ctx, running, 0, depth+1);
		if (res < 0) return -1;
		dst += res;
		if (parens || rhs_parens) dst += snprintf(dst, dstlen, "%s", ")");
	}

	return dst - save_dst;
}

char *print_canonical_to_strbuf(query *q, cell *c, idx_t c_ctx, int running)
{
	bool cycle_error = false;
	ssize_t len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, running, 0);

	if (len < 0) {
		running = 0;
		len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, running, 1);
		cycle_error = true;
	}

	char *buf = malloc(len+10);
	ensure(buf);
	len = print_canonical_to_buf(q, buf, len+1, c, c_ctx, running, 0);
	q->cycle_error = cycle_error;
	return buf;
}

pl_state print_canonical_to_stream(query *q, stream *str, cell *c, idx_t c_ctx, int running)
{
	bool cycle_error = false;
	ssize_t len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, running, 0);

	if (len < 0) {
		running = 0;
		len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, running, 1);
		cycle_error = true;
	}

	char *dst = malloc(len*2+1); //cehteh: why *2?
	may_ptr_error(dst);
	len = print_canonical_to_buf(q, dst, len+1, c, c_ctx, running, 0);
	const char *src = dst;

	if (q->nv_start == -1) {
		memset(q->nv_mask, 0, MAX_ARITY);
		q->nv_start = 0;
	}

	while (len) {
		size_t nbytes = net_write(src, len, str);

		if (feof(str->fp)) {
			q->error = true;
			free(dst);
			q->cycle_error = cycle_error;
			return pl_error; //cehteh: need a pl_eof error?
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
	q->cycle_error = cycle_error;
	return pl_success;
}

pl_state print_canonical(query *q, FILE *fp, cell *c, idx_t c_ctx, int running)
{
	bool cycle_error = false;
	ssize_t len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, running, 0);

	if (len < 0) {
		running = 0;
		len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, running, 1);
		cycle_error = true;
	}

	char *dst = malloc(len*2+1); //cehteh: why *2?
	may_ptr_error(dst);
	len = print_canonical_to_buf(q, dst, len+1, c, c_ctx, running, 0);
	const char *src = dst;

	if (q->nv_start == -1) {
		memset(q->nv_mask, 0, MAX_ARITY);
		q->nv_start = 0;
	}

	while (len) {
		size_t nbytes = fwrite(src, 1, len, fp);

		if (feof(fp)) {
			q->error = true;
			free(dst);
			q->cycle_error = cycle_error;
			return pl_error;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
	q->cycle_error = cycle_error;
	return pl_success;
}

char *print_term_to_strbuf(query *q, cell *c, idx_t c_ctx, int running)
{
	bool cycle_error = false;
	ssize_t len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, 0, 0);

	if (len < 0) {
		running = 0;
		len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, 0, 1);
		cycle_error = true;
	}

	char *buf = malloc(len+10);
	ensure(buf);
	len = print_term_to_buf(q, buf, len+1, c, c_ctx, running, 0, 0);
	q->numbervars = false;
	q->cycle_error = cycle_error;
	return buf;
}

pl_state print_term_to_stream(query *q, stream *str, cell *c, idx_t c_ctx, int running)
{
	bool cycle_error = false;
	ssize_t len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, 0, 0);

	if (len < 0) {
		running = 0;
		len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, 0, 1);
		cycle_error = true;
	}

	char *dst = malloc(len+10);
	may_ptr_error(dst);
	len = print_term_to_buf(q, dst, len+1, c, c_ctx, running, 0, 0);
	const char *src = dst;

	while (len) {
		size_t nbytes = net_write(src, len, str);

		if (feof(str->fp)) {
			q->error = true;
			free(dst);
			q->cycle_error = cycle_error;
			return pl_error;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
	q->numbervars = false;
	q->cycle_error = cycle_error;
	return pl_success;
}

pl_state print_term(query *q, FILE *fp, cell *c, idx_t c_ctx, int running)
{
	bool cycle_error = false;
	ssize_t len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, 0, 0);

	if (len < 0) {
		running = 0;
		len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, 0, 1);
		cycle_error = true;
	}

	char *dst = malloc(len+10);
	may_ptr_error(dst);
	len = print_term_to_buf(q, dst, len+1, c, c_ctx, running, 0, 0);
	const char *src = dst;

	while (len) {
		size_t nbytes = fwrite(src, 1, len, fp);

		if (feof(fp)) {
			q->error = true;
			free(dst);
			q->cycle_error = cycle_error;
			return pl_error;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
	q->numbervars = false;
	q->cycle_error = cycle_error;
	return pl_success;
}
