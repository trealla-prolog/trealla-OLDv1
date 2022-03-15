#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <math.h>
#include <float.h>
#include <inttypes.h>

#include "internal.h"
#include "parser.h"
#include "module.h"
#include "query.h"
#include "network.h"
#include "heap.h"
#include "utf8.h"

#ifndef DBL_DECIMAL_DIG
#define DBL_DECIMAL_DIG DBL_DIG
#endif

bool needs_quoting(module *m, const char *src, int srclen)
{
	if (!*src)
		return true;

	if (!strcmp(src, ",") || !strcmp(src, ".") || !strcmp(src, "|"))
		return true;

	if (!strcmp(src, "{}") || !strcmp(src, "[]") || !strcmp(src, "!") || !strcmp(src, "\\"))
		return false;

	if ((src[0] == '/') && (src[1] == '*'))
		return true;

	int ch = peek_char_utf8(src);

	if (iswupper(ch) || isdigit(ch) || (ch == '_'))
		return true;

	if (search_op(m, src, NULL, false))
		return strchr(src, ' ')
			|| strchr(src, '\'')
			|| strchr(src, '\"')
			|| !strcmp(src, "(")
			|| !strcmp(src, ")")
			|| !strcmp(src, "[")
			|| !strcmp(src, "]")
			|| !strcmp(src, "{")
			|| !strcmp(src, "}");

	const char *s = src;
	int slen = srclen;

	while (slen > 0) {
		slen -= len_char_utf8(s);
		int ch = get_char_utf8(&s);

		if (((ch < 256) && strchr(g_solo, ch)) || iswspace(ch))
			return true;
	}

	int cnt = 0, alphas = 0, nonalphas = 0, graphs = 0;

	while (srclen > 0) {
		srclen -= len_char_utf8(src);
		int ch = get_char_utf8(&src);
		cnt++;

		if (iswalnum(ch) || (ch == '_'))
			alphas++;
		else if ((ch < 256) && isgraph(ch) && (ch != '%'))
			graphs++;
		else
			nonalphas++;
	}

	if (cnt == alphas)
		return false;

	if (cnt == graphs)
		return false;

#if 0
	if (cnt == nonalphas)
		return false;
#endif

	return true;
}

static bool op_needs_quoting(module *m, const char *src, int srclen)
{
	if (!strcmp(src, "{}") || !strcmp(src, "[]") || !strcmp(src, "!"))
		return false;

	int ch = peek_char_utf8(src);

	if (iswupper(ch) || isdigit(ch) || (ch == '_'))
		return true;

	if (search_op(m, src, NULL, false))
		return strchr(src, ' ')
			|| strchr(src, '\'')
			|| strchr(src, '\"')
			|| !strcmp(src, "(")
			|| !strcmp(src, ")")
			|| !strcmp(src, "[")
			|| !strcmp(src, "]")
			|| !strcmp(src, "{")
			|| !strcmp(src, "}");

	if (!iswlower(ch) || !iswalpha(ch)) { // NO %/
		static const char *s_symbols = "+-*<>=@#^~\\:$.";
		int quote = false;

		while (srclen--) {
			if (!strchr(s_symbols, *src)) {
				quote = true;
				break;
			}

			src++;
		}

		return quote;
	}

	while (srclen > 0) {
		int lench = len_char_utf8(src);
		int ch = get_char_utf8(&src);
		srclen -= lench;

		if (!iswalnum(ch) && (ch != '_'))
			return true;
	}

	return false;
}

static bool has_spaces(const char *src, int srclen)
{
	if (!*src)
		return true;

	while (srclen > 0) {
		int lench = len_char_utf8(src);
		int ch = get_char_utf8(&src);
		srclen -= lench;

		if (isspace(ch))
			return true;
	}

	return false;
}

size_t formatted(char *dst, size_t dstlen, const char *src, int srclen, bool dq)
{
	extern const char *g_escapes;
	extern const char *g_anti_escapes;
	size_t len = 0;
	int chars = 0;

	while (srclen > 0) {
		int lench = len_char_utf8(src);
		int ch = get_char_utf8(&src);
		srclen -= lench;
		chars++;
		const char *ptr = (lench == 1) && (ch != ' ') ? strchr(g_escapes, ch) : NULL;

		if ((ch == '\'') && dq)
			ptr = 0;

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
				dst += put_char_bare_utf8(dst, ch);

			len += lench;
		}
	}

	if (dstlen)
		*dst = '\0';

	return len;
}

static size_t plain(char *dst, size_t dstlen, const char *src, int srclen)
{
	if (dstlen) {
		memcpy(dst, src, srclen);
		dst[srclen] = '\0';
	}

	return srclen;
}

static size_t sprint_int_(char *dst, size_t size, pl_int_t n, int pbase)
{
	int base = abs(pbase);
	const char *save_dst = dst;

	if ((n / base) > 0)
		dst += sprint_int_(dst, size, n / base, pbase);

	int n2 = n % base;

	if (n2 > 9) {
		n2 -= 10;
		n2 += pbase < 0 ? 'A' : 'a';
	} else
		n2 += '0';

	if (size)
		*dst++ = n2;
	else
		dst++;

	return dst - save_dst;
}

size_t sprint_int(char *dst, size_t size, pl_int_t n, int base)
{
	const char *save_dst = dst;

	if ((n < 0) && (base == 10)) {
		if (size)
			*dst++ = '-';
		else
			dst++;

		// NOTE: according to the man pages...
		//
		//		"Trying to take the absolute value of
		// 		the most negative integer is not defined."
		//

		if (n == PL_INT_MIN)
			n = imaxabs(n+1) - 1;
		else
			n = imaxabs(n);
	}

	if (n == 0) {
		if (size)
			*dst++ = '0';
		else
			dst++;

		if (size)
			*dst = '\0';

		return dst - save_dst;
	}

	dst += sprint_int_(dst, size, n, base);

	if (size)
		*dst = '\0';

	return dst - save_dst;
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

	if ((*src != '.') && (*src != ',')) {
		*dst++ = '.';
		*dst++ = '0';
	} else if (*src == ',') {
		*dst++ = '.';
		src++;
	}

	while (*src)
		*dst++ = *src++;

	*dst = '\0';
}

static int find_binding(query *q, pl_idx_t var_nbr, pl_idx_t var_ctx)
{
	const frame *f = GET_FRAME(q->st.curr_frame);
	const slot *e = GET_FIRST_SLOT(f);

	for (pl_idx_t i = 0; i < f->nbr_vars; i++, e++) {
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

ssize_t print_canonical_to_buf(query *q, char *dst, size_t dstlen, cell *c, pl_idx_t c_ctx, int running, bool cons, unsigned depth)
{
	//if (!running)
	//	return print_term_to_buf(q, dst, dstlen, c, c_ctx, running, cons, depth);

	char *save_dst = dst;

	if (depth > MAX_DEPTH) {
		q->cycle_error = true;
		return -1;
	}

#if 0
	if (q->is_dump_vars && is_stream(c)) {
		dst += snprintf(dst, dstlen, "'$stream'(%d)", (int)get_smallint(c));
		return dst - save_dst;
	}
#endif

	if (is_bigint(c)) {
		int radix = 10;

		if (q->listing) {
			if (c->flags & FLAG_INT_BINARY)
				radix = 2;
			else if (c->flags & FLAG_INT_HEX)
				radix = 16;
			else if ((c->flags & FLAG_INT_OCTAL) && !running)
				radix = 8;

			if (c->flags & FLAG_INT_BINARY)
				dst += snprintf(dst, dstlen, "%s0b", is_negative(c)?"-":"");
			else if (c->flags & FLAG_INT_HEX)
				dst += snprintf(dst, dstlen, "%s0x", is_negative(c)?"-":"");
			else if (c->flags & FLAG_INT_OCTAL)
				dst += snprintf(dst, dstlen, "%s0o", is_negative(c)?"-":"");
		}

		if (!dstlen)
			dst += mp_int_string_len(&c->val_bigint->ival, radix) - 1;
		else {
			size_t len = mp_int_string_len(&c->val_bigint->ival, radix) -1;
			mp_int_to_string(&c->val_bigint->ival, radix, dst, len+1);
			dst += strlen(dst);
		}

		if (dstlen) *dst = 0;
		return dst - save_dst;
	}

	if (is_smallint(c)) {
		if (q->listing) {
			if (((c->flags & FLAG_INT_HEX) || (c->flags & FLAG_INT_BINARY))) {
				dst += snprintf(dst, dstlen, "%s0x", get_smallint(c)<0?"-":"");
				dst += sprint_int(dst, dstlen, get_smallint(c), 16);
			} else if ((c->flags & FLAG_INT_OCTAL) && !running) {
				dst += snprintf(dst, dstlen, "%s0o", get_smallint(c)<0?"-":"");
				dst += sprint_int(dst, dstlen, get_smallint(c), 8);
			} else
				dst += sprint_int(dst, dstlen, get_smallint(c), 10);
		} else
			dst += sprint_int(dst, dstlen, get_smallint(c), 10);

		if (dstlen) *dst = 0;
		return dst - save_dst;
	}

	if (is_real(c) && (get_real(c) == M_PI)) {
		dst += snprintf(dst, dstlen, "3.141592653589793");
		return dst - save_dst;
	}

	if (is_real(c) && (get_real(c) == M_E)) {
		dst += snprintf(dst, dstlen, "2.718281828459045");
		return dst - save_dst;
	}

	if (is_real(c)) {
		char tmpbuf[256];
		sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG, get_real(c));
		const char *ptr = strchr(tmpbuf, '.');

		if (ptr && (strlen(ptr+1) > 1))
			sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG, get_real(c));

		reformat_float(tmpbuf);
		dst += snprintf(dst, dstlen, "%s", tmpbuf);
		return dst - save_dst;
	}

	pl_idx_t var_nbr = 0;

	if (running && is_variable(c) && q->variable_names) {
		cell *l = q->variable_names;
		pl_idx_t l_ctx = q->variable_names_ctx;
		LIST_HANDLER(l);

		while (is_iso_list(l)) {
			cell *h = LIST_HEAD(l);
			h = deref(q, h, l_ctx);
			pl_idx_t h_ctx = q->latest_ctx;
			cell *name = h+1;
			name = deref(q, name, h_ctx);
			cell *var = h+2;
			var = deref(q, var, h_ctx);
			pl_idx_t var_ctx = q->latest_ctx;

			if (is_variable(var) && (var->var_nbr == c->var_nbr) && (var_ctx = c_ctx)) {
				dst += snprintf(dst, dstlen, "%s", GET_STR(q, name));
				return dst - save_dst;
			}

			l = LIST_TAIL(l);
			l = deref(q, l, l_ctx);
			l_ctx = q->latest_ctx;
		}
	}

	if (is_variable(c) && running && (q->nv_start == -1)
		&& ((var_nbr = find_binding(q, c->var_nbr, c_ctx)) != ERR_IDX)) {
#if 0
		for (unsigned i = 0; i < MAX_ARITY && var_nbr; i++) {
			if (q->nv_mask[i])
				break;

			var_nbr--;
		}
#endif

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

		return dst - save_dst;
	}

	if (is_variable(c) && !running && !q->cycle_error) {
		dst += snprintf(dst, dstlen, "%s", GET_STR(q, c));
		return dst - save_dst;
	}

	if (is_variable(c)) {
		frame *f = GET_FRAME(c_ctx);
		slot *e = GET_SLOT(f, c->var_nbr);
		pl_idx_t slot_nbr = e - q->slots;
		dst += snprintf(dst, dstlen, "_%u", (unsigned)slot_nbr);
		return dst - save_dst;
	}

	if (is_string(c)) {
		unsigned cnt = 0;
		cell *l = c;
		LIST_HANDLER(l);

		while (is_list(l)) {
			if (q->max_depth && (cnt >= q->max_depth)) {
				dst += snprintf(dst, dstlen, ",...");
				return dst - save_dst;
			}

			cell *h = LIST_HEAD(l);
			l = LIST_TAIL(l);
			h->flags &= ~FLAG_CSTR_STRING;

			if (!cnt++)
				allocate_list(q, h);
			else
				append_list(q, h);
		}

		c = end_list(q);
	}

	const char *src = GET_STR(q, c);
	size_t srclen = LEN_STR(q, c);
	int dq = 0, quote = !is_variable(c) && needs_quoting(q->st.m, src, srclen);
	quote += has_spaces(src, srclen);
	if (is_string(c)) dq = quote = 1;
	dst += snprintf(dst, dstlen, "%s", quote?dq?"\"":"'":"");

	if (quote)
		dst += formatted(dst, dstlen, src, srclen, dq);
	else
		dst += plain(dst, dstlen, src, srclen);

	dst += snprintf(dst, dstlen, "%s", quote?dq?"\"":"'":"");

	if (!is_structure(c))
		return dst - save_dst;

	pl_idx_t arity = c->arity;
	dst += snprintf(dst, dstlen, "(");

	for (c++; arity--; c += c->nbr_cells) {

		cell *tmp = running ? deref(q, c, c_ctx) : c;
		ssize_t res = print_canonical_to_buf(q, dst, dstlen, tmp, q->latest_ctx, running, cons, depth+1);
		if (res < 0) return -1;
		dst += res;

		if (q->max_depth && ((depth+1) >= q->max_depth)) {
			dst += snprintf(dst, dstlen, ",...)");
			return dst - save_dst;
		}

		if (arity)
			dst += snprintf(dst, dstlen, ",");
	}

	dst += snprintf(dst, dstlen, ")");
	return dst - save_dst;
}

static const char *varformat(unsigned nbr)
{
	static char tmpbuf[80];
	char *dst = tmpbuf;
	dst += sprintf(dst, "%c", 'A'+nbr%26);
	if ((nbr/26) > 0) dst += sprintf(dst, "%u", (nbr/26)%26);
	if ((nbr/26/26) > 0) sprintf(dst, "_%u", (nbr/26/26)%26);
	return tmpbuf;
}

static const char *get_slot_name(query *q, pl_idx_t slot_idx)
{
	for (unsigned i = 0; i < q->pl->tab_idx; i++) {
		if (q->pl->tab1[i] == slot_idx) {
			unsigned offset = 0;

			while (q->ignore[i+offset])
				offset++;

			q->ignore[i+offset] = true;
			return varformat(i+offset);
		}
	}

	unsigned i = q->pl->tab_idx++;
	q->pl->tab1[i] = slot_idx;
	const char *s = varformat(i);
	//fprintf(stderr, "%u => %u => %s\n", slot_idx, i, s);
	return s;
}

ssize_t print_term_to_buf(query *q, char *dst, size_t dstlen, cell *c, pl_idx_t c_ctx, int running, bool cons, unsigned depth)
{
	char *save_dst = dst;

	if (depth > MAX_DEPTH) {
		q->cycle_error = true;
		return -1;
	}

#if 0
	if (q->is_dump_vars && is_stream(c)) {
		dst += snprintf(dst, dstlen, "'$stream'(%d)", (int)get_smallint(c));
		return dst - save_dst;
	}
#endif

	if (is_bigint(c)) {
		int radix = 10;

		if (q->listing) {
			if (q->listing) {
				if (c->flags & FLAG_INT_BINARY)
					radix = 2;
				else if (c->flags & FLAG_INT_HEX)
					radix = 16;
				else if ((c->flags & FLAG_INT_OCTAL) && !running)
					radix = 8;
			}

			if (c->flags & FLAG_INT_BINARY)
				dst += snprintf(dst, dstlen, "%s0b", is_negative(c)?"-":"");
			else if (c->flags & FLAG_INT_HEX)
				dst += snprintf(dst, dstlen, "%s0x", is_negative(c)?"-":"");
			else if ((c->flags & FLAG_INT_OCTAL) && !running)
				dst += snprintf(dst, dstlen, "%s0o", is_negative(c)?"-":"");
		}

		if (!dstlen)
			dst += mp_int_string_len(&c->val_bigint->ival, radix) - 1;
		else {
			size_t len = mp_int_string_len(&c->val_bigint->ival, radix) - 1;
			mp_int_to_string(&c->val_bigint->ival, radix, dst, len+1);
			dst += strlen(dst);
		}

		if (dstlen) *dst = 0;
		return dst - save_dst;
	}

	if (is_smallint(c)) {
		if (q->listing) {
			if (((c->flags & FLAG_INT_HEX) || (c->flags & FLAG_INT_BINARY))) {
				dst += snprintf(dst, dstlen, "%s0x", get_smallint(c)<0?"-":"");
				dst += sprint_int(dst, dstlen, get_smallint(c), 16);
			} else if ((c->flags & FLAG_INT_OCTAL) && !running) {
				dst += snprintf(dst, dstlen, "%s0o", get_smallint(c)<0?"-":"");
				dst += sprint_int(dst, dstlen, get_smallint(c), 8);
			} else
				dst += sprint_int(dst, dstlen, get_smallint(c), 10);
		} else
			dst += sprint_int(dst, dstlen, get_smallint(c), 10);

		if (dstlen) *dst = 0;
		return dst - save_dst;
	}

	if (is_real(c) && (get_real(c) == M_PI)) {
		dst += snprintf(dst, dstlen, "3.141592653589793");
		return dst - save_dst;
	}

	if (is_real(c) && (get_real(c) == M_E)) {
		dst += snprintf(dst, dstlen, "2.718281828459045");
		return dst - save_dst;
	}

	if (is_real(c)) {
		char tmpbuf[256];
		sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG-1, get_real(c));
		const char *ptr = strchr(tmpbuf, '.');

		if (ptr && (strlen(ptr+1) > 1))
			sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG-1, get_real(c));

		reformat_float(tmpbuf);
		dst += snprintf(dst, dstlen, "%s", tmpbuf);
		return dst - save_dst;
	}

	int is_chars_list = is_string(c);

	if (!is_chars_list && running)
		is_chars_list += q->st.m->flag.double_quote_chars && scan_is_chars_list(q, c, c_ctx, false);

	if (is_string(c)) {
		dst += snprintf(dst, dstlen, "%s", "\"");
		dst += formatted(dst, dstlen, GET_STR(q, c), LEN_STR(q, c), true);
		dst += snprintf(dst, dstlen, "%s", "\"");
		return dst - save_dst;
	} else if (is_chars_list) {
		cell *l = c;
		dst += snprintf(dst, dstlen, "%s", "\"");
		unsigned cnt = 0;
		LIST_HANDLER(l);

		while (is_list(l)) {
			if (q->max_depth && (cnt++ >= q->max_depth)) {
				dst += snprintf(dst, dstlen, "%s", "...");
				break;
			}

			cell *h = LIST_HEAD(l);
			cell *c = running ? deref(q, h, c_ctx) : h;
			dst += formatted(dst, dstlen, GET_STR(q, c), LEN_STR(q, c), true);
			l = LIST_TAIL(l);
			l = running ? deref(q, l, c_ctx) : l;
			c_ctx = q->latest_ctx;
		}

		dst += snprintf(dst, dstlen, "%s", "\"");
		return dst - save_dst;
	}

	// FIXME make non-recursive

	const char *src = GET_STR(q, c);
	unsigned print_list = 0, cnt = 0;

	while (is_iso_list(c)) {
		if (q->max_depth && (cnt++ >= q->max_depth)) {
			dst--;
			dst += snprintf(dst, dstlen, "%s", ",...]");
			return dst - save_dst;
		}

		if (q->max_depth && (depth >= q->max_depth)) {
			dst--;
			dst += snprintf(dst, dstlen, "%s", ",...]");
			return dst - save_dst;
		}

		LIST_HANDLER(c);
		cell *head = LIST_HEAD(c);

		if (!cons)
			dst += snprintf(dst, dstlen, "%s", "[");

		head = running ? deref(q, head, c_ctx) : head;
		pl_idx_t head_ctx = q->latest_ctx;
		bool special_op = false;

		if (is_literal(head)) {
			special_op = (
				!strcmp(GET_STR(q, head), ",")
				|| !strcmp(GET_STR(q, head), "|")
				|| !strcmp(GET_STR(q, head), ";")
				|| !strcmp(GET_STR(q, head), "->")
				|| !strcmp(GET_STR(q, head), "*->")
				|| !strcmp(GET_STR(q, head), "-->"));
		}

		int parens = is_structure(head) && special_op;
		if (parens) dst += snprintf(dst, dstlen, "%s", "(");
		ssize_t res = print_term_to_buf(q, dst, dstlen, head, head_ctx, running, false, depth+1);
		if (res < 0) return -1;
		dst += res;
		if (parens) dst += snprintf(dst, dstlen, "%s", ")");

		cell *tail = LIST_TAIL(c);
		tail = running ? deref(q, tail, c_ctx) : tail;
		c_ctx = q->latest_ctx;
		size_t tmp_len = 0;

		if (is_literal(tail) && !is_structure(tail)) {
			src = GET_STR(q, tail);

			if (strcmp(src, "[]")) {
				dst += snprintf(dst, dstlen, "%s", "|");
				ssize_t res = print_term_to_buf(q, dst, dstlen, tail, c_ctx, running, true, depth+1);
				if (res < 0) return -1;
				dst += res;
			}
		} else if (q->st.m->flag.double_quote_chars && running
			&& (tmp_len = scan_is_chars_list(q, tail, c_ctx, false)) > 0) {
			char *tmp_src = chars_list_to_string(q, tail, c_ctx, tmp_len);

			if ((strlen(tmp_src) == 1) && (*tmp_src == '\''))
				dst += snprintf(dst, dstlen, "|\"%s\"", tmp_src);
			else if ((strlen(tmp_src) == 1) && needs_quoting(q->st.m, tmp_src, 1))
				dst += snprintf(dst, dstlen, ",'%s'", tmp_src);
			else if (strlen(tmp_src) == 1)
				dst += snprintf(dst, dstlen, ",%s", tmp_src);
			else
				dst += snprintf(dst, dstlen, "|\"%s\"", tmp_src);

			free(tmp_src);
			print_list++;
		} else if (is_iso_list(tail)) {
			dst += snprintf(dst, dstlen, "%s", ",");
			c = tail;
			print_list++;
			cons = 1;
			continue;
		} else if (is_string(tail)) {
			dst+= snprintf(dst, dstlen, "%s", "|\"");
			dst += formatted(dst, dstlen, GET_STR(q, tail), LEN_STR(q, tail), true);
			dst += snprintf(dst, dstlen, "%s", "\"");
			print_list++;
		} else {
			dst += snprintf(dst, dstlen, "%s", "|");
			ssize_t res = print_term_to_buf(q, dst, dstlen, tail, c_ctx, running, true, depth+1);
			if (res < 0) return -1;
			dst += res;
		}

		if (!cons || print_list)
			dst += snprintf(dst, dstlen, "%s", "]");

		return dst - save_dst;
	}

	int optype = GET_OP(c);
	unsigned specifier;

	if (!optype && !is_variable(c)
		&& search_op(q->st.m, GET_STR(q, c), &specifier, true) && (c->arity == 1)) {
		if (IS_PREFIX(specifier)) {
			SET_OP(c, specifier);
			optype = specifier;
		}
	}

	if (q->ignore_ops || !optype || !c->arity) {
		int quote = ((running <= 0) || q->quoted) && !is_variable(c) && needs_quoting(q->st.m, src, LEN_STR(q, c));
		int dq = 0, braces = 0;
		if (is_string(c)) dq = quote = 1;
		if (q->quoted < 0) quote = 0;
		if ((c->arity == 1) && is_literal(c) && !strcmp(src, "{}")) braces = 1;

		if (running && is_literal(c) && !strcmp(src, "$VAR")
			&& q->numbervars && (!q->is_dump_vars || depth) && is_integer(c+1)) {
			unsigned var_nbr = get_smallint(c+1) - q->nv_start;
			dst += snprintf(dst, dstlen, "%s", varformat(var_nbr));
			return dst - save_dst;
		}

		if (running && is_variable(c) && q->variable_names) {
			cell *l = q->variable_names;
			pl_idx_t l_ctx = q->variable_names_ctx;
			LIST_HANDLER(l);

			while (is_iso_list(l)) {
				cell *h = LIST_HEAD(l);
				h = deref(q, h, l_ctx);
				pl_idx_t h_ctx = q->latest_ctx;
				cell *name = h+1;
				name = deref(q, name, h_ctx);
				cell *var = h+2;
				var = deref(q, var, h_ctx);
				pl_idx_t var_ctx = q->latest_ctx;

				if (is_variable(var) && (var->var_nbr == c->var_nbr) && (var_ctx == c_ctx)) {
					dst += snprintf(dst, dstlen, "%s", GET_STR(q, name));
					return dst - save_dst;
				}

				l = LIST_TAIL(l);
				l = deref(q, l, l_ctx);
				l_ctx = q->latest_ctx;
			}
		}

		dst += snprintf(dst, dstlen, "%s", !braces&&quote?dq?"\"":"'":"");

		if (is_variable(c)) {
			frame *f = GET_FRAME(c_ctx);
			slot *e = GET_SLOT(f, c->var_nbr);
			pl_idx_t slot_idx = e - q->slots;

			if (q->is_dump_vars) {
				const char *name = get_slot_name(q, slot_idx);
				dst += snprintf(dst, dstlen, "_%s", name);
			} else if (!running) {
				dst += snprintf(dst, dstlen, "%s", GET_STR(q, c));
			} else
				dst += snprintf(dst, dstlen, "_%u", (unsigned)slot_idx);

			return dst - save_dst;
		}

		if (is_variable(c)) {
			dst += snprintf(dst, dstlen, "_%u", c->var_nbr);
			return dst - save_dst;
		}

		unsigned len_str = LEN_STR(q, c);

		if (braces)
			;
		else if (quote) {
			if (is_blob(c) && q->max_depth && (len_str >= q->max_depth) && (LEN_STR(q,c) > 128))
				len_str = q->max_depth;

			dst += formatted(dst, dstlen, src, len_str, dq);

			if (is_blob(c) && q->max_depth && (len_str >= q->max_depth) && (LEN_STR(q,c) > 128)) {
				dst--;
				dst += snprintf(dst, dstlen, "%s", ",...");
			}
		} else
			dst += plain(dst, dstlen, src, len_str);

		dst += snprintf(dst, dstlen, "%s", !braces&&quote?dq?"\"":"'":"");
		q->did_quote = !braces&&quote;

		if (is_structure(c) && !is_string(c)) {
			pl_idx_t arity = c->arity;
			dst += snprintf(dst, dstlen, "%s", braces?"{":"(");

			for (c++; arity--; c += c->nbr_cells) {
				cell *tmp = running ? deref(q, c, c_ctx) : c;
				pl_idx_t tmp_ctx = q->latest_ctx;
				int parens = 0;

				if (!braces && is_literal(tmp)) {
					const char *s = GET_STR(q, tmp);

					if (!strcmp(s, ","))
						parens = 1;
				}

				if (parens)
					dst += snprintf(dst, dstlen, "%s", "(");

				if (q->max_depth && ((depth+1) >= q->max_depth)) {
					dst += snprintf(dst, dstlen, "...)");
					return dst - save_dst;
				}

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

		return dst - save_dst;
	}

	size_t srclen = LEN_STR(q, c);

	if (CELL_POSTFIX(c)) {
		cell *lhs = c + 1;
		lhs = running ? deref(q, lhs, c_ctx) : lhs;
		pl_idx_t lhs_ctx = q->latest_ctx;
		ssize_t res = print_term_to_buf(q, dst, dstlen, lhs, lhs_ctx, running, 0, depth+1);
		if (res < 0) return -1;
		dst += res;

		bool space = (c->val_off == g_minus_s) && (is_number(lhs) || search_op(q->st.m, GET_STR(q, lhs), NULL, true));
		if ((c->val_off == g_plus_s) && search_op(q->st.m, GET_STR(q, lhs), NULL, true) && lhs->arity) space = true;
		if (isalpha(*src)) space = true;
		if (space) dst += snprintf(dst, dstlen, "%s", " ");

		int quote = q->quoted && has_spaces(src, LEN_STR(q,c));
		if (quote) dst += snprintf(dst, dstlen, "%s", quote?"'":"");

		dst += plain(dst, dstlen, src, srclen);
		if (quote) dst += snprintf(dst, dstlen, "%s", quote?"'":"");
		return dst - save_dst;
	}

	if (CELL_PREFIX(c)) {
		cell *rhs = c + 1;
		rhs = running ? deref(q, rhs, c_ctx) : rhs;
		pl_idx_t rhs_ctx = q->latest_ctx;
		unsigned my_priority = search_op(q->st.m, GET_STR(q, c), NULL, true);
		unsigned rhs_pri = is_literal(rhs) ? search_op(q->st.m, GET_STR(q, rhs), NULL, true) : 0;

		bool space = (c->val_off == g_minus_s) && (is_number(rhs) || search_op(q->st.m, GET_STR(q, rhs), NULL, true));
		if ((c->val_off == g_plus_s) && search_op(q->st.m, GET_STR(q, rhs), NULL, true) && rhs->arity) space = true;
		if (isalpha(*src)) space = true;

		bool parens = false; //is_op(rhs);
		if (rhs_pri > my_priority) parens = true;
		if (!strcmp(src, "-") && (rhs_pri == my_priority) && (rhs->arity > 1)) parens = true;
		//if (strcmp(GET_STR(q, c), "\\+")) if (is_atomic(rhs)) parens = false; // Hack
		if ((c->val_off == g_minus_s) && is_number(rhs) && !is_negative(rhs)) parens = true;
		if ((c->val_off == g_minus_s) && search_op(q->st.m, GET_STR(q, rhs), NULL, true) && !rhs->arity) parens = true;
		if ((c->val_off == g_plus_s) && search_op(q->st.m, GET_STR(q, rhs), NULL, true) && !rhs->arity) parens = true;

		bool quote = q->quoted && has_spaces(src, LEN_STR(q,c));

		if (quote) dst += snprintf(dst, dstlen, "%s", quote?"'":"");
		dst += plain(dst, dstlen, src, srclen);
		if (quote) dst += snprintf(dst, dstlen, "%s", quote?"'":"");
		if (space) dst += snprintf(dst, dstlen, "%s", " ");
		if (parens) dst += snprintf(dst, dstlen, "%s", "(");
		ssize_t res = print_term_to_buf(q, dst, dstlen, rhs, rhs_ctx, running, 0, depth+1);
		if (res < 0) return -1;
		dst += res;
		if (parens) dst += snprintf(dst, dstlen, "%s", ")");
		return dst - save_dst;
	}

	// Infix...

	cell *lhs = c + 1;
	cell *rhs = lhs + lhs->nbr_cells;
	lhs = running ? deref(q, lhs, c_ctx) : lhs;
	pl_idx_t lhs_ctx = q->latest_ctx;
	rhs = running ? deref(q, rhs, c_ctx) : rhs;
	pl_idx_t rhs_ctx = q->latest_ctx;

	unsigned lhs_pri_1 = is_literal(lhs) ? search_op(q->st.m, GET_STR(q, lhs), NULL, false) : 0;
	unsigned lhs_pri_2 = is_literal(lhs) && !lhs->arity ? search_op(q->st.m, GET_STR(q, lhs), NULL, false) : 0;
	unsigned rhs_pri_1 = is_literal(rhs) ? search_op(q->st.m, GET_STR(q, rhs), NULL, false) : 0;
	unsigned rhs_pri_2 = is_literal(rhs) && !rhs->arity ? search_op(q->st.m, GET_STR(q, rhs), NULL, false) : 0;
	unsigned my_priority = search_op(q->st.m, GET_STR(q, c), NULL, false);

	bool lhs_parens = lhs_pri_1 >= my_priority;
	if ((lhs_pri_1 == my_priority) && IS_YFX(c)) lhs_parens = false;
	if (lhs_pri_2 > 0) lhs_parens = true;
	if (is_structure(lhs) && (lhs_pri_1 <= my_priority) && (lhs->val_off == g_plus_s)) { lhs_parens = false; }

	if (lhs_parens) dst += snprintf(dst, dstlen, "%s", "(");
	ssize_t res = print_term_to_buf(q, dst, dstlen, lhs, lhs_ctx, running, 0, depth+1);
	if (res < 0) return -1;
	dst += res;
	if (lhs_parens) dst += snprintf(dst, dstlen, "%s", ")");

	int space = iswalpha(peek_char_utf8(src))
		|| iswspace(*src)
		|| !strcmp(src, ":-")
		|| !strcmp(src, "-->")
		|| !strcmp(src, "->")
		|| !strcmp(src, "*->")
		|| !strcmp(src, "=~=")
		|| !strcmp(src, "=..")
		|| !strcmp(src, "=>")
		|| !strcmp(src, "?=")
//		|| (*src == '#')
		|| !*src;
	if (space) dst += snprintf(dst, dstlen, "%s", " ");

	int quote = q->quoted && has_spaces(src, LEN_STR(q,c));
	if (op_needs_quoting(q->st.m, GET_STR(q, c), LEN_STR(q, c))) quote = 1;
	if (quote) dst += snprintf(dst, dstlen, "%s", quote?"'":"");
	dst += plain(dst, dstlen, src, srclen);
	if (quote) dst += snprintf(dst, dstlen, "%s", quote?"'":"");

	if ((strchr(src, '=') || strchr(src, '+') || strchr(src, '#')) &&
		((*GET_STR(q, rhs) == '-')
			|| (*GET_STR(q, rhs) == '*')
			|| (*GET_STR(q, rhs) == '+')
			|| (*GET_STR(q, rhs) == '~')
			|| (*GET_STR(q, rhs) == '?') ||
			(*GET_STR(q, rhs) == '#')))
		space = 1;

	if (!*src) space = 0;
	space += is_smallint(rhs) && is_negative(rhs);

	bool rhs_parens = rhs_pri_1 >= my_priority;
	if ((rhs_pri_1 == my_priority) && IS_XFY(c)) rhs_parens = false;
	if (rhs_pri_2 > 0) rhs_parens = true;
	if (is_structure(rhs) && (rhs_pri_1 <= my_priority)
		&& ((rhs->val_off == g_plus_s) || (rhs->val_off == g_minus_s))) { rhs_parens = false; space = true; }

	if (space) dst += snprintf(dst, dstlen, "%s", " ");

	if (rhs_parens) dst += snprintf(dst, dstlen, "%s", "(");
	res = print_term_to_buf(q, dst, dstlen, rhs, rhs_ctx, running, 0, depth+1);
	if (res < 0) return -1;
	dst += res;
	if (rhs_parens) dst += snprintf(dst, dstlen, "%s", ")");

	return dst - save_dst;
}

char *print_canonical_to_strbuf(query *q, cell *c, pl_idx_t c_ctx, int running)
{
	ssize_t len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, running, false, 0);

	if (len < 0) {
		running = 0;
		len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, running, false, 1);
	}

	char *buf = malloc(len+10);
	ensure(buf);
	len = print_canonical_to_buf(q, buf, len+1, c, c_ctx, running, false, 0);
	return buf;
}

pl_status print_canonical_to_stream(query *q, stream *str, cell *c, pl_idx_t c_ctx, int running)
{
	ssize_t len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, running, false, 0);

	if (len < 0) {
		running = 0;
		len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, running, false, 1);
	}

	char *dst = malloc(len*2+1); //cehteh: why *2?
	may_ptr_error(dst);
	len = print_canonical_to_buf(q, dst, len+1, c, c_ctx, running, false, 0);
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
			return pl_error;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
	return pl_success;
}

pl_status print_canonical(query *q, FILE *fp, cell *c, pl_idx_t c_ctx, int running)
{
	ssize_t len = 0;

	if (!running || is_cyclic_term(q, c, c_ctx)) {
		len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, running=0, false, 1);
	} else {
		len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, running, false, 0);
		q->did_quote = false;
	}

	char *dst = malloc(len*2+1); //cehteh: why *2?
	may_ptr_error(dst);
	len = print_canonical_to_buf(q, dst, len+1, c, c_ctx, running, false, 0);
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
			return pl_error;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
	return pl_success;
}

char *print_term_to_strbuf(query *q, cell *c, pl_idx_t c_ctx, int running)
{
	ssize_t len = 0;

	if (!running || is_cyclic_term(q, c, c_ctx)) {
		len = print_term_to_buf(q, NULL, 0, c, c_ctx, running=0, false, 1);
	} else {
		len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, false, 0);
		q->did_quote = false;
	}

	char *buf = malloc(len+10);
	ensure(buf);
	len = print_term_to_buf(q, buf, len+1, c, c_ctx, running, false, 0);
	return buf;
}

pl_status print_term_to_stream(query *q, stream *str, cell *c, pl_idx_t c_ctx, int running)
{
	ssize_t len = 0;

	if (!running || is_cyclic_term(q, c, c_ctx)) {
		len = print_term_to_buf(q, NULL, 0, c, c_ctx, running=0, false, 1);
	} else {
		len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, false, 0);
		q->did_quote = false;
	}

	char *dst = malloc(len+10);
	may_ptr_error(dst);
	len = print_term_to_buf(q, dst, len+1, c, c_ctx, running, false, 0);
	const char *src = dst;

	while (len) {
		size_t nbytes = net_write(src, len, str);

		if (feof(str->fp)) {
			q->error = true;
			free(dst);
			return pl_error;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
	return pl_success;
}

pl_status print_term(query *q, FILE *fp, cell *c, pl_idx_t c_ctx, int running)
{
	ssize_t len = 0;

	if (!running || is_cyclic_term(q, c, c_ctx)) {
		len = print_term_to_buf(q, NULL, 0, c, c_ctx, running=0, false, 1);
	} else {
		len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, false, 0);
		q->did_quote = false;
	}

	char *dst = malloc(len+10);
	may_ptr_error(dst);
	len = print_term_to_buf(q, dst, len+1, c, c_ctx, running, false, 0);
	const char *src = dst;

	while (len) {
		size_t nbytes = fwrite(src, 1, len, fp);

		if (feof(fp)) {
			q->error = true;
			free(dst);
			return pl_error;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
	return pl_success;
}

void clear_write_options(query *q)
{
	q->max_depth = q->quoted = 0;
	q->nl = q->fullstop = q->varnames = q->ignore_ops = q->numbervars = false;
	q->variable_names = NULL;
}
