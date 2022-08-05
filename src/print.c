#include <ctype.h>
#include <float.h>
#include <inttypes.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "heap.h"
#include "module.h"
#include "network.h"
#include "parser.h"
#include "query.h"
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

	if (!strcmp(src, "{}") || !strcmp(src, "[]")
		|| !strcmp(src, "!") || !strcmp(src, ";")
		|| !strcmp(src, "\\")	// ???????
		)
		return false;

	if ((src[0] == '/') && (src[1] == '*'))
		return true;

	int ch = peek_char_utf8(src);

	if (!isalnum(ch) && strchr(src, '_'))
		return true;

	if (iswupper(ch) || isdigit(ch) || (ch == '_'))
		return true;

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

			size_t n = snprintf(dst, dstlen, "%x", ch);
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
				dst += put_char_utf8(dst, ch);

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

static int find_binding(query *q, pl_idx_t var_nbr, pl_idx_t tmp_ctx)
{
	const frame *f = GET_FRAME(q->st.curr_frame);
	const slot *e = GET_FIRST_SLOT(f);

	for (pl_idx_t i = 0; i < f->nbr_vars; i++, e++) {
		if (!is_variable(&e->c))
			continue;

		if (e->c.var_ctx != tmp_ctx)
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

#if 1
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

	if (is_float(c) && (get_float(c) == M_PI)) {
		dst += snprintf(dst, dstlen, "3.141592653589793");
		return dst - save_dst;
	}

	if (is_float(c) && (get_float(c) == M_E)) {
		dst += snprintf(dst, dstlen, "2.718281828459045");
		return dst - save_dst;
	}

	if (is_float(c)) {
		char tmpbuf[256];
		sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG, get_float(c));
		const char *ptr = strchr(tmpbuf, '.');

		if (ptr && (strlen(ptr+1) > 1))
			sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG, get_float(c));

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
			CHECK_INTERRUPT();
			cell *h = LIST_HEAD(l);
			h = deref(q, h, l_ctx);
			pl_idx_t h_ctx = q->latest_ctx;
			cell *name = deref(q, h+1, h_ctx);
			cell *var = h+2;
			pl_idx_t tmp_ctx = h_ctx;

			if (!q->is_dump_vars) {
				var = deref(q, var, h_ctx);
				tmp_ctx = q->latest_ctx;
			}

			if (is_variable(var) && (var->var_nbr == c->var_nbr) && (tmp_ctx = c_ctx)) {
				dst += snprintf(dst, dstlen, "%s", C_STR(q, name));
				return dst - save_dst;
			}

			l = LIST_TAIL(l);
			l = deref(q, l, l_ctx);
			l_ctx = q->latest_ctx;
		}
	}

	if (is_variable(c) && running && (q->nv_start == -1)
		&& ((var_nbr = find_binding(q, c->var_nbr, c_ctx)) != ERR_IDX)) {
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

	if (is_variable(c) && !running && !q->cycle_error && !is_ref(c)) {
		dst += snprintf(dst, dstlen, "%s", C_STR(q, c));
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

	if (is_iso_list(c)) {
		LIST_HANDLER(c);
		unsigned cnt = 0;

		while (is_iso_list(c)) {
			CHECK_INTERRUPT();
			cell *save_c = c;
			pl_idx_t save_c_ctx = c_ctx;

			if (q->max_depth && (cnt >= q->max_depth)) {
				dst += snprintf(dst, dstlen, ",...");
				break;
			}

			dst += snprintf(dst, dstlen, "'.'(");
			cnt++;

			cell *h = LIST_HEAD(c);
			h = deref(q, h, c_ctx);
			pl_idx_t h_ctx = q->latest_ctx;

			ssize_t res = print_canonical_to_buf(q, dst, dstlen, h, h_ctx, running, cons, depth+1);
			if (res < 0) return -1;
			dst += res;

			c = LIST_TAIL(c);
			c = deref(q, c, c_ctx);
			c_ctx = q->latest_ctx;

			if (is_iso_list(c)) {
				if ((c == save_c) && (c_ctx == save_c_ctx) /*&& running*/) {
					dst += snprintf(dst, dstlen, ",");
					dst += snprintf(dst, dstlen, "%s", running ? C_STR(q, save_c) : "_");

					while (cnt--)
						dst += snprintf(dst, dstlen, ")");

					return dst - save_dst;
				}
			}

			dst += snprintf(dst, dstlen, ",");
		}

		ssize_t res = print_canonical_to_buf(q, dst, dstlen, c, c_ctx, running, cons, depth+1);
		if (res < 0) return -1;
		dst += res;

		while (cnt--)
			dst += snprintf(dst, dstlen, ")");

		return dst - save_dst;
	}

	const char *src = C_STR(q, c);
	size_t srclen = C_STRLEN(q, c);
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

	unsigned cnt = 0;

	for (c++; arity--; c += c->nbr_cells, cnt++) {
		CHECK_INTERRUPT();
		cell *tmp = running ? deref(q, c, c_ctx) : c;
		ssize_t res = print_canonical_to_buf(q, dst, dstlen, tmp, q->latest_ctx, running, cons, depth+1);
		if (res < 0) return -1;
		dst += res;

		if (q->max_depth && ((depth+1) >= q->max_depth)) {
			dst += snprintf(dst, dstlen, ",...)");
			return dst - save_dst;
		}

		if (q->max_depth && (cnt >= q->max_depth)) {
			dst += snprintf(dst, dstlen, "...)");
			return dst - save_dst;
		}

		if (arity)
			dst += snprintf(dst, dstlen, ",");
	}

	dst += snprintf(dst, dstlen, ")");
	return dst - save_dst;
}

static const char *varformat2(cell *c, unsigned nv_start)
{
	mpz_t tmp;

	if (is_smallint(c))
		mp_int_init_value(&tmp, c->val_int);
	else
		mp_int_init_copy(&tmp, &c->val_bigint->ival);

	mp_small nbr;
	mp_int_mod_value(&tmp, 26, &nbr);
	static char tmpbuf[8192];
	char *dst = tmpbuf;
	dst += sprintf(dst, "%c", 'A'+(unsigned)(nbr));
	mp_int_div_value(&tmp, 26, &tmp, NULL);

	if (mp_int_compare_zero(&tmp) > 0)
		dst += mp_int_to_string(&tmp, 10, dst, sizeof(tmpbuf));

	mp_int_clear(&tmp);
	return tmpbuf;
}

static const char *varformat(unsigned long long nbr)
{
	static char tmpbuf[80];
	char *dst = tmpbuf;
	dst += sprintf(dst, "%c", 'A'+(unsigned)(nbr%26));
	if ((nbr/26) > 0) dst += sprintf(dst, "%llu", (nbr/26));
	return tmpbuf;
}

static const char *get_slot_name(query *q, pl_idx_t slot_idx)
{
	for (unsigned i = 0; i < q->print_idx; i++) {
		if (q->pl->tab1[i] == slot_idx) {
			return varformat(q->pl->tab2[i]);
		}
	}

	unsigned j, i = q->print_idx++;
	q->pl->tab1[i] = slot_idx;

	for (j = 0; j < MAX_IGNORES; j++) {
		if (!q->ignores[j]) {
			q->ignores[j] = true;
			break;
		}
	}

	q->pl->tab2[i] = j;
	return varformat(i);
}

ssize_t print_variable(query *q, char *dst, size_t dstlen, cell *c, pl_idx_t c_ctx, bool running)
{
	char *save_dst = dst;
	frame *f = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(f, c->var_nbr);
	pl_idx_t slot_idx = e - q->slots;

	if (q->varnames && !is_fresh(c) && !is_anon(c) /*&& c->val_off*/ && !e->c.attrs && !is_ref(c)) {
		dst += snprintf(dst, dstlen, "%s", C_STR(q, c));
	} if (q->varnames && !is_fresh(c) && !is_anon(c)) {
		dst += snprintf(dst, dstlen, "%s", q->p->vartab.var_name[c->var_nbr]);
	} else if (q->is_dump_vars) {
		dst += snprintf(dst, dstlen, "_%s", get_slot_name(q, slot_idx));
	} else if (!running && !is_ref(c)) {
		dst += snprintf(dst, dstlen, "%s", C_STR(q, c));
	} else
		dst += snprintf(dst, dstlen, "_%u", (unsigned)slot_idx);

	return dst - save_dst;
}

static ssize_t print_iso_list(query *q, char *save_dst, char *dst, size_t dstlen, cell *c, pl_idx_t c_ctx, int running, bool cons, unsigned depth)
{
	unsigned print_list = 0, cnt = 0;

	while (is_iso_list(c)) {
		CHECK_INTERRUPT();
		cell *save_c = c;
		pl_idx_t save_c_ctx = c_ctx;

		if (q->max_depth && (print_list >= q->max_depth)) {
			dst--;
			dst += snprintf(dst, dstlen, "%s", ",...]");
			q->last_thing_was_symbol = false;
			return dst - save_dst;
		}

		LIST_HANDLER(c);
		cell *head = LIST_HEAD(c);

		if (!cons)
			dst += snprintf(dst, dstlen, "%s", "[");

		head = running ? deref(q, head, c_ctx) : head;
		pl_idx_t head_ctx = q->latest_ctx;
		bool special_op = false;

		if (is_interned(head)) {
			special_op = (
				!strcmp(C_STR(q, head), ",")
				|| !strcmp(C_STR(q, head), "|")
				|| !strcmp(C_STR(q, head), ";")
				|| !strcmp(C_STR(q, head), ":-")
				|| !strcmp(C_STR(q, head), "->")
				|| !strcmp(C_STR(q, head), "*->")
				|| !strcmp(C_STR(q, head), "-->"));
		}

		int parens = is_structure(head) && special_op;
		if (parens) dst += snprintf(dst, dstlen, "%s", "(");
		q->parens = parens;
		ssize_t res = print_term_to_buf(q, dst, dstlen, head, head_ctx, running, false, depth+1);
		q->parens = false;
		if (res < 0) return -1;
		dst += res;
		if (parens) dst += snprintf(dst, dstlen, "%s", ")");
		bool possible_chars = false;

		if (is_interned(head) && (C_STRLEN_UTF8(head) == 1))
			possible_chars = true;

		cell *tail = LIST_TAIL(c);
		cell *save_tail = tail;
		tail = running ? deref(q, tail, c_ctx) : tail;
		c_ctx = q->latest_ctx;
		size_t tmp_len = 0;

		if (is_interned(tail) && !is_structure(tail)) {
			const char *src = C_STR(q, tail);

			if (strcmp(src, "[]")) {
				dst += snprintf(dst, dstlen, "%s", "|");
				ssize_t res = print_term_to_buf(q, dst, dstlen, tail, c_ctx, running, true, depth+1);
				if (res < 0) return -1;
				dst += res;
			}
		} else if (q->st.m->flags.double_quote_chars && running
			&& possible_chars && !is_cyclic_term(q, c, c_ctx)
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
			if ((tail == save_c) && (c_ctx == save_c_ctx) && running) {
				dst += snprintf(dst, dstlen, "%s", "|");
				dst += snprintf(dst, dstlen, "%s", !is_ref(save_tail) ? C_STR(q, save_tail) : "_");
			} else {
				dst += snprintf(dst, dstlen, "%s", ",");
				c = tail;
				print_list++;
				cons = 1;
				continue;
			}
		} else if (is_string(tail)) {
			dst+= snprintf(dst, dstlen, "%s", "|\"");
			dst += formatted(dst, dstlen, C_STR(q, tail), C_STRLEN(q, tail), true);
			dst += snprintf(dst, dstlen, "%s", "\"");
			print_list++;
			q->last_thing_was_symbol = false;
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

	return dst - save_dst;
}

ssize_t print_term_to_buf(query *q, char *dst, size_t dstlen, cell *c, pl_idx_t c_ctx, int running, bool cons, unsigned depth)
{
	char *save_dst = dst;

	if (depth > MAX_DEPTH) {
		q->cycle_error = true;
		q->last_thing_was_symbol = false;
		return -1;
	}

#if 1
	if (q->is_dump_vars && is_stream(c)) {
		dst += snprintf(dst, dstlen, "'$stream'(%d)", (int)get_smallint(c));
		q->last_thing_was_symbol = false;
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
		q->last_thing_was_symbol = false;
		return dst - save_dst;
	}

	if (is_smallint(c)) {
		//if (dstlen) printf("*** int %d,  was=%d, is=%d\n", (int)c->val_int, q->last_thing_was_symbol, false);

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
		q->last_thing_was_symbol = false;
		return dst - save_dst;
	}

	if (is_float(c) && (get_float(c) == M_PI)) {
		dst += snprintf(dst, dstlen, "3.141592653589793");
		q->last_thing_was_symbol = false;
		return dst - save_dst;
	}

	if (is_float(c) && (get_float(c) == M_E)) {
		dst += snprintf(dst, dstlen, "2.718281828459045");
		q->last_thing_was_symbol = false;
		return dst - save_dst;
	}

	if (is_float(c)) {
		char tmpbuf[256];
		sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG-1, get_float(c));
		const char *ptr = strchr(tmpbuf, '.');

		if (ptr && (strlen(ptr+1) > 1))
			sprintf(tmpbuf, "%.*g", DBL_DECIMAL_DIG-1, get_float(c));

		reformat_float(tmpbuf);
		dst += snprintf(dst, dstlen, "%s", tmpbuf);
		q->last_thing_was_symbol = false;
		return dst - save_dst;
	}

	int is_chars_list = is_string(c);
	bool possible_chars = false;

	if (is_interned(c) && (C_STRLEN_UTF8(c) == 1))
		possible_chars = true;

	if (!is_chars_list && running
		&& possible_chars && !is_cyclic_term(q, c, c_ctx))
		is_chars_list += q->st.m->flags.double_quote_chars && scan_is_chars_list(q, c, c_ctx, false);

	if (is_string(c)) {
		dst += snprintf(dst, dstlen, "%s", "\"");
		dst += formatted(dst, dstlen, C_STR(q, c), C_STRLEN(q, c), true);
		dst += snprintf(dst, dstlen, "%s", "\"");
		q->last_thing_was_symbol = false;
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
			dst += formatted(dst, dstlen, C_STR(q, c), C_STRLEN(q, c), true);
			l = LIST_TAIL(l);
			l = running ? deref(q, l, c_ctx) : l;
			c_ctx = q->latest_ctx;
		}

		dst += snprintf(dst, dstlen, "%s", "\"");
		q->last_thing_was_symbol = false;
		return dst - save_dst;
	}

	if (is_iso_list(c)) {
		return print_iso_list(q, save_dst, dst, dstlen, c, c_ctx, running, cons, depth+1);
	}

	const char *src = !is_ref(c) ? C_STR(q, c) : "_";
	size_t src_len = !is_ref(c) ? C_STRLEN(q, c) : 1;
	int optype = GET_OP(c);
	unsigned specifier = 0, pri = 0;

	if (!optype && !is_variable(c)
		&& (pri = search_op(q->st.m, src, &specifier, true) && (c->arity == 1))) {
		if (IS_PREFIX(specifier)) {
			SET_OP(c, specifier);
			optype = specifier;
		}
	}

	if (q->ignore_ops || !optype || !c->arity) {
		int quote = ((running <= 0) || q->quoted) && !is_variable(c) && needs_quoting(q->st.m, src, src_len);
		int dq = 0, braces = 0;
		if (is_string(c)) dq = quote = 1;
		if (q->quoted < 0) quote = 0;
		if ((c->arity == 1) && is_interned(c) && !strcmp(src, "{}")) braces = 1;
		cell *c1 = c->arity ? deref(q, c+1, c_ctx) : NULL;

		if (running && is_interned(c) && c->arity && !strcmp(src, "$VAR") && c1
			&& q->numbervars && is_integer(c1) && q->nv_start != -1) {
			dst += snprintf(dst, dstlen, "%s", varformat2(c1, q->nv_start));
			q->last_thing_was_symbol = false;
			return dst - save_dst;
		}

		if (is_variable(c) && !is_anon(c) && q->variable_names) {
			cell *l = q->variable_names;
			pl_idx_t l_ctx = q->variable_names_ctx;
			LIST_HANDLER(l);

			while (is_iso_list(l)) {
				cell *h = LIST_HEAD(l);
				h = deref(q, h, l_ctx);
				pl_idx_t h_ctx = q->latest_ctx;
				cell *name = deref(q, h+1, h_ctx);
				cell *var = h+2;
				pl_idx_t tmp_ctx = h_ctx;

				if (!q->is_dump_vars) {
					var = deref(q, var, h_ctx);
					tmp_ctx = q->latest_ctx;
				}

				if (is_variable(var) && (var->var_nbr == c->var_nbr) && (tmp_ctx == c_ctx)) {
					dst += snprintf(dst, dstlen, "%s", C_STR(q, name));
					q->last_thing_was_symbol = false;
					return dst - save_dst;
				}

				l = LIST_TAIL(l);
				l = deref(q, l, l_ctx);
				l_ctx = q->latest_ctx;
			}
		}

		dst += snprintf(dst, dstlen, "%s", !braces&&quote?dq?"\"":"'":"");

		if (is_variable(c)) {
			dst += print_variable(q, dst, dstlen, c, c_ctx, running);
			q->last_thing_was_symbol = false;
			return dst - save_dst;
		}

		unsigned len_str = src_len;

		if (braces)
			;
		else if (quote) {
			if (is_blob(c) && q->max_depth && (len_str >= q->max_depth) && (src_len > 128))
				len_str = q->max_depth;

			dst += formatted(dst, dstlen, src, len_str, dq);

			if (is_blob(c) && q->max_depth && (len_str >= q->max_depth) && (src_len > 128)) {
				dst--;
				dst += snprintf(dst, dstlen, "%s", ",...");
			}

			q->last_thing_was_symbol = false;
		} else {
			int ch = peek_char_utf8(src);
			bool is_symbol = !needs_quoting(q->st.m, src, src_len) && !iswalpha(ch)
				&& strcmp(src, "\\") && strcmp(src, ",") && strcmp(src, ";")
				&& strcmp(src, "[]") && strcmp(src, "{}") && !q->parens;

			//if (dstlen) printf("*** literal '%s',  was=%d, is=%d\n", src, q->last_thing_was_symbol, is_symbol);

			if (q->last_thing_was_symbol && is_symbol && !q->parens && !q->quoted)
				dst += snprintf(dst, dstlen, "%s", " ");

			dst += plain(dst, dstlen, src, len_str);
			q->last_thing_was_symbol = is_symbol;
		}

		dst += snprintf(dst, dstlen, "%s", !braces&&quote?dq?"\"":"'":"");
		q->did_quote = !braces&&quote;

		if (is_structure(c) && !is_string(c)) {
			pl_idx_t arity = c->arity;
			dst += snprintf(dst, dstlen, "%s", braces?"{":"(");
			q->parens = true;

#if 0
			cell *save_c = c;
			pl_idx_t save_ctx = c_ctx;
#endif

			for (c++; arity--; c += c->nbr_cells) {
				CHECK_INTERRUPT();
				cell *tmp = running ? deref(q, c, c_ctx) : c;
				pl_idx_t tmp_ctx = q->latest_ctx;

#if 0
				if ((tmp == save_c) && (tmp_ctx == save_ctx)) {
					dst += print_variable(q, dst, dstlen, c, c_ctx, running);
					q->last_thing_was_symbol = false;
					return dst - save_dst;
				}
#endif

				bool parens = false;

				if (!braces && is_interned(tmp)) {
					unsigned tmp_priority = search_op(q->st.m, C_STR(q, tmp), NULL, tmp->arity==1);

					if ((tmp_priority >= 1000) && tmp->arity)
						q->parens = parens = true;
				}

				if (parens)
					dst += snprintf(dst, dstlen, "%s", "(");

				if (q->max_depth && ((depth+1) >= q->max_depth)) {
					dst += snprintf(dst, dstlen, "...)");
					q->last_thing_was_symbol = false;
					return dst - save_dst;
				}

				q->parens = parens;
				ssize_t res = print_term_to_buf(q, dst, dstlen, tmp, tmp_ctx, running, 0, depth+1);
				q->parens = false;
				if (res < 0) return -1;
				dst += res;

				if (parens)
					dst += snprintf(dst, dstlen, "%s", ")");

				if (arity)
					dst += snprintf(dst, dstlen, "%s", ",");

				q->parens = false;
			}

			dst += snprintf(dst, dstlen, "%s", braces?"}":")");
			q->parens = false;
			q->last_thing_was_symbol = false;
		}

		return dst - save_dst;
	}

	size_t srclen = src_len;

	if (is_postfix(c)) {
		cell *lhs = c + 1;
		lhs = running ? deref(q, lhs, c_ctx) : lhs;
		pl_idx_t lhs_ctx = q->latest_ctx;
		ssize_t res = print_term_to_buf(q, dst, dstlen, lhs, lhs_ctx, running, 0, depth+1);
		if (res < 0) return -1;
		dst += res;

		bool space = (c->val_off == g_minus_s) && (is_number(lhs) || search_op(q->st.m, C_STR(q, lhs), NULL, true));
		if ((c->val_off == g_plus_s) && search_op(q->st.m, C_STR(q, lhs), NULL, true) && lhs->arity) space = true;
		if (isalpha(*src)) space = true;
		if (space) dst += snprintf(dst, dstlen, "%s", " ");

		int quote = q->quoted && has_spaces(src, src_len);
		if (quote) dst += snprintf(dst, dstlen, "%s", quote?" '":"");

		dst += plain(dst, dstlen, src, srclen);
		if (quote) dst += snprintf(dst, dstlen, "%s", quote?"'":"");
		q->last_thing_was_symbol = false;
		return dst - save_dst;
	}

	if (is_prefix(c)) {
		cell *rhs = c + 1;
		rhs = running ? deref(q, rhs, c_ctx) : rhs;
		pl_idx_t rhs_ctx = q->latest_ctx;
		unsigned my_priority = search_op(q->st.m, src, NULL, true);
		unsigned rhs_pri = is_interned(rhs) ? search_op(q->st.m, C_STR(q, rhs), NULL, true) : 0;

		bool space = (c->val_off == g_minus_s) && (is_number(rhs) || search_op(q->st.m, C_STR(q, rhs), NULL, true));
		if ((c->val_off == g_plus_s) && search_op(q->st.m, C_STR(q, rhs), NULL, true) && rhs->arity) space = true;
		if (isalpha(*src)) space = true;
		if (is_op(rhs) || is_negative(rhs)) space = true;

		bool parens = false; //is_op(rhs);
		if (strcmp(src, "+") && (is_infix(rhs) || is_postfix(rhs))) parens = true;
		if (rhs_pri > my_priority) parens = true;
		if (my_priority && (rhs_pri == my_priority) && strcmp(src, "-") && strcmp(src, "+")) parens = true;
		if (!strcmp(src, "-") && (rhs_pri == my_priority) && (rhs->arity > 1)) parens = true;
		//if (strcmp(src, "\\+")) if (is_atomic(rhs)) parens = false; // Hack
		if ((c->val_off == g_minus_s) && is_number(rhs) && !is_negative(rhs)) parens = true;
		if ((c->val_off == g_minus_s) && search_op(q->st.m, C_STR(q, rhs), NULL, true) && !rhs->arity) parens = true;
		if ((c->val_off == g_plus_s) && search_op(q->st.m, C_STR(q, rhs), NULL, true) && !rhs->arity) parens = true;

		bool quote = q->quoted && has_spaces(src, src_len);

		if (is_interned(rhs) && !rhs->arity && !parens) {
			const char *rhs_src = C_STR(q, rhs);
			if (!iswalpha(*rhs_src) && !isdigit(*rhs_src) && strcmp(rhs_src, "[]") && strcmp(rhs_src, "{}"))
				space = 1;
		}

		if (quote) dst += snprintf(dst, dstlen, "%s", quote?"'":"");
		dst += plain(dst, dstlen, src, srclen);
		if (quote) dst += snprintf(dst, dstlen, "%s", quote?"' ":"");
		if (space) dst += snprintf(dst, dstlen, "%s", " ");
		if (parens) dst += snprintf(dst, dstlen, "%s", "(");
		q->last_thing_was_symbol = false;
		q->parens = parens;
		ssize_t res = print_term_to_buf(q, dst, dstlen, rhs, rhs_ctx, running, 0, depth+1);
		q->parens = false;
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

	unsigned lhs_pri_1 = is_interned(lhs) ? search_op(q->st.m, C_STR(q, lhs), NULL, false) : 0;
	unsigned lhs_pri_2 = is_interned(lhs) && !lhs->arity ? search_op(q->st.m, C_STR(q, lhs), NULL, false) : 0;
	unsigned rhs_pri_1 = is_interned(rhs) ? search_op(q->st.m, C_STR(q, rhs), NULL, is_prefix(rhs)) : 0;
	unsigned rhs_pri_2 = is_interned(rhs) && !rhs->arity ? search_op(q->st.m, C_STR(q, rhs), NULL, false) : 0;
	unsigned my_priority = search_op(q->st.m, src, NULL, false);

	// Print LHS...

	bool lhs_parens = lhs_pri_1 >= my_priority;
	if ((lhs_pri_1 == my_priority) && is_yfx(c)) lhs_parens = false;
	if (lhs_pri_2 > 0) lhs_parens = true;
	if (is_structure(lhs) && (lhs_pri_1 <= my_priority) && (lhs->val_off == g_plus_s)) { lhs_parens = false; }
	bool lhs_space = false;

	if (lhs_space) dst += snprintf(dst, dstlen, "%s", " ");
	if (lhs_parens) dst += snprintf(dst, dstlen, "%s", "(");
	q->parens = lhs_parens;
	ssize_t res = print_term_to_buf(q, dst, dstlen, lhs, lhs_ctx, running, 0, depth+1);
	q->parens = false;
	if (res < 0) return -1;
	dst += res;
	if (lhs_parens) dst += snprintf(dst, dstlen, "%s", ")");
	bool space = false;

	if (is_interned(lhs) && !lhs->arity && !lhs_parens) {
		const char *lhs_src = C_STR(q, lhs);
		if (!isalpha(*lhs_src) && !isdigit(*lhs_src) && (*lhs_src != '$')
			&& strcmp(src, ",") && strcmp(src, ";")
			&& strcmp(lhs_src, "[]") && strcmp(lhs_src, "{}"))
			space = true;
	}

	if (space) dst += snprintf(dst, dstlen, "%s", " ");

	int ch = peek_char_utf8(src);
	bool is_symbol = !needs_quoting(q->st.m, src, src_len)
		&& !iswalpha(ch) && strcmp(src, ",") && strcmp(src, ";")
		&& strcmp(src, "[]") && strcmp(src, "{}") && !q->parens;

	//if (dstlen) printf("*** op '%s',  was=%d, is=%d\n", src, q->last_thing_was_symbol, is_symbol);

	if (!*src || (q->last_thing_was_symbol && is_symbol && !lhs_parens && !space && !q->parens))
		dst += snprintf(dst, dstlen, "%s", " ");

	// Print OP...

	q->last_thing_was_symbol = is_symbol;
	space = iswalpha(*src);
	if (space) dst += snprintf(dst, dstlen, "%s", " ");
	int quote = q->quoted && has_spaces(src, src_len);
	if (op_needs_quoting(q->st.m, src, src_len)) quote = 1;
	if (quote) dst += snprintf(dst, dstlen, "%s", quote?" '":"");
	dst += plain(dst, dstlen, src, srclen);
	if (quote) dst += snprintf(dst, dstlen, "%s", quote?"' ":"");
	if (space) dst += snprintf(dst, dstlen, "%s", " ");

	// Print RHS...

	bool rhs_parens = rhs_pri_1 >= my_priority;
	space = is_number(rhs) && is_negative(rhs);

	if (is_prefix(rhs) && strcmp(src, "|"))
		space = true;

	bool rhs_is_symbol = is_interned(rhs) && !rhs->arity
		&& !iswalpha(*C_STR(q, rhs)) && !needs_quoting(q->st.m, C_STR(q, rhs), C_STRLEN(q, rhs))
		&& !rhs_parens;

	if (rhs_is_symbol) { space = true; }

	if ((rhs_pri_1 == my_priority) && is_xfy(c)) rhs_parens = false;
	if (rhs_pri_2 > 0) rhs_parens = true;
	if (is_structure(rhs) && (rhs_pri_1 <= my_priority)
		&& ((rhs->val_off == g_plus_s) || (rhs->val_off == g_minus_s))) { rhs_parens = false; space = true; }

	if (space) dst += snprintf(dst, dstlen, "%s", " ");

	if (rhs_parens) dst += snprintf(dst, dstlen, "%s", "(");
	q->parens = rhs_parens || space;
	res = print_term_to_buf(q, dst, dstlen, rhs, rhs_ctx, running, 0, depth+1);
	q->parens = false;
	if (res < 0) return -1;
	dst += res;
	if (rhs_parens) dst += snprintf(dst, dstlen, "%s", ")");
	if (rhs_is_symbol) { q->last_thing_was_symbol = true; }
	return dst - save_dst;
}

char *print_canonical_to_strbuf(query *q, cell *c, pl_idx_t c_ctx, int running)
{
	pl_int_t skip = 0, max = 1000000000;
	pl_idx_t tmp_ctx = c_ctx;
	cell tmp = {0};

	if (running && is_iso_list(c)) {
		cell *t = skip_max_list(q, c, &tmp_ctx, max, &skip, &tmp);

		if (t && !is_variable(t) && !skip)
			running = 0;
	} else if (running && is_cyclic_term(q, c, c_ctx)) {
		running = 0;
	}

	ssize_t len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, running, false, 1);

	char *buf = malloc(len+10);
	ensure(buf);
	len = print_canonical_to_buf(q, buf, len+1, c, c_ctx, running, false, 0);
	return buf;
}

bool print_canonical_to_stream(query *q, stream *str, cell *c, pl_idx_t c_ctx, int running)
{
	pl_int_t skip = 0, max = 1000000000;
	pl_idx_t tmp_ctx = c_ctx;
	cell tmp = {0};

	if (running && is_iso_list(c)) {
		cell *t = skip_max_list(q, c, &tmp_ctx, max, &skip, &tmp);

		if (t && !is_variable(t) && !skip)
			running = 0;
	} else if (running && is_cyclic_term(q, c, c_ctx)) {
		running = 0;
	}

	ssize_t len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, running, false, 1);

	char *dst = malloc(len*2+1); //cehteh: why *2?
	check_heap_error(dst);
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
			return false;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
	return true;
}

bool print_canonical(query *q, FILE *fp, cell *c, pl_idx_t c_ctx, int running)
{
	pl_int_t skip = 0, max = 1000000000;
	pl_idx_t tmp_ctx = c_ctx;
	cell tmp = {0};

	if (running && is_iso_list(c)) {
		cell *t = skip_max_list(q, c, &tmp_ctx, max, &skip, &tmp);

		if (t && !is_variable(t) && !skip)
			running = 0;
	} else if (running && is_cyclic_term(q, c, c_ctx)) {
		running = 0;
	}

	ssize_t len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, running, false, 0);
	q->did_quote = false;

	char *dst = malloc(len*2+1); //cehteh: why *2?
	check_heap_error(dst);
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
			return false;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
	return true;
}

char *print_term_to_strbuf(query *q, cell *c, pl_idx_t c_ctx, int running)
{
	pl_int_t skip = 0, max = 1000000000;
	pl_idx_t tmp_ctx = c_ctx;
	cell tmp = {0};

	if (running && is_iso_list(c)) {
		cell *t = skip_max_list(q, c, &tmp_ctx, max, &skip, &tmp);

		if (t && !is_variable(t) && !skip)
			running = 0;
	} else if (running && is_cyclic_term(q, c, c_ctx)) {
		running = 0;
	}

	q->last_thing_was_symbol = false;
	ssize_t len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, false, 0);

	if ((len < 0) || q->cycle_error) {
		len = print_term_to_buf(q, NULL, 0, c, c_ctx, running=0, false, 0);
	}

	q->did_quote = false;

	char *buf = malloc(len+10);
	ensure(buf);
	q->last_thing_was_symbol = false;
	len = print_term_to_buf(q, buf, len+1, c, c_ctx, running, false, 0);
	return buf;
}

bool print_term_to_stream(query *q, stream *str, cell *c, pl_idx_t c_ctx, int running)
{
	pl_int_t skip = 0, max = 1000000000;
	pl_idx_t tmp_ctx = c_ctx;
	cell tmp = {0};

	if (running && is_iso_list(c)) {
		cell *t = skip_max_list(q, c, &tmp_ctx, max, &skip, &tmp);

		if (t && !is_variable(t) && !skip)
			running = 0;
	} else if (running && is_cyclic_term(q, c, c_ctx)) {
		running = 0;
	}

	q->last_thing_was_symbol = false;
	ssize_t len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, false, 0);

	if ((len < 0) || q->cycle_error) {
		len = print_term_to_buf(q, NULL, 0, c, c_ctx, running=0, false, 0);
	}

	q->did_quote = false;

	char *dst = malloc(len+10);
	check_heap_error(dst);
	q->last_thing_was_symbol = false;
	len = print_term_to_buf(q, dst, len+1, c, c_ctx, running, false, 0);
	const char *src = dst;

	while (len) {
		size_t nbytes = net_write(src, len, str);

		if (feof(str->fp)) {
			q->error = true;
			free(dst);
			return false;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
	return true;
}

bool print_term(query *q, FILE *fp, cell *c, pl_idx_t c_ctx, int running)
{
	pl_int_t skip = 0, max = 1000000000;
	pl_idx_t tmp_ctx = c_ctx;
	cell tmp = {0};

	if (running && is_iso_list(c)) {
		cell *t = skip_max_list(q, c, &tmp_ctx, max, &skip, &tmp);

		if (t && !is_variable(t) && !skip)
			running = 0;
	} else if (running && is_cyclic_term(q, c, c_ctx)) {
		running = 0;
	}

	q->last_thing_was_symbol = false;
	ssize_t len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, false, 0);

	if ((len < 0) || q->cycle_error) {
		len = print_term_to_buf(q, NULL, 0, c, c_ctx, running=0, false, 0);
	}

	q->did_quote = false;

	char *dst = malloc(len+10);
	check_heap_error(dst);
	q->last_thing_was_symbol = false;
	len = print_term_to_buf(q, dst, len+1, c, c_ctx, running, false, 0);
	const char *src = dst;

	while (len) {
		size_t nbytes = fwrite(src, 1, len, fp);

		if (feof(fp)) {
			q->error = true;
			free(dst);
			return false;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
	return true;
}

void clear_write_options(query *q)
{
	q->max_depth = q->quoted = 0;
	q->nl = q->fullstop = q->varnames = q->ignore_ops = false;
	q->parens = q->numbervars = false;
	q->last_thing_was_symbol = false;
	q->variable_names = NULL;
}
