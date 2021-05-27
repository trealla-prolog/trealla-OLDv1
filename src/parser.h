#pragma once

extern parser *create_parser(module *m);
extern void destroy_parser(parser *p);

extern unsigned parser_tokenize(parser *p, bool args, bool consing);
extern void term_xref(parser *p, term *t, predicate *parent);
extern void parser_reset(parser *p);
extern void consultall(parser *p, cell *l);
extern void term_to_body(parser *p);
extern cell *check_body_callable(parser *p, cell *c);

extern char *format_property(char **bufptr, size_t *lenptr, char *dst, const char *name, unsigned arity, const char *type);
extern void clear_term(term *t);
extern size_t sprint_int(char *dst, size_t size, int_t n, int base);
extern void fix_list(cell *c);
