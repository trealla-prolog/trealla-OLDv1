#pragma once

#define DUMP_ERRS 0

parser *create_parser(module *m);
void destroy_parser(parser *p);

unsigned tokenize(parser *p, bool args, bool consing);
void xref_rule(parser *p, clause *t, predicate *parent);
void reset(parser *p);
void consultall(parser *p, cell *l);
void term_to_body(parser *p);
cell *check_body_callable(parser *p, cell *c);
bool run(parser *p, const char *src, bool dump);
void xref_db(parser *p);
char *eat_space(parser *p);
bool virtual_term(parser *p, const char *src);
bool get_token(parser *p, int last_op);
void read_integer(parser *p, mp_int v2, int base, const char *src,  const char **srcptr);

void clear_rule(clause *t);
void do_reduce(cell *n);
void fix_list(cell *c);
bool check_if_rule(const cell *c);
cell *get_head(cell *c);
cell *get_body(cell *c);
cell *get_logical_body(cell *c);
