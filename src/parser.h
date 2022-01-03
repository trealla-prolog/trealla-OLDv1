#pragma once

#define DUMP_ERRS 0

extern parser *create_parser(module *m);
extern void destroy_parser(parser *p);

extern unsigned tokenize(parser *p, bool args, bool consing);
extern void xref_rule(parser *p, clause *t, predicate *parent);
extern void reset(parser *p);
extern void consultall(parser *p, cell *l);
extern void term_to_body(parser *p);
extern cell *check_body_callable(parser *p, cell *c);
extern bool run(parser *p, const char *src, bool dump);
extern void xref_db(parser *p);
extern char *eat_space(parser *p);
extern bool virtual_term(parser *p, const char *src);
extern bool get_token(parser *p, int last_op);

extern void clear_rule(clause *t);
extern void do_reduce(cell *n);
extern void fix_list(cell *c);
extern bool check_if_rule(const cell *c);
extern cell *get_head(cell *c);
extern cell *get_body(cell *c);
extern cell *get_logical_body(cell *c);
