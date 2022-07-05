#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <float.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>

#ifdef _WIN32
#define USE_MMAP 0
#define mkdir(p1,p2) mkdir(p1)
#else
#ifndef USE_MMAP
#define USE_MMAP 1
#endif
#if USE_MMAP
#include <sys/mman.h>
#endif
#endif

#include "heap.h"
#include "module.h"
#include "network.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"
#include "utf8.h"

#ifdef _WIN32
#include <windows.h>

char *realpath(const char *path, char resolved_path[PATH_MAX])
{
  char *return_path = 0;

  if (path) //Else EINVAL
  {
    if (resolved_path)
    {
      return_path = resolved_path;
    }
    else
    {
      //Non standard extension that glibc uses
      return_path = malloc(PATH_MAX);
    }

    if (return_path) //Else EINVAL
    {
      //This is a Win32 API function similar to what realpath() is supposed to do
      size_t size = GetFullPathNameA(path, PATH_MAX, return_path, 0);

      //GetFullPathNameA() returns a size larger than buffer if buffer is too small
      if (size > PATH_MAX)
      {
        if (return_path != resolved_path) //Malloc'd buffer - Unstandard extension retry
        {
          size_t new_size;

          free(return_path);
          return_path = malloc(size);

          if (return_path)
          {
            new_size = GetFullPathNameA(path, size, return_path, 0); //Try again

            if (new_size > size) //If it's still too large, we have a problem, don't try again
            {
              free(return_path);
              return_path = 0;
              errno = ENAMETOOLONG;
            }
            else
            {
              size = new_size;
            }
          }
          else
          {
            //I wasn't sure what to return here, but the standard does say to return EINVAL
            //if resolved_path is null, and in this case we couldn't malloc large enough buffer
            errno = EINVAL;
          }
        }
        else //resolved_path buffer isn't big enough
        {
          return_path = 0;
          errno = ENAMETOOLONG;
        }
      }

      //GetFullPathNameA() returns 0 if some path resolve problem occured
      if (!size)
      {
        if (return_path != resolved_path) //Malloc'd buffer
        {
          free(return_path);
        }

        return_path = 0;

        //Convert MS errors into standard errors
        switch (GetLastError())
        {
          case ERROR_FILE_NOT_FOUND:
            errno = ENOENT;
            break;

          case ERROR_PATH_NOT_FOUND: case ERROR_INVALID_DRIVE:
            errno = ENOTDIR;
            break;

          case ERROR_ACCESS_DENIED:
            errno = EACCES;
            break;

          default: //Unknown Error
            errno = EIO;
            break;
        }
      }

      //If we get to here with a valid return_path, we're still doing good
      if (return_path)
      {
        struct stat stat_buffer;

        //Make sure path exists, stat() returns 0 on success
        if (stat(return_path, &stat_buffer))
        {
          if (return_path != resolved_path)
          {
            free(return_path);
          }

          return_path = 0;
          //stat() will set the correct errno for us
        }
        //else we succeeded!
      }
    }
    else
    {
      errno = EINVAL;
    }
  }
  else
  {
    errno = EINVAL;
  }

  return return_path;
}
#endif

#ifdef _WIN32
// if typedef doesn't exist (msvc, blah)
typedef intptr_t ssize_t;

ssize_t getline(char **lineptr, size_t *n, FILE *stream) {
    size_t pos;
    int c;

    if (lineptr == NULL || stream == NULL || n == NULL) {
        errno = EINVAL;
        return -1;
    }

    c = getc(stream);
    if (c == EOF) {
        return -1;
    }

    if (*lineptr == NULL) {
        *lineptr = malloc(128);
        if (*lineptr == NULL) {
            return -1;
        }
        *n = 128;
    }

    pos = 0;
    while(c != EOF) {
        if (pos + 1 >= *n) {
            size_t new_size = *n + (*n >> 2);
            if (new_size < 128) {
                new_size = 128;
            }
            char *new_ptr = realloc(*lineptr, new_size);
            if (new_ptr == NULL) {
                return -1;
            }
            *n = new_size;
            *lineptr = new_ptr;
        }

        ((unsigned char *)(*lineptr))[pos ++] = c;
        if (c == '\n') {
            break;
        }
        c = getc(stream);
    }

    (*lineptr)[pos] = '\0';
    return pos;
}
#endif

static int get_named_stream(prolog *pl, const char *name, size_t len)
{
	for (int i = 0; i < MAX_STREAMS; i++) {
		stream *str = &pl->streams[i];

		if (!str->fp)
			continue;

		if (str->name && (strlen(str->name) == len)
			&& !strncmp(str->name, name, len))
			return i;

		if (str->filename && (strlen(str->filename) == len)
			&& !strncmp(str->filename, name, len))
			return i;
	}

	return -1;
}

static bool is_closed_stream(prolog *pl, cell *p1)
{
	if (!(p1->flags&FLAG_INT_STREAM))
		return false;

	if (pl->streams[get_smallint(p1)].fp)
		return false;

	return true;
}

static void add_stream_properties(query *q, int n)
{
	stream *str = &q->pl->streams[n];
	char tmpbuf[1024*8];
	char *dst = tmpbuf;
	*dst = '\0';
	off_t pos = ftello(str->fp);
	bool at_end_of_file = false;

	if (!str->at_end_of_file && (n > 2)) {
		if (str->p) {
			if (str->p->srcptr && *str->p->srcptr) {
				int ch = get_char_utf8((const char**)&str->p->srcptr);
				str->ungetch = ch;
			}
		}

		int ch = str->ungetch ? str->ungetch : net_getc(str);

		if (str->ungetch)
			;
		else if (feof(str->fp) || ferror(str->fp)) {
			clearerr(str->fp);

			if (str->eof_action != eof_action_reset)
				at_end_of_file = true;
		} else
			str->ungetch = ch;
	}

	char tmpbuf2[1024];
	formatted(tmpbuf2, sizeof(tmpbuf2), str->name, strlen(str->name), false);
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, alias('%s')).\n", n, tmpbuf2);
	formatted(tmpbuf2, sizeof(tmpbuf2), str->filename, strlen(str->filename), false);
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, file_name('%s')).\n", n, tmpbuf2);
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, mode(%s)).\n", n, str->mode);
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, type(%s)).\n", n, str->binary ? "binary" : "text");
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, line_count(%d)).\n", n, str->p ? str->p->line_nbr : 1);
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, position(%llu)).\n", n, (unsigned long long)(pos != -1 ? pos : 0));
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, reposition(%s)).\n", n, (n < 3) || str->socket ? "false" : "true");
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, end_of_stream(%s)).\n", n, str->at_end_of_file ? "past" : at_end_of_file ? "at" : "not");
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, eof_action(%s)).\n", n, str->eof_action == eof_action_eof_code ? "eof_code" : str->eof_action == eof_action_error ? "error" : str->eof_action == eof_action_reset ? "reset" : "none");

	if (!str->binary) {
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, bom(%s)).\n", n, str->bom ? "true" : "false");
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, encoding('%s')).\n", n, "UTF-8");
	}

	if (!strcmp(str->mode, "read"))
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, input).\n", n);
	else
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, output).\n", n);

	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, newline(%s)).\n", n, NEWLINE_MODE);

	parser *p = create_parser(q->st.m);
	p->srcptr = tmpbuf;
	p->consulting = true;
	tokenize(p, false, false);
	destroy_parser(p);
}

static void del_stream_properties(query *q, int n)
{
	cell *tmp = alloc_on_heap(q, 3);
	ensure(tmp);
	make_atom(tmp+0, g_sys_stream_property_s);
	make_int(tmp+1, n);
	make_var(tmp+2, g_anon_s, create_vars(q, 1));
	tmp->nbr_cells = 3;
	tmp->arity = 2;
	q->retry = QUERY_OK;

#if 0
	predicate *pr = find_predicate(q->st.m, tmp);

	if (!pr) {
		DISCARD_RESULT throw_error(q, tmp, "existence_error", "procedure");
		return;
	}
#endif

	while (do_retract(q, tmp, q->st.curr_frame, DO_STREAM_RETRACT)) {
		if (q->did_throw)
			return;

		q->retry = QUERY_RETRY;
		retry_choice(q);
	}

	q->retry = QUERY_OK;
}

static bool do_stream_property(query *q)
{
	GET_FIRST_ARG(pstr,any);
	GET_NEXT_ARG(p1,any);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	cell *c = p1 + 1;
	c = deref(q, c, p1_ctx);
	pl_idx_t c_ctx = q->latest_ctx;

	if (!CMP_STR_CSTR(q, p1, "file_name")) {
		cell tmp;
		may_error(make_cstring(&tmp, str->filename));
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STR_CSTR(q, p1, "alias")) {
		cell tmp;
		may_error(make_cstring(&tmp, str->name));
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STR_CSTR(q, p1, "mode")) {
		cell tmp;
		may_error(make_cstring(&tmp, str->mode));
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STR_CSTR(q, p1, "bom") && !str->binary) {
		cell tmp;
		may_error(make_cstring(&tmp, str->bom?"true":"false"));
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STR_CSTR(q, p1, "type")) {
		cell tmp;
		may_error(make_cstring(&tmp, str->binary ? "binary" : "text"));
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STR_CSTR(q, p1, "reposition")) {
		cell tmp;
		may_error(make_cstring(&tmp, str->socket || (n <= 2) ? "false" : "true"));
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STR_CSTR(q, p1, "encoding") && !str->binary) {
		cell tmp;
		may_error(make_cstring(&tmp, "UTF-8"));
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STR_CSTR(q, p1, "newline")) {
		cell tmp;
		may_error(make_cstring(&tmp, NEWLINE_MODE));
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STR_CSTR(q, p1, "input"))
		return !strcmp(str->mode, "read");

	if (!CMP_STR_CSTR(q, p1, "output"))
		return strcmp(str->mode, "read");

	if (!CMP_STR_CSTR(q, p1, "eof_action") && is_stream(pstr)) {
		cell tmp;

		if (str->eof_action == eof_action_eof_code)
			make_atom(&tmp, index_from_pool(q->pl, "eof_code"));
		else if (str->eof_action == eof_action_error)
			make_atom(&tmp, index_from_pool(q->pl, "error"));
		else if (str->eof_action == eof_action_reset)
			make_atom(&tmp, index_from_pool(q->pl, "reset"));
		else
			make_atom(&tmp, index_from_pool(q->pl, "none"));

		return unify(q, c, c_ctx, &tmp, q->st.curr_frame);
	}

	if (!CMP_STR_CSTR(q, p1, "end_of_stream") && is_stream(pstr)) {
		bool at_end_of_file = false;

		if (!str->at_end_of_file && (n > 2)) {
			if (str->p) {
				if (str->p->srcptr && *str->p->srcptr) {
					int ch = get_char_utf8((const char**)&str->p->srcptr);
					str->ungetch = ch;
				}
			}

			int ch = str->ungetch ? str->ungetch : net_getc(str);

			if (str->ungetch)
				;
			else if (feof(str->fp) || ferror(str->fp)) {
				clearerr(str->fp);

				if (str->eof_action != eof_action_reset)
					at_end_of_file = true;
			} else
				str->ungetch = ch;
		}

		cell tmp;

		if (str->at_end_of_file)
			make_atom(&tmp, index_from_pool(q->pl, "past"));
		else if (at_end_of_file)
			make_atom(&tmp, index_from_pool(q->pl, "at"));
		else
			make_atom(&tmp, index_from_pool(q->pl, "not"));

		return unify(q, c, c_ctx, &tmp, q->st.curr_frame);
	}

	if (!CMP_STR_CSTR(q, p1, "position") && !is_variable(pstr)) {
		cell tmp;
		make_int(&tmp, ftello(str->fp));
		return unify(q, c, c_ctx, &tmp, q->st.curr_frame);
	}

	if (!CMP_STR_CSTR(q, p1, "line_count") && !is_variable(pstr)) {
		cell tmp;
		make_int(&tmp, str->p->line_nbr);
		return unify(q, c, c_ctx, &tmp, q->st.curr_frame);
	}

	return false;
}

static void clear_streams_properties(query *q)
{
	cell tmp;
	make_atom(&tmp, g_sys_stream_property_s);
	tmp.nbr_cells = 1;
	tmp.arity = 2;

	predicate *pr = find_predicate(q->st.m, &tmp);

	if (pr) {
		for (db_entry *dbe = pr->head; dbe;) {
			db_entry *save = dbe;
			dbe = dbe->next;
			add_to_dirty_list(q->st.m, save);
		}

		pr->head = pr->tail = NULL;
		pr->cnt = 0;
	}
}

static const char *s_properties =
	"alias,file_name,mode,encoding,type,line_count,"			\
	"position,reposition,end_of_stream,eof_action,"				\
	"input,output,newline";

static USE_RESULT bool fn_iso_stream_property_2(query *q)
{
	GET_FIRST_ARG(pstr,any);
	GET_NEXT_ARG(p1,any);

	if (!is_stream_or_var(pstr)) {
		if (is_closed_stream(q->pl, pstr))
			return throw_error(q, pstr, q->st.curr_frame, "existence_error", "stream");
		else
			return throw_error(q, pstr, q->st.curr_frame, "domain_error", "stream");
	}

	if (p1->arity > 1)
		return throw_error(q, p1, p1_ctx, "domain_error", "stream_property");

	if (!is_variable(p1) && !is_callable(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "stream_property");

	if (!is_variable(pstr) && !is_variable(p1))
		return do_stream_property(q);

	if (!q->retry) {
		clear_streams_properties(q);

		for (int i = 0; i < MAX_STREAMS; i++) {
			if (!q->pl->streams[i].fp)
				continue;

			stream *str = &q->pl->streams[i];

			if (!str->socket)
				add_stream_properties(q, i);
		}
	}

	may_heap_error(init_tmp_heap(q));
	cell *tmp = deep_clone_to_tmp(q, q->st.curr_cell, q->st.curr_frame);
	may_heap_error(tmp);
	tmp->val_off = g_sys_stream_property_s;

	if (match_clause(q, tmp, q->st.curr_frame, DO_CLAUSE) != true) {
		clear_streams_properties(q);

		if (is_callable(p1) && !strstr(s_properties, C_STR(q, p1)))
			return throw_error(q, p1, p1_ctx, "domain_error", "stream_property");

		return false;
	}

	clause *r = &q->st.curr_clause2->cl;
	GET_FIRST_ARG(pstrx,smallint);
	pstrx->flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	stash_me(q, r, false);
	return true;
}

void convert_path(char *filename)
{
	char *src = filename;

	while (*src) {
		if ((*src == '/') || (*src == '\\'))
			*src = PATH_SEP_CHAR;

		src++;
	}
}

#ifndef _WIN32
static USE_RESULT bool fn_popen_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,variable);
	GET_NEXT_ARG(p4,list_or_nil);
	int n = new_stream(q->pl);
	char *src = NULL;

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");

	char *filename;

	if (is_atom(p1))
		filename = src = DUP_STR(q, p1);
	else
		return throw_error(q, p1, p1_ctx, "domain_error", "source_sink");

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	}

	convert_path(filename);

	stream *str = &q->pl->streams[n];
	str->domain = true;
	may_ptr_error(str->filename = strdup(filename));
	may_ptr_error(str->name = strdup(filename));
	may_ptr_error(str->mode = DUP_STR(q, p2));
	str->eof_action = eof_action_eof_code;
	bool binary = false;
	LIST_HANDLER(p4);

	while (is_list(p4)) {
		cell *h = LIST_HEAD(p4);
		cell *c = deref(q, h, p4_ctx);

		if (is_variable(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		if (is_structure(c) && (c->arity == 1)) {
			cell *name = c + 1;
			name = deref(q, name, q->latest_ctx);


			if (get_named_stream(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0)
				return throw_error(q, c, q->latest_ctx, "permission_error", "open,source_sink");

			if (!CMP_STR_CSTR(q, c, "alias")) {
				free(str->name);
				str->name = DUP_STR(q, name);
			} else if (!CMP_STR_CSTR(q, c, "type")) {
				if (is_atom(name) && !CMP_STR_CSTR(q, name, "binary")) {
					str->binary = true;
					binary = true;
				} else if (is_atom(name) && !CMP_STR_CSTR(q, name, "text"))
					binary = false;
			} else if (!CMP_STR_CSTR(q, c, "eof_action")) {
				if (is_atom(name) && !CMP_STR_CSTR(q, name, "error")) {
					str->eof_action = eof_action_error;
				} else if (is_atom(name) && !CMP_STR_CSTR(q, name, "eof_code")) {
					str->eof_action = eof_action_eof_code;
				} else if (is_atom(name) && !CMP_STR_CSTR(q, name, "reset")) {
					str->eof_action = eof_action_reset;
				}
			}
		} else
			return throw_error(q, c, q->latest_ctx, "domain_error", "stream_option");

		p4 = LIST_TAIL(p4);
		p4 = deref(q, p4, p4_ctx);
		p4_ctx = q->latest_ctx;

		if (is_variable(p4))
			return throw_error(q, p4, p4_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	if (!strcmp(str->mode, "read"))
		str->fp = popen(filename, binary?"rb":"r");
	else if (!strcmp(str->mode, "write"))
		str->fp = popen(filename, binary?"wb":"w");
	else
		return throw_error(q, p2, p2_ctx, "domain_error", "io_mode");

	free(src);

	if (!str->fp) {
		if ((errno == EACCES) || (strcmp(str->mode, "read") && (errno == EROFS)))
			return throw_error(q, p1, p1_ctx, "permission_error", "open,source_sink");
		else
			return throw_error(q, p1, p1_ctx, "existence_error", "source_sink");
	}

	cell tmp;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	return true;
}
#endif

static USE_RESULT bool fn_iso_open_4(query *q)
{
	GET_FIRST_ARG(p1,atom_or_structure);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,variable);
	GET_NEXT_ARG(p4,list_or_nil);
	int n = new_stream(q->pl);
	char *src = NULL;

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");

	char *filename;
	stream *oldstr = NULL;

	if (is_structure(p1) && (p1->arity == 1) && !CMP_STR_CSTR(q, p1, "stream")) {
		int oldn = get_stream(q, p1+1);

		if (oldn < 0)
			return throw_error(q, p1, p1_ctx, "type_error", "not_a_stream");

		stream *oldstr = &q->pl->streams[oldn];
		filename = oldstr->filename;
	} else if (is_atom(p1))
		filename = src = DUP_STR(q, p1);
	else
		return throw_error(q, p1, p1_ctx, "domain_error", "source_sink");

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = src = chars_list_to_string(q, p1, p1_ctx, len);
	}

	convert_path(filename);
	stream *str = &q->pl->streams[n];
	may_ptr_error(str->filename = strdup(filename));
	may_ptr_error(str->name = strdup(filename));
	may_ptr_error(str->mode = DUP_STR(q, p2));
	str->eof_action = eof_action_eof_code;
	free(src);

#if USE_MMAP
	cell *mmap_var = NULL;
	pl_idx_t mmap_ctx = 0;
#endif

	bool bom_specified = false, use_bom = false;
	LIST_HANDLER(p4);

	while (is_list(p4)) {
		cell *h = LIST_HEAD(p4);
		cell *c = deref(q, h, p4_ctx);
		pl_idx_t c_ctx = q->latest_ctx;

		if (is_variable(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		cell *name = c + 1;
		name = deref(q, name, c_ctx);

		if (!CMP_STR_CSTR(q, c, "mmap")) {
#if USE_MMAP
			mmap_var = name;
			mmap_var = deref(q, mmap_var, q->latest_ctx);
			mmap_ctx = q->latest_ctx;
#endif
		} else if (!CMP_STR_CSTR(q, c, "encoding")) {
			if (is_variable(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		} else if (!CMP_STR_CSTR(q, c, "alias")) {
			if (is_variable(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (get_named_stream(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0)
				return throw_error(q, c, c_ctx, "permission_error", "open,source_sink");

			free(str->name);
			str->name = DUP_STR(q, name);
		} else if (!CMP_STR_CSTR(q, c, "type")) {
			if (is_variable(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (is_atom(name) && !CMP_STR_CSTR(q, name, "binary")) {
				str->binary = true;
			} else if (is_atom(name) && !CMP_STR_CSTR(q, name, "text"))
				str->binary = false;
			else
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		} else if (!CMP_STR_CSTR(q, c, "bom")) {
			if (is_variable(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			bom_specified = true;

			if (is_atom(name) && !CMP_STR_CSTR(q, name, "true"))
				use_bom = true;
			else if (is_atom(name) && !CMP_STR_CSTR(q, name, "false"))
				use_bom = false;
		} else if (!CMP_STR_CSTR(q, c, "reposition")) {
			if (is_variable(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (is_atom(name) && !CMP_STR_CSTR(q, name, "true"))
				str->repo = true;
			else if (is_atom(name) && !CMP_STR_CSTR(q, name, "false"))
				str->repo = false;
		} else if (!CMP_STR_CSTR(q, c, "eof_action")) {
			if (is_variable(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (is_atom(name) && !CMP_STR_CSTR(q, name, "error")) {
				str->eof_action = eof_action_error;
			} else if (is_atom(name) && !CMP_STR_CSTR(q, name, "eof_code")) {
				str->eof_action = eof_action_eof_code;
			} else if (is_atom(name) && !CMP_STR_CSTR(q, name, "reset")) {
				str->eof_action = eof_action_reset;
			}
		} else {
			return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		}

		p4 = LIST_TAIL(p4);
		p4 = deref(q, p4, p4_ctx);
		p4_ctx = q->latest_ctx;

		if (is_variable(p4))
			return throw_error(q, p4, p4_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	if (oldstr) {
		int fd = fileno(oldstr->fp);

		if (!strcmp(str->mode, "read"))
			str->fp = fdopen(fd, str->binary?"rb":"r");
		else if (!strcmp(str->mode, "write"))
			str->fp = fdopen(fd, str->binary?"wb":"w");
		else if (!strcmp(str->mode, "append"))
			str->fp = fdopen(fd, str->binary?"ab":"a");
		else if (!strcmp(str->mode, "update"))
			str->fp = fdopen(fd, str->binary?"rb+":"r+");
		else
			return throw_error(q, p2, p2_ctx, "domain_error", "io_mode");
	} else {
		if (!strcmp(str->mode, "read"))
			str->fp = fopen(str->filename, str->binary?"rb":"r");
		else if (!strcmp(str->mode, "write"))
			str->fp = fopen(str->filename, str->binary?"wb":"w");
		else if (!strcmp(str->mode, "append"))
			str->fp = fopen(str->filename, str->binary?"ab":"a");
		else if (!strcmp(str->mode, "update"))
			str->fp = fopen(str->filename, str->binary?"rb+":"r+");
		else
			return throw_error(q, p2, p2_ctx, "domain_error", "io_mode");
	}

	if (!str->fp) {
		if ((errno == EACCES) || (strcmp(str->mode, "read") && (errno == EROFS)))
			return throw_error(q, p1, p1_ctx, "permission_error", "open,source_sink");
		//else if ((strcmp(str->mode, "read") && (errno == EISDIR)))
		//	return throw_error(q, p1, p1_ctx, "permission_error", "open,isadir");
		else
			return throw_error(q, p1, p1_ctx, "existence_error", "source_sink");
	}

	size_t offset = 0;

	if (!strcmp(str->mode, "read") && !str->binary && (!bom_specified || use_bom)) {
		int ch = xgetc_utf8(net_getc, str);

		if (feof(str->fp))
			clearerr(str->fp);

		if ((unsigned)ch == 0xFEFF) {
			str->bom = true;
			offset = 3;
		} else
			fseek(str->fp, 0, SEEK_SET);
	} else if (!strcmp(str->mode, "write") && !str->binary && use_bom) {
		int ch = 0xFEFF;
		char tmpbuf[10];
		put_char_utf8(tmpbuf, ch);
		net_write(tmpbuf, strlen(tmpbuf), str);
		str->bom = true;
	}

#if USE_MMAP
	int prot = 0;

	if (!strcmp(str->mode, "read"))
		prot = PROT_READ;
	else
		prot = PROT_WRITE;

	if (mmap_var && is_variable(mmap_var)) {
		int fd = fileno(str->fp);
		struct stat st = {0};
		fstat(fd, &st);
		size_t len = st.st_size;
		void *addr = mmap(0, len, prot, MAP_PRIVATE, fd, offset);
		cell tmp = {0};
		tmp.tag = TAG_CSTR;
		tmp.flags = FLAG_CSTR_BLOB | FLAG_CSTR_STRING | FLAG_STATIC;
		tmp.nbr_cells = 1;
		tmp.arity = 2;
		tmp.val_str = addr;
		tmp.str_len = len;
		set_var(q, mmap_var, mmap_ctx, &tmp, q->st.curr_frame);
	}
#endif

	cell tmp ;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	return true;
}

static USE_RESULT bool fn_iso_close_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if ((str->fp == stdin)
		|| (str->fp == stdout)
		|| (str->fp == stderr))
		return true;

	if (q->pl->current_input == n)
		q->pl->current_input = 0;

	if (q->pl->current_output == n)
		q->pl->current_output = 1;

	if (q->pl->current_error == n)
		q->pl->current_error = 2;

	if (str->p)
		destroy_parser(str->p);

	if (!str->socket)
		del_stream_properties(q, n);

	net_close(str);
	free(str->mode);
	free(str->filename);
	free(str->name);
	free(str->data);
	return true;
}

static USE_RESULT bool fn_iso_close_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,list_or_nil);
	LIST_HANDLER(p1);

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);
		h = deref(q, h, p1_ctx);

		if (!is_structure(h)
			|| CMP_STR_CSTR(q, h, "force")
			|| CMP_STR_CSTR(q, h+1, "true"))
			return throw_error(q, h, q->latest_ctx, "domain_error", "close_option");

		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	if (is_variable(p1))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "close_option");

	if (!is_nil(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "list");

	return fn_iso_close_1(q);
}

static USE_RESULT bool fn_iso_at_end_of_stream_0(query *q)
{
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (!str->socket) {
		int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);
		str->ungetch = ch;
	}

	if (!feof(str->fp) && !ferror(str->fp))
		return false;

	if (str->eof_action == eof_action_reset)
		clearerr(str->fp);

	return true;
}

static USE_RESULT bool fn_iso_at_end_of_stream_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (strcmp(str->mode, "read") && strcmp(str->mode, "update"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (!str->socket) {
		int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);
		str->ungetch = ch;
	}

	if (!feof(str->fp) && !ferror(str->fp))
		return false;

	if (str->eof_action == eof_action_reset)
		clearerr(str->fp);

	return true;
}

static USE_RESULT bool fn_iso_flush_output_0(query *q)
{
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	fflush(str->fp);
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_flush_output_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	fflush(str->fp);
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_nl_0(query *q)
{
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	fputc('\n', str->fp);
	//fflush(str->fp);
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_nl_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	fputc('\n', str->fp);
	//fflush(str->fp);
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_read_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	cell tmp;
	make_atom(&tmp, g_nil_s);
	return do_read_term(q, str, p1, p1_ctx, &tmp, q->st.curr_frame, NULL);
}

static USE_RESULT bool fn_iso_read_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	cell tmp;
	make_atom(&tmp, g_nil_s);
	return do_read_term(q, str, p1, p1_ctx, &tmp, q->st.curr_frame, NULL);
}

static bool parse_read_params(query *q, stream *str, cell *c, pl_idx_t c_ctx, cell **vars, pl_idx_t *vars_ctx, cell **varnames, pl_idx_t *varnames_ctx, cell **sings, pl_idx_t *sings_ctx)
{
	parser *p = str->p;

	if (!is_structure(c)) {
		DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "read_option");
		return false;
	}

	cell *c1 = deref(q, c+1, c_ctx);
	pl_idx_t c1_ctx = q->latest_ctx;

	if (!CMP_STR_CSTR(q, c, "character_escapes")) {
		if (is_interned(c1))
			p->flags.character_escapes = !CMP_STR_CSTR(q, c1, "true");
	} else if (!CMP_STR_CSTR(q, c, "double_quotes")) {
		if (is_interned(c1)) {
			if (!CMP_STR_CSTR(q, c1, "atom")) {
				p->flags.double_quote_codes = p->flags.double_quote_chars = false;
				p->flags.double_quote_atom = true;
			} else if (!CMP_STR_CSTR(q, c1, "chars")) {
				p->flags.double_quote_atom = p->flags.double_quote_codes = false;
				p->flags.double_quote_chars = true;
			} else if (!CMP_STR_CSTR(q, c1, "codes")) {
				p->flags.double_quote_atom = p->flags.double_quote_chars = false;
				p->flags.double_quote_codes = true;
			}
		}
	} else if (!CMP_STR_CSTR(q, c, "variables")) {
		if (is_variable(c1)) {
			if (vars) *vars = c1;
			if (vars_ctx) *vars_ctx = c1_ctx;
		} else {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "read_option");
			return false;
		}
	} else if (!CMP_STR_CSTR(q, c, "variable_names")) {
		if (is_variable(c1)) {
			if (varnames) *varnames = c1;
			if (varnames_ctx) *varnames_ctx = c1_ctx;
		} else {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "read_option");
			return false;
		}
	} else if (!CMP_STR_CSTR(q, c, "singletons")) {
		if (is_variable(c1)) {
			if (sings) *sings = c1;
			if (sings_ctx) *sings_ctx = c1_ctx;
		} else {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "read_option");
			return false;
		}
	} else if (!CMP_STR_CSTR(q, c, "positions") && (c->arity == 2) && str->fp) {
		p->pos_start = ftello(str->fp);
	} else if (!CMP_STR_CSTR(q, c, "line_counts") && (c->arity == 2)) {
	} else {
		DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "read_option");
		return false;
	}

	return true;
}

bool do_read_term(query *q, stream *str, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx, char *src)
{
	if (!str->p) {
		str->p = create_parser(q->st.m);
		str->p->flags = q->st.m->flags;
		str->p->fp = str->fp;
		str->p->no_fp = q->p->no_fp;
	} else
		reset(str->p);

	parser *p = str->p;
	p->one_shot = true;
	cell *vars = NULL, *varnames = NULL, *sings = NULL;
	pl_idx_t vars_ctx = 0, varnames_ctx = 0, sings_ctx = 0;
	cell *p21 = p2;
	pl_idx_t p21_ctx = p2_ctx;

	LIST_HANDLER(p21);

	while (is_list(p21)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p21);
		h = deref(q, h, p21_ctx);
		pl_idx_t h_ctx = q->latest_ctx;

		if (is_variable(h))
			return throw_error(q, p2, p2_ctx, "instantiation_error", "read_option");

		if (!parse_read_params(q, str, h, h_ctx, &vars, &vars_ctx, &varnames, &varnames_ctx, &sings, &sings_ctx))
			return true;

		p21 = LIST_TAIL(p21);
		p21 = deref(q, p21, p21_ctx);
		p21_ctx = q->latest_ctx;
	}

	if (is_variable(p21))
		return throw_error(q, p2, p2_ctx, "instantiation_error", "read_option");

	if (!is_nil(p21))
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (!src && !p->srcptr && str->fp) {
		if (p->no_fp || getline(&p->save_line, &p->n_line, str->fp) == -1) {
			if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
				clearerr(str->fp);
				return do_yield_0(q, 1);
			}

			p->srcptr = "";
		} else
			p->srcptr = p->save_line;
	}

	if (p->srcptr) {
		char *src = (char*)eat_space(p);
		p->line_nbr_start = p->line_nbr;
		p->srcptr = src;
	}

	for (;;) {
#if 0
		if (isatty(fileno(str->fp)) && !src) {
			fprintf(str->fp, "%s", PROMPT);
			fflush(str->fp);
		}
#endif

		if (!src && (!p->srcptr || !*p->srcptr || (*p->srcptr == '\n'))) {
			if (p->srcptr && (*p->srcptr == '\n'))
				p->line_nbr++;

			if (p->no_fp || getline(&p->save_line, &p->n_line, str->fp) == -1) {
				if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
					clearerr(str->fp);
					return do_yield_0(q, 1);
				}

				p->srcptr = "";
				str->at_end_of_file = str->eof_action != eof_action_reset;

				if (str->eof_action == eof_action_reset)
					clearerr(str->fp);

				if (vars) {
					cell tmp;
					make_atom(&tmp, g_nil_s);
					set_var(q, vars, vars_ctx, &tmp, q->st.curr_frame);
				}

				if (varnames) {
					cell tmp;
					make_atom(&tmp, g_nil_s);
					set_var(q, varnames, varnames_ctx, &tmp, q->st.curr_frame);
				}

				if (sings) {
					cell tmp;
					make_atom(&tmp, g_nil_s);
					set_var(q, sings, sings_ctx, &tmp, q->st.curr_frame);
				}

				cell *p22 = p2;
				pl_idx_t p22_ctx = p2_ctx;
				LIST_HANDLER(p22);

				while (is_list(p22)) {
					CHECK_INTERRUPT();
					cell *h = LIST_HEAD(p22);
					h = deref(q, h, p22_ctx);
					pl_idx_t h_ctx = q->latest_ctx;

					if (is_variable(h))
						return throw_error(q, p2, p2_ctx, "instantiation_error", "read_option");

					if (!CMP_STR_CSTR(q, h, "positions") && (h->arity == 2)) {
						cell *p = h+1;
						p = deref(q, p, h_ctx);
						pl_idx_t p_ctx = q->latest_ctx;
						cell tmp;
						make_int(&tmp, str->p->pos_start);
						unify(q, p, p_ctx, &tmp, q->st.curr_frame);
						p = h+2;
						p = deref(q, p, h_ctx);
						p_ctx = q->latest_ctx;
						make_int(&tmp, ftello(str->fp));
						unify(q, p, p_ctx, &tmp, q->st.curr_frame);
					} else if (!CMP_STR_CSTR(q, h, "line_counts") && (h->arity == 2)) {
						cell *p = h+1;
						p = deref(q, p, h_ctx);
						pl_idx_t p_ctx = q->latest_ctx;
						cell tmp;
						make_int(&tmp, str->p->line_nbr_start);
						unify(q, p, p_ctx, &tmp, q->st.curr_frame);
						p = h+2;
						p = deref(q, p, h_ctx);
						p_ctx = q->latest_ctx;
						make_int(&tmp, str->p->line_nbr);
						unify(q, p, p_ctx, &tmp, q->st.curr_frame);
					}

					p22 = LIST_TAIL(p22);
					p22 = deref(q, p22, p22_ctx);
					p22_ctx = q->latest_ctx;
				}

				cell tmp;
				make_atom(&tmp, g_eof_s);
				return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
			}

			//if (!*p->save_line || (*p->save_line == '\r') || (*p->save_line == '\n'))
			//	continue;

			p->srcptr = p->save_line;
		} else if (src)
			p->srcptr = src;

		break;
	}

	if (p->did_getline)
		q->is_input = true;

	frame *f = GET_CURR_FRAME();
	p->read_term = f->nbr_vars;
	p->do_read_term = true;
	tokenize(p, false, false);
	p->read_term = 0;

	if (p->error || !p->end_of_term) {
		p->error = false;

		if (!p->fp || !isatty(fileno(p->fp))) {
			void *save_fp = p->fp;
			p->fp = NULL;

			while (get_token(p, false, false)
				&& p->token[0] && strcmp(p->token, ".")) {
				CHECK_INTERRUPT();
			}

			p->fp = save_fp;
			p->did_getline = false;
		}

		cell tmp;
		make_atom(&tmp, g_nil_s);
		p->do_read_term = false;
		return throw_error(q, &tmp, q->st.curr_frame, "syntax_error", p->error_desc?p->error_desc:"read_term");
	}

	p->do_read_term = false;

	cell *p22 = p2;
	pl_idx_t p22_ctx = p2_ctx;
	LIST_HANDLER(p22);

	while (is_list(p22)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p22);
		h = deref(q, h, p22_ctx);
		pl_idx_t h_ctx = q->latest_ctx;

		if (is_variable(h))
			return throw_error(q, p2, p2_ctx, "instantiation_error", "read_option");

		if (!CMP_STR_CSTR(q, h, "positions") && (h->arity == 2)) {
			cell *p = h+1;
			p = deref(q, p, h_ctx);
			pl_idx_t p_ctx = q->latest_ctx;
			cell tmp;
			make_int(&tmp, str->p->pos_start);
			unify(q, p, p_ctx, &tmp, q->st.curr_frame);
			p = h+2;
			p = deref(q, p, h_ctx);
			p_ctx = q->latest_ctx;
			make_int(&tmp, ftello(str->fp));
			unify(q, p, p_ctx, &tmp, q->st.curr_frame);
		} else if (!CMP_STR_CSTR(q, h, "line_counts") && (h->arity == 2)) {
			cell *p = h+1;
			p = deref(q, p, h_ctx);
			pl_idx_t p_ctx = q->latest_ctx;
			cell tmp;
			make_int(&tmp, str->p->line_nbr_start);
			unify(q, p, p_ctx, &tmp, q->st.curr_frame);
			p = h+2;
			p = deref(q, p, h_ctx);
			p_ctx = q->latest_ctx;
			make_int(&tmp, str->p->line_nbr);
			unify(q, p, p_ctx, &tmp, q->st.curr_frame);
		}

		p22 = LIST_TAIL(p22);
		p22 = deref(q, p22, p22_ctx);
		p22_ctx = q->latest_ctx;
	}

	if (!p->cl->cidx) {
		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	xref_rule(p->m, p->cl, NULL);

	if (p->nbr_vars) {
		if (!create_vars(q, p->nbr_vars))
			return throw_error(q, p1, p1_ctx, "resource_error", "stack");
	}

	q->tab_idx = 0;

	if (p->nbr_vars)
		collect_vars(q, p->cl->cells, q->st.curr_frame);

	if (vars) {
		unsigned cnt = q->tab_idx;
		may_ptr_error(init_tmp_heap(q));
		cell *tmp = alloc_on_tmp(q, (cnt*2)+1);
		may_ptr_error(tmp);
		unsigned idx = 0;

		if (cnt) {
			unsigned done = 0;

			for (unsigned i = 0; i < q->tab_idx; i++) {
				make_atom(tmp+idx, g_dot_s);
				tmp[idx].arity = 2;
				tmp[idx++].nbr_cells = ((cnt-done)*2)+1;
				cell v;
				make_var(&v, q->pl->tabs[i].val_off, q->pl->tabs[i].var_nbr);
				tmp[idx++] = v;
				done++;
			}

			make_atom(tmp+idx++, g_nil_s);
			tmp[0].arity = 2;
			tmp[0].nbr_cells = idx;

			cell *save = tmp;
			tmp = alloc_on_heap(q, idx);
			may_heap_error(tmp);
			safe_copy_cells(tmp, save, idx);
			tmp->nbr_cells = idx;
			set_var(q, vars, vars_ctx, tmp, q->st.curr_frame);
		} else {
			cell tmp;
			make_atom(&tmp, g_nil_s);
			set_var(q, vars, vars_ctx, &tmp, q->st.curr_frame);
		}
	}

	if (varnames) {
		unsigned cnt = 0;
		may_ptr_error(init_tmp_heap(q));
		cell *tmp = alloc_on_tmp(q, (cnt*4)+1);
		may_ptr_error(tmp);
		unsigned idx = 0;

		for (unsigned i = 0; i < q->tab_idx; i++) {
			if (q->pl->tabs[i].is_anon)
				continue;

			cnt++;
		}

		if (cnt) {
			unsigned done = 0;

			for (unsigned i = 0; i < q->tab_idx; i++) {
				if (q->pl->tabs[i].is_anon)
					continue;

				make_atom(tmp+idx, g_dot_s);
				tmp[idx].arity = 2;
				tmp[idx++].nbr_cells = ((cnt-done)*4)+1;
				cell v;
				make_atom(&v, g_unify_s);
				v.flags |= FLAG_BUILTIN;
				v.fn_ptr = get_fn_ptr(fn_iso_unify_2);
				v.arity = 2;
				v.nbr_cells = 3;
				SET_OP(&v,OP_XFX);
				tmp[idx++] = v;
				make_atom(&v, q->pl->tabs[i].val_off);
				tmp[idx++] = v;
				make_var(&v, q->pl->tabs[i].val_off, q->pl->tabs[i].var_nbr);
				tmp[idx++] = v;
				done++;
			}

			make_atom(tmp+idx++, g_nil_s);
			tmp[0].arity = 2;
			tmp[0].nbr_cells = idx;

			cell *save = tmp;
			tmp = alloc_on_heap(q, idx);
			may_heap_error(tmp);
			safe_copy_cells(tmp, save, idx);
			tmp->nbr_cells = idx;
			set_var(q, varnames, varnames_ctx, tmp, q->st.curr_frame);
		} else {
			cell tmp;
			make_atom(&tmp, g_nil_s);
			set_var(q, varnames, varnames_ctx, &tmp, q->st.curr_frame);
		}
	}

	if (sings) {
		unsigned cnt = 0;
		may_ptr_error(init_tmp_heap(q));
		cell *tmp = alloc_on_tmp(q, (cnt*4)+1);
		may_ptr_error(tmp);
		unsigned idx = 0;

		for (unsigned i = 0; i < q->tab_idx; i++) {
			if (q->pl->tabs[i].cnt != 1)
				continue;

			if (varnames && (q->pl->tabs[i].is_anon))
				continue;

			cnt++;
		}

		if (cnt) {
			unsigned done = 0;

			for (unsigned i = 0; i < q->tab_idx; i++) {
				if (q->pl->tabs[i].cnt != 1)
					continue;

				if (varnames && (q->pl->tabs[i].is_anon))
					continue;

				make_atom(tmp+idx, g_dot_s);
				tmp[idx].arity = 2;
				tmp[idx++].nbr_cells = ((cnt-done)*4)+1;
				cell v;
				make_atom(&v, g_unify_s);
				v.flags |= FLAG_BUILTIN;
				v.fn_ptr = get_fn_ptr(fn_iso_unify_2);
				v.arity = 2;
				v.nbr_cells = 3;
				SET_OP(&v,OP_XFX);
				tmp[idx++] = v;
				make_atom(&v, q->pl->tabs[i].val_off);
				tmp[idx++] = v;
				make_var(&v, q->pl->tabs[i].val_off, q->pl->tabs[i].var_nbr);
				tmp[idx++] = v;
				done++;
			}

			make_atom(tmp+idx++, g_nil_s);
			tmp[0].arity = 2;
			tmp[0].nbr_cells = idx;

			cell *save = tmp;
			tmp = alloc_on_heap(q, idx);
			may_heap_error(tmp);
			safe_copy_cells(tmp, save, idx);
			tmp->nbr_cells = idx;
			set_var(q, sings, sings_ctx, tmp, q->st.curr_frame);
		} else {
			cell tmp;
			make_atom(&tmp, g_nil_s);
			set_var(q, sings, sings_ctx, &tmp, q->st.curr_frame);
		}
	}

	cell *tmp = alloc_on_heap(q, p->cl->cidx-1);
	may_heap_error(tmp);
	safe_copy_cells(tmp, p->cl->cells, p->cl->cidx-1);
	bool ok = unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
	clear_rule(p->cl);
	return ok;
}

static USE_RESULT bool fn_iso_read_term_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	return do_read_term(q, str, p1, p1_ctx, p2, p2_ctx, NULL);
}

static USE_RESULT bool fn_iso_read_term_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	return do_read_term(q, str, p1, p1_ctx, p2, p2_ctx, NULL);
}

static USE_RESULT bool fn_iso_write_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->numbervars = true;
	print_term_to_stream(q, str, p1, p1_ctx, 1);
	q->numbervars = false;
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_write_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->numbervars = true;
	print_term_to_stream(q, str, p1, p1_ctx, 1);
	q->numbervars = false;
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_writeq_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->quoted = 1;
	q->numbervars = true;
	print_term_to_stream(q, str, p1, p1_ctx, 1);
	q->numbervars = false;
	q->quoted = 0;
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_writeq_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->quoted = 1;
	q->numbervars = true;
	print_term_to_stream(q, str, p1, p1_ctx, 1);
	q->numbervars = false;
	q->quoted = 0;
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_write_canonical_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	print_canonical(q, str->fp, p1, p1_ctx, 1);
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_write_canonical_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	print_canonical(q, str->fp, p1, p1_ctx, 1);
	return !ferror(str->fp);
}

bool parse_write_params(query *q, cell *c, pl_idx_t c_ctx, cell **vnames, pl_idx_t *vnames_ctx)
{
	if (is_variable(c)) {
		DISCARD_RESULT throw_error(q, c, c_ctx, "instantiation_error", "write_option");
		return false;
	}

	if (!is_interned(c) || !is_structure(c)) {
		DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
		return false;
	}

	cell *c1 = deref(q, c+1, c_ctx);
	pl_idx_t c1_ctx = q->latest_ctx;

	if (!CMP_STR_CSTR(q, c, "max_depth")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (is_integer(c1) && (get_int(&c[1]) >= 1))
			q->max_depth = get_int(&c[1]);
	} else if (!CMP_STR_CSTR(q, c, "fullstop")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_interned(c1) || (CMP_STR_CSTR(q, c1, "true") && CMP_STR_CSTR(q, c1, "false"))) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->fullstop = !CMP_STR_CSTR(q, c1, "true");
	} else if (!CMP_STR_CSTR(q, c, "nl")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_interned(c1) || (CMP_STR_CSTR(q, c1, "true") && CMP_STR_CSTR(q, c1, "false"))) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->nl = !CMP_STR_CSTR(q, c1, "true");
	} else if (!CMP_STR_CSTR(q, c, "quoted")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_interned(c1) || (CMP_STR_CSTR(q, c1, "true") && CMP_STR_CSTR(q, c1, "false"))) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->quoted = !CMP_STR_CSTR(q, c1, "true");
	} else if (!CMP_STR_CSTR(q, c, "varnames")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_interned(c1) || (CMP_STR_CSTR(q, c1, "true") && CMP_STR_CSTR(q, c1, "false"))) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->varnames = !CMP_STR_CSTR(q, c1, "true");
	} else if (!CMP_STR_CSTR(q, c, "ignore_ops")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_interned(c1) || (CMP_STR_CSTR(q, c1, "true") && CMP_STR_CSTR(q, c1, "false"))) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->ignore_ops = !CMP_STR_CSTR(q, c1, "true");
	} else if (!CMP_STR_CSTR(q, c, "numbervars")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_interned(c1) || (CMP_STR_CSTR(q, c1, "true") && CMP_STR_CSTR(q, c1, "false"))) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->numbervars = !CMP_STR_CSTR(q, c1, "true");
	} else if (!CMP_STR_CSTR(q, c, "variable_names")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_list_or_nil(c1)) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		cell *c1_orig = c1;
		pl_idx_t c1_orig_ctx = c1_ctx;
		LIST_HANDLER(c1);

		while (is_list(c1)) {
			cell *h = LIST_HEAD(c1);
			h = deref(q, h, c1_ctx);
			pl_idx_t h_ctx = q->latest_ctx;

			if (is_variable(h)) {
				DISCARD_RESULT throw_error(q, h, h_ctx, "instantiation_error", "write_option");
				return false;
			}

			if (!is_structure(h)) {
				DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
				return false;
			}

			if (CMP_STR_CSTR(q, h, "=")) {
				DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
				return false;
			}

			if (is_interned(h)) {
				cell *h1 = deref(q, h+1, h_ctx);

				if (is_variable(h1)) {
					DISCARD_RESULT throw_error(q, c, c_ctx, "instantiation_error", "write_option");
					return false;
				} else if (!is_atom(h1)) {
					DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
					return false;
				}

#if 0
				cell *h2 = deref(q, h+2, h_ctx);

				if (!is_variable(h2)) {
					DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
					return false;
				}
#endif
			}

			c1 = LIST_TAIL(c1);
			c1 = deref(q, c1, c1_ctx);
			c1_ctx = q->latest_ctx;
		}

		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1_orig, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_nil(c1)) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		if (vnames) *vnames = c1_orig;
		if (vnames_ctx) *vnames_ctx = c1_orig_ctx;
	} else {
		DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
		return false;
	}

	return true;
}

static USE_RESULT bool fn_iso_write_term_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->flags = q->st.m->flags;
	q->numbervars = false;
	cell *p2_orig = p2, *vnames = NULL;
	pl_idx_t p2_orig_ctx = p2_ctx, vnames_ctx = 0;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx_t h_ctx = q->latest_ctx;

		if (!parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx)) {
			clear_write_options(q);
			return true;
		}

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	if (is_variable(p2)) {
		clear_write_options(q);
		return throw_error(q, p2_orig, p2_orig_ctx, "instantiation_error", "write_option");
	}

	if (!is_nil(p2)) {
		clear_write_options(q);
		return throw_error(q, p2_orig, p2_orig_ctx, "type_error", "list");
	}

	q->variable_names = vnames;
	q->variable_names_ctx = vnames_ctx;

	if (q->ignore_ops)
		print_canonical_to_stream(q, str, p1, p1_ctx, 1);
	else
		print_term_to_stream(q, str, p1, p1_ctx, 1);

	if (q->fullstop)
		net_write(".", 1, str);

	if (q->nl) {
		net_write("\n", 1, str);
		//fflush(str->fp);
	}

	clear_write_options(q);
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_write_term_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->flags = q->st.m->flags;
	q->numbervars = false;
	cell *p2_orig = p2, *vnames = NULL;
	pl_idx_t p2_orig_ctx = p2_ctx, vnames_ctx;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx_t h_ctx = q->latest_ctx;

		if (!parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx)) {
			clear_write_options(q);
			return true;
		}

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	if (is_variable(p2)) {
		clear_write_options(q);
		return throw_error(q, p2_orig, p2_orig_ctx, "instantiation_error", "write_option");
	}

	if (!is_nil(p2)) {
		clear_write_options(q);
		return throw_error(q, p2_orig, p2_orig_ctx, "type_error", "list");
	}

	q->latest_ctx = p1_ctx;
	q->variable_names = vnames;
	q->variable_names_ctx = vnames_ctx;

	if (q->ignore_ops)
		print_canonical_to_stream(q, str, p1, p1_ctx, 1);
	else
		print_term_to_stream(q, str, p1, p1_ctx, 1);

	if (q->fullstop)
		net_write(".", 1, str);

	if (q->nl) {
		net_write("\n", 1, str);
		//fflush(str->fp);
	}

	clear_write_options(q);
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_put_char_1(query *q)
{
	GET_FIRST_ARG(p1,character);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	size_t len = len_char_utf8(C_STR(q, p1));

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	if (len != C_STRLEN(q, p1))
		return throw_error(q, p1, p1_ctx, "type_error", "character");

	const char *src = C_STR(q, p1);
	int ch = get_char_utf8(&src);
	char tmpbuf[80];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_put_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,character);
	size_t len = len_char_utf8(C_STR(q, p1));

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	if (len != C_STRLEN(q, p1))
		return throw_error(q, p1, p1_ctx, "type_error", "character");

	const char *src = C_STR(q, p1);
	int ch = get_char_utf8(&src);
	char tmpbuf[80];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_put_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	if (!is_integer(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	if (is_integer(p1) && is_le(p1,-1))
		return throw_error(q, p1, p1_ctx, "representation_error", "character_code");

	int ch = (int)get_int(p1);
	char tmpbuf[80];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_put_code_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,integer);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	if (!is_integer(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	if (is_integer(p1) && is_le(p1,-1))
		return throw_error(q, p1, p1_ctx, "representation_error", "character_code");

	int ch = (int)get_int(p1);
	char tmpbuf[80];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_put_byte_1(query *q)
{
	GET_FIRST_ARG(p1,byte);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,text_stream");
	}

	if (!is_integer(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	if (is_integer(p1) && is_le(p1,-1))
		return throw_error(q, p1, p1_ctx, "representation_error", "character_code");

	int ch = (int)get_int(p1);
	char tmpbuf[80];
	snprintf(tmpbuf, sizeof(tmpbuf), "%c", ch);
	net_write(tmpbuf, 1, str);
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_put_byte_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,byte);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (!str->binary)
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,text_stream");

	if (!is_integer(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	if (is_integer(p1) && is_le(p1,-1))
		return throw_error(q, p1, p1_ctx, "representation_error", "character_code");

	int ch = (int)get_int(p1);
	char tmpbuf[80];
	snprintf(tmpbuf, sizeof(tmpbuf), "%c", ch);
	net_write(tmpbuf, 1, str);
	return !ferror(str->fp);
}

static USE_RESULT bool fn_iso_get_char_1(query *q)
{
	GET_FIRST_ARG(p1,in_character_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if (ch == '\n') {
		str->did_getc = false;

		if (str->p)
			str->p->line_nbr++;
	}

	char tmpbuf[80];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT bool fn_iso_get_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,in_character_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if (ch == '\n') {
		str->did_getc = false;

		if (str->p)
			str->p->line_nbr++;
	}

	char tmpbuf[80];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT bool fn_iso_get_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (is_integer(p1) && (get_int(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if (ch == '\n') {
		str->did_getc = false;

		if (str->p)
			str->p->line_nbr++;
	} else if (ch == EOF)
		str->did_getc = false;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT bool fn_iso_get_code_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,integer_or_var);

	if (is_integer(p1) && (get_int(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if (ch == '\n') {
		str->did_getc = false;

		if (str->p)
			str->p->line_nbr++;
	}

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT bool fn_iso_get_byte_1(query *q)
{
	GET_FIRST_ARG(p1,in_byte_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = *str->p->srcptr++;
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT bool fn_iso_get_byte_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,in_byte_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = *str->p->srcptr;
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT bool fn_iso_peek_char_1(query *q)
{
	GET_FIRST_ARG(p1,in_character_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}


	if (FEOF(str)) {
		str->did_getc = false;
		clearerr(str->fp);
		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	char tmpbuf[80];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT bool fn_iso_peek_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,in_character_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	if (FEOF(str)) {
		str->did_getc = false;
		clearerr(str->fp);
		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	char tmpbuf[80];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT bool fn_iso_peek_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (is_integer(p1) && (get_int(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	if (FEOF(str)) {
		str->did_getc = false;
		clearerr(str->fp);
		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT bool fn_iso_peek_code_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,integer_or_var);

	if (is_integer(p1) && (get_int(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	if (FEOF(str)) {
		str->did_getc = false;
		clearerr(str->fp);
		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT bool fn_iso_peek_byte_1(query *q)
{
	GET_FIRST_ARG(p1,in_byte_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	if (FEOF(str)) {
		clearerr(str->fp);
		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT bool fn_iso_peek_byte_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,in_byte_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	if (FEOF(str)) {
		clearerr(str->fp);
		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

int new_stream(prolog *pl)
{
	for (int i = 0; i < MAX_STREAMS; i++) {
		if (!pl->streams[i].fp && !pl->streams[i].ignore) {
			memset(&pl->streams[i], 0, sizeof(stream));
			return i;
		}
	}

	return -1;
}

int get_stream(query *q, cell *p1)
{
	if (is_atom(p1)) {
		int n = get_named_stream(q->pl, C_STR(q, p1), C_STRLEN(q, p1));

		if (n < 0)
			return -1;

		return n;
	}

	if (!(p1->flags&FLAG_INT_STREAM))
		return -1;

	if (!q->pl->streams[get_int(p1)].fp)
		return -1;

	return get_smallint(p1);
}

static USE_RESULT bool fn_iso_current_input_1(query *q)
{
	GET_FIRST_ARG(pstr,any);

	if (is_variable(pstr)) {
		cell tmp;
		make_int(&tmp, q->pl->current_input);
		tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
		set_var(q, pstr, pstr_ctx, &tmp, q->st.curr_frame);
		return true;
	}

	if (!is_stream(pstr))
		return throw_error(q, pstr, q->st.curr_frame, "domain_error", "stream");

	int n = get_stream(q, pstr);
	return n == q->pl->current_input ? true : false;
}

static USE_RESULT bool fn_iso_current_output_1(query *q)
{
	GET_FIRST_ARG(pstr,any);

	if (is_variable(pstr)) {
		cell tmp;
		make_int(&tmp, q->pl->current_output);
		tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
		set_var(q, pstr, pstr_ctx, &tmp, q->st.curr_frame);
		return true;
	}

	if (!is_stream(pstr))
		return throw_error(q, pstr, q->st.curr_frame, "domain_error", "stream");

	int n = get_stream(q, pstr);
	return n == q->pl->current_output ? true : false;
}

static USE_RESULT bool fn_iso_set_input_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (strcmp(str->mode, "read") && strcmp(str->mode, "update"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	q->pl->current_input = n;
	return true;
}

static USE_RESULT bool fn_iso_set_output_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	q->pl->current_output = n;
	return true;
}

static USE_RESULT bool fn_iso_set_stream_position_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);

	if (!str->repo)
		return throw_error(q, p1, p1_ctx, "permission_error", "reposition,stream");

	if (!is_smallint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "stream_position");

	off_t pos = get_smallint(p1);

	if (fseeko(str->fp, pos, SEEK_SET))
		return throw_error(q, p1, p1_ctx, "domain_error", "position");

	return true;
}

static USE_RESULT bool fn_read_term_from_chars_3(query *q)
{
	GET_FIRST_ARG(p_chars,any);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p_opts,list_or_nil);
	int n = 3;
	stream *str = &q->pl->streams[n];
	char *src = NULL;
	size_t len;
	bool has_var, is_partial;

	if (is_atom(p_chars) && !is_string(p_chars)) {
		if (!strcmp(C_STR(q, p_chars), "[]")) {
			cell tmp;
			make_atom(&tmp, g_eof_s);
			return unify(q, p_term, p_term_ctx, &tmp, q->st.curr_frame);
		} else
			return throw_error(q, p_chars, p_chars_ctx, "type_error", "character");
	} else if (is_string(p_chars)) {
		len = C_STRLEN(q, p_chars);
		src = malloc(len+1+1);		// +1 is to allow adding a '.'
		may_ptr_error(src);
		memcpy(src, C_STR(q, p_chars), len);
		src[len] = '\0';
	} else if (!check_list(q, p_chars, p_chars_ctx, &is_partial, NULL)) {
		return throw_error(q, p_chars, p_chars_ctx, "type_error", "list");
	} else if ((len = scan_is_chars_list2(q, p_chars, p_chars_ctx, false, &has_var, &is_partial)) > 0) {
		if (!len)
			return throw_error(q, p_chars, p_chars_ctx, "type_error", "character");

		src = chars_list_to_string(q, p_chars, p_chars_ctx, len);
	} else {
		if (has_var)
			return throw_error(q, p_chars, p_chars_ctx, "instantiation_error", "variable");

		return throw_error(q, p_chars, p_chars_ctx, "type_error", "character");
	}

	if (!str->p) {
		str->p = create_parser(q->st.m);
		str->p->flags = q->st.m->flags;
		str->p->fp = str->fp;
	} else
		reset(str->p);

	char *save_src = src;
	str->p->srcptr = src;
	src = eat_space(str->p);

	if (!src || !*src) {
		free(save_src);
		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p_term, p_term_ctx, &tmp, q->st.curr_frame);
	}

	const char *end_ptr = src + strlen(src) - 1;

	while (isspace(*end_ptr) && (end_ptr != src))
		end_ptr--;

	if (src[strlen(src)-1] != '.')
		strcat(src, ".");

	q->p->no_fp = true;
	bool ok = do_read_term(q, str, p_term, p_term_ctx, p_opts, p_opts_ctx, src);
	q->p->no_fp = false;
	free(save_src);
	destroy_parser(str->p);
	str->p = NULL;

	if (ok != true)
		return false;

	return ok;
}

static USE_RESULT bool fn_read_term_from_atom_3(query *q)
{
	GET_FIRST_ARG(p_chars,any);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p_opts,list_or_nil);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	char *src;
	size_t len;

	if (is_cstring(p_chars)) {
		len = C_STRLEN(q, p_chars);
		src = malloc(len+1+1);	// final +1 is for look-ahead
		may_ptr_error(src);
		memcpy(src, C_STR(q, p_chars), len);
		src[len] = '\0';
	} else if ((len = scan_is_chars_list(q, p_chars, p_chars_ctx, false)) > 0) {
		if (!len)
			return throw_error(q, p_chars, p_chars_ctx, "type_error", "atom");

		src = chars_list_to_string(q, p_chars, p_chars_ctx, len);
	} else
		return throw_error(q, p_chars, p_chars_ctx, "type_error", "atom");

	const char *end_ptr = src + strlen(src) - 1;

	while (isspace(*end_ptr) && (end_ptr != src))
		end_ptr--;

	if (src[strlen(src)-1] != '.')
		strcat(src, ".");

	q->p->no_fp = true;
	bool ok = do_read_term(q, str, p_term, p_term_ctx, p_opts, p_opts_ctx, src);
	q->p->no_fp = false;
	free(src);
	return ok;
}

static USE_RESULT bool fn_write_term_to_atom_3(query *q)
{
	GET_FIRST_ARG(p_chars,atom_or_var);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p2,list_or_nil);
	cell *vnames = NULL;
	pl_idx_t vnames_ctx = 0;
	q->flags = q->st.m->flags;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx_t h_ctx = q->latest_ctx;
		parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	q->variable_names = vnames;
	q->variable_names_ctx = vnames_ctx;
	char *dst = print_term_to_strbuf(q, p_term, p_term_ctx, 1);
	clear_write_options(q);
	cell tmp;
	may_error(make_cstring(&tmp, dst), free(dst));
	free(dst);
	bool ok = unify(q, p_chars, p_chars_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT bool fn_write_canonical_to_atom_3(query *q)
{
	GET_FIRST_ARG(p_chars,atom_or_var);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p2,list_or_nil);
	cell *vnames = NULL;
	pl_idx_t vnames_ctx = 0;
	q->flags = q->st.m->flags;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx_t h_ctx = q->latest_ctx;
		parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	char *dst = print_canonical_to_strbuf(q, p_term, p_term_ctx, 1);
	clear_write_options(q);
	cell tmp;
	may_error(make_cstring(&tmp, dst), free(dst));
	free(dst);
	bool ok = unify(q, p_chars, p_chars_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT bool fn_write_term_to_chars_3(query *q)
{
	GET_FIRST_ARG(p_chars,atom_or_var);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p2,list_or_nil);
	cell *vnames = NULL;
	pl_idx_t vnames_ctx = 0;
	q->flags = q->st.m->flags;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx_t h_ctx = q->latest_ctx;
		parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	q->variable_names = vnames;
	q->variable_names_ctx = vnames_ctx;
	char *dst = print_term_to_strbuf(q, p_term, p_term_ctx, 1);
	clear_write_options(q);
	cell tmp;
	may_error(make_string(&tmp, dst), free(dst));
	free(dst);
	bool ok = unify(q, p_chars, p_chars_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT bool fn_write_canonical_to_chars_3(query *q)
{
	GET_FIRST_ARG(p_chars,atom_or_var);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p2,list_or_nil);
	cell *vnames = NULL;
	pl_idx_t vnames_ctx = 0;
	q->flags = q->st.m->flags;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx_t h_ctx = q->latest_ctx;
		parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	char *dst = print_canonical_to_strbuf(q, p_term, p_term_ctx, 1);
	clear_write_options(q);
	cell tmp;
	may_error(make_string(&tmp, dst), free(dst));
	free(dst);
	bool ok = unify(q, p_chars, p_chars_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT bool fn_edin_redo_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	for (;;) {
		str->did_getc = true;
		int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);
		str->ungetch = 0;

		if (feof(str->fp)) {
			str->did_getc = false;
			break;
		} else if (ch == '\n')
			str->did_getc = false;

		if (ch == get_int(p1))
			break;
	}

	return true;
}

static USE_RESULT bool fn_edin_redo_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,integer);

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	for (;;) {
		str->did_getc = true;
		int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);
		str->ungetch = 0;

		if (feof(str->fp)) {
			str->did_getc = false;
			break;
		} else if (ch == '\n')
			str->did_getc = false;

		if (ch == get_int(p1))
			break;
	}

	return true;
}

static USE_RESULT bool fn_edin_tab_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = eval(q, p1_tmp);

	if (!is_integer(&p1))
		return throw_error(q, &p1, p1_tmp_ctx, "type_error", "integer");

	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	for (int i = 0; i < get_int(&p1); i++)
		fputc(' ', str->fp);

	return !ferror(str->fp);
}

static USE_RESULT bool fn_edin_tab_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = eval(q, p1_tmp);

	if (!is_integer(&p1))
		return throw_error(q, &p1, p1_tmp_ctx, "type_error", "integer");

	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	for (int i = 0; i < get_int(&p1); i++)
		fputc(' ', str->fp);

	return !ferror(str->fp);
}

static USE_RESULT bool fn_edin_seen_0(query *q)
{
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (n <= 2)
		return true;

	if ((str->fp != stdin)
		&& (str->fp != stdout)
		&& (str->fp != stderr))
		fclose(str->fp);

	free(str->filename);
	free(str->mode);
	free(str->name);
	memset(str, 0, sizeof(stream));
	q->pl->current_input = 0;
	return true;
}

static USE_RESULT bool fn_edin_told_0(query *q)
{
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (n <= 2)
		return true;

	if ((str->fp != stdin)
		&& (str->fp != stdout)
		&& (str->fp != stderr))
		fclose(str->fp);

	free(str->filename);
	free(str->mode);
	free(str->name);
	memset(str, 0, sizeof(stream));
	q->pl->current_output = 0;
	return true;
}

static USE_RESULT bool fn_edin_seeing_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	char *name = q->pl->current_input==0?"user":q->pl->streams[q->pl->current_input].name;
	cell tmp;
	may_error(make_cstring(&tmp, name));
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return true;
}

static USE_RESULT bool fn_edin_telling_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	char *name =q->pl->current_output==1?"user":q->pl->streams[q->pl->current_output].name;
	cell tmp;
	may_error(make_cstring(&tmp, name));
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return true;
}

static USE_RESULT bool fn_read_line_to_string_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,any);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;

	if (isatty(fileno(str->fp))) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	if (net_getline(&line, &len, str) == -1) {
		free(line);

		if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
			clearerr(str->fp);
			return do_yield_0(q, 1);
		}

		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	len = strlen(line);

	if (len && (line[len-1] == '\n')) {
		line[len-1] = '\0';
		len--;
	}

	if (len && (line[len-1] == '\r')) {
		line[len-1] = '\0';
		len--;
	}

	cell tmp;
	may_error(make_string(&tmp, line), free(line));
	free(line);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT bool fn_read_file_to_string_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,variable);
	GET_NEXT_ARG(p3,list_or_nil);
	char *filename;
	char *src = NULL;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else
		filename = src = DUP_STR(q, p1);

	convert_path(filename);

	bool bom_specified = false, use_bom = false, is_binary = false;
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		cell *c = deref(q, h, p3_ctx);

		if (is_variable(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		if (is_structure(c) && (c->arity == 1)) {
			cell *name = c + 1;
			name = deref(q, name, q->latest_ctx);

			if (!CMP_STR_CSTR(q, c, "type")) {
				if (is_atom(name) && !CMP_STR_CSTR(q, name, "binary")) {
					is_binary = true;
				} else if (is_atom(name) && !CMP_STR_CSTR(q, name, "text"))
					is_binary = false;
				else
					return throw_error(q, c, q->latest_ctx, "domain_error", "stream_option");
			} else if (!CMP_STR_CSTR(q, c, "bom")) {
				bom_specified = true;

				if (is_atom(name) && !CMP_STR_CSTR(q, name, "true"))
					use_bom = true;
				else if (is_atom(name) && !CMP_STR_CSTR(q, name, "false"))
					use_bom = false;
			}
		} else
			return throw_error(q, c, q->latest_ctx, "domain_error", "stream_option");

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;

		if (is_variable(p3))
			return throw_error(q, p3, p3_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	FILE *fp = fopen(filename, is_binary?"rb":"r");
	free(src);

	if (!fp)
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_file");

	// Check for a BOM

	size_t offset = 0;

	if (!is_binary && (!bom_specified || use_bom)) {
		int ch = getc_utf8(fp);

		if ((unsigned)ch != 0xFEFF)
			fseek(fp, 0, SEEK_SET);
		else
			offset = 3;
	}

	struct stat st = {0};

	if (fstat(fileno(fp), &st)) {
		return false;
	}

	size_t len = st.st_size - offset;
	char *s = malloc(len+1);
	may_ptr_error(s, fclose(fp));

	if (fread(s, 1, len, fp) != (size_t)len) {
		free(s);
		fclose(fp);
		return throw_error(q, p1, p1_ctx, "domain_error", "cannot_read");
	}

	s[st.st_size] = '\0';
	fclose(fp);
	cell tmp;
	may_error(make_stringn(&tmp, s, len), free(s));
	set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	free(s);
	return true;
}

static bool do_consult(query *q, cell *p1, pl_idx_t p1_ctx)
{
	if (is_atom(p1)) {
		char *src = DUP_STR(q, p1);
		char *filename = relative_to(q->st.m->filename, src);
		convert_path(filename);
		//unload_file(q->st.m, filename);
		free(src);

		if (!load_file(q->st.m, filename, false)) {
			free(filename);
			return throw_error(q, p1, p1_ctx, "existence_error", "source_sink");
		}

		free(filename);
		return true;
	}

	if (!is_structure(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	if (CMP_STR_CSTR(q, p1, ":"))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	cell *mod = deref(q, p1+1, p1_ctx);
	cell *file = deref(q, p1+2, p1_ctx);

	if (!is_atom(mod) || !is_atom(file))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	module *tmp_m = create_module(q->pl, C_STR(q, mod));
	char *filename = C_STR(q, file);
	tmp_m->make_public = 1;
	filename = relative_to(q->st.m->filename, filename);
	convert_path(filename);
	unload_file(q->st.m, filename);

	if (!load_file(tmp_m, filename, false)) {
		destroy_module(tmp_m);
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "source_sink");
	}

	free(filename);
	return true;
}

static bool do_deconsult(query *q, cell *p1, pl_idx_t p1_ctx)
{
	if (is_atom(p1)) {
		char *src = DUP_STR(q, p1);
		char *filename = relative_to(q->st.m->filename, src);
		convert_path(filename);
		unload_file(q->st.m, filename);
		free(filename);
		free(src);
		return true;
	}

	if (!is_structure(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "source_sink");

	if (CMP_STR_CSTR(q, p1, ":"))
		return throw_error(q, p1, p1_ctx, "type_error", "source_sink");

	cell *mod = deref(q, p1+1, p1_ctx);
	cell *file = deref(q, p1+2, p1_ctx);

	if (!is_atom(mod) || !is_atom(file))
		return throw_error(q, p1, p1_ctx, "type_error", "source_sink");

	module *tmp_m = create_module(q->pl, C_STR(q, mod));
	char *filename = C_STR(q, file);
	tmp_m->make_public = 1;
	filename = relative_to(q->st.m->filename, filename);
	convert_path(filename);
	unload_file(q->st.m, filename);
	free(filename);
	return true;
}

static USE_RESULT bool fn_load_files_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);

	if (is_atom(p1)) {
		may_error(do_consult(q, p1, p1_ctx));
		return true;
	}

	LIST_HANDLER(p1);

	while (is_list(p1)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p1);
		cell *c = deref(q, h, p1_ctx);
		pl_idx_t c_ctx = q->latest_ctx;
		may_error(do_consult(q, c, c_ctx));
		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	return true;
}

static USE_RESULT bool fn_unload_files_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_structure);

	if (is_atom(p1)) {
		may_error(do_deconsult(q, p1, p1_ctx));
		return true;
	}

	LIST_HANDLER(p1);

	while (is_list(p1)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p1);
		cell *c = deref(q, h, p1_ctx);
		pl_idx_t c_ctx = q->latest_ctx;
		may_error(do_deconsult(q, c, c_ctx));
		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	return true;
}

static USE_RESULT bool fn_savefile_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,atom);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STR(q, p1);

	convert_path(filename);
	FILE *fp = fopen(filename, "wb");
	may_ptr_error(fp);
	fwrite(C_STR(q, p2), 1, C_STRLEN(q, p2), fp);
	fclose(fp);
	free(filename);
	return true;
}

static USE_RESULT bool fn_loadfile_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,variable);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STR(q, p1);

	convert_path(filename);
	FILE *fp = fopen(filename, "rb");
	free(filename);

	if (!fp)
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_file");

	// Check for a BOM

	int ch = getc_utf8(fp), offset = 0;

	if ((unsigned)ch != 0xFEFF)
		fseek(fp, 0, SEEK_SET);
	else
		offset = 3;

	struct stat st = {0};

	if (fstat(fileno(fp), &st)) {
		return false;
	}

	size_t len = st.st_size - offset;
	char *s = malloc(len+1);
	may_ptr_error(s, fclose(fp));

	if (fread(s, 1, len, fp) != (size_t)len) {
		free(s);
		fclose(fp);
		return throw_error(q, p1, p1_ctx, "domain_error", "cannot_read");
	}

	s[st.st_size] = '\0';
	fclose(fp);
	cell tmp;
	may_error(make_stringn(&tmp, s, len), free(s));
	set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	free(s);
	return true;
}

static USE_RESULT bool fn_getfile_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,variable);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STR(q, p1);

	convert_path(filename);
	FILE *fp = fopen(filename, "r");
	free(filename);

	if (!fp)
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_file");

	// Check for a BOM

	int ch = getc_utf8(fp);

	if ((unsigned)ch != 0xFEFF)
		fseek(fp, 0, SEEK_SET);

	char *line = NULL;
	size_t len = 0;
	int nbr = 1, in_list = 0;

	while (getline(&line, &len, fp) != -1) {
		CHECK_INTERRUPT();
		int len = strlen(line);

		if (len && (line[len-1] == '\n')) {
			line[len-1] = '\0';
			len--;
		}

		if (len && (line[len-1] == '\r')) {
			line[len-1] = '\0';
			len--;
		}

		cell tmp;
		may_error(make_stringn(&tmp, line, len));

		if (nbr++ == 1)
			allocate_list(q, &tmp);
		else
			append_list(q, &tmp);

		in_list = 1;
	}

	free(line);
	fclose(fp);

	if (!in_list) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else {
		cell *l = end_list(q);
		may_ptr_error(l);
		set_var(q, p2, p2_ctx, l, q->st.curr_frame);
	}

	return true;
}

static USE_RESULT bool fn_getlines_1(query *q)
{
	GET_NEXT_ARG(p1,variable);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;
	int nbr = 1, in_list = 0;

	while (getline(&line, &len, str->fp) != -1) {
		CHECK_INTERRUPT();
		int len = strlen(line);

		if (len && (line[len-1] == '\n')) {
			line[len-1] = '\0';
			len--;
		}

		if (len && (line[len-1] == '\r')) {
			line[len-1] = '\0';
			len--;
		}

		cell tmp;
		may_error(make_stringn(&tmp, line, len));

		if (nbr++ == 1)
			allocate_list(q, &tmp);
		else
			append_list(q, &tmp);

		in_list = 1;
	}

	free(line);

	if (!in_list) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	} else {
		cell *l = end_list(q);
		may_ptr_error(l);
		set_var(q, p1, p1_ctx, l, q->st.curr_frame);
	}

	return true;
}

static USE_RESULT bool fn_getlines_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,variable);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;
	int nbr = 1, in_list = 0;

	while (getline(&line, &len, str->fp) != -1) {
		CHECK_INTERRUPT();
		int len = strlen(line);

		if (len && (line[len-1] == '\n')) {
			line[len-1] = '\0';
			len--;
		}

		if (len && (line[len-1] == '\r')) {
			line[len-1] = '\0';
			len--;
		}

		cell tmp;
		may_error(make_stringn(&tmp, line, len));

		if (nbr++ == 1)
			allocate_list(q, &tmp);
		else
			append_list(q, &tmp);

		in_list = 1;
	}

	free(line);

	if (!in_list) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	} else {
		cell *l = end_list(q);
		may_ptr_error(l);
		set_var(q, p1, p1_ctx, l, q->st.curr_frame);
	}

	return true;
}

static char *fixup(const char *srcptr)
{
	char *tmpbuf = strdup(srcptr);
	const char *src = srcptr;
	char *dst = tmpbuf;

	while (*src) {
		if ((src[0] == '.') && (src[1] == '.') && ((src[2] == '/') || (src[2] == '\\'))) {
			dst -= 2;

			while ((dst != tmpbuf) && ((*dst != '/') && (*dst != '\\')
#ifdef _WIN32
				&& (*dst != ':')
#endif
				))
				dst--;

			src += 2;
			dst++;
		} else if ((src[0] == '.') && ((src[1] == '/') || (src[1] == '\\')
#ifdef _WIN32
				|| (src[1] == ':')
#endif
			))
			src += 1;
		else
			*dst++ = *src;

		src++;
	}

	*dst = '\0';
	return tmpbuf;
}

static USE_RESULT bool fn_absolute_file_name_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	GET_NEXT_ARG(p_opts,list_or_nil);
	bool expand = false;
	char *filename = NULL;
	char *here = strdup(q->st.m->filename);
	may_ptr_error(here);
	char *ptr = here + strlen(here) - 1;

	while ((ptr != here) && *ptr && (*ptr != '/') && (*ptr != '\\') && (*ptr != ':'))
		ptr--;

	ptr[1] = '\0';
	char *cwd = here;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STR(q, p1);

	convert_path(filename);
	LIST_HANDLER(p_opts);

	while (is_list(p_opts)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p_opts);
		h = deref(q, h, p_opts_ctx);

		if (is_structure(h) && (h->arity == 1)) {
			if (!CMP_STR_CSTR(q, h, "expand")) {
				if (is_interned(h+1)) {
					if (!CMP_STR_CSTR(q, h+1, "true"))
						expand = true;
				}
			} else if (!CMP_STR_CSTR(q, h, "relative_to")) {
				if (is_atom(h+1))
					cwd = DUP_STR(q, h+1);
			}
		}

		p_opts = LIST_TAIL(p_opts);
		p_opts = deref(q, p_opts, p_opts_ctx);
		p_opts_ctx = q->latest_ctx;
	}

	char *tmpbuf = NULL;
	const char *s = filename;
	//printf("*** from=%s, cwd=%s", filename, cwd);

	if (expand && (*s == '$')) {
		char envbuf[PATH_MAX];
		char *dst = envbuf;
		s++;

		while (*s && (*s != '/') && (*s != '\\') && ((dst-envbuf-1) != sizeof(envbuf)))
			*dst++ = *s++;

		if ((*s == '\\') || (*s == '/'))
			s++;

		*dst = '\0';
		char *ptr = getenv(envbuf);

		if (!ptr) {
			free(filename);
			return throw_error(q, p1, p1_ctx, "existence_error", "environment_variable");
		}

		size_t buflen = strlen(ptr)+1+strlen(s)+1;
		tmpbuf = malloc(buflen);
		may_ptr_error(tmpbuf);
		snprintf(tmpbuf, buflen, "%s/%s", ptr, s);
		convert_path(tmpbuf);
		char *tmpbuf2;

		if ((tmpbuf2 = realpath(tmpbuf, NULL)) == NULL) {
		} else {
			free(tmpbuf);
			tmpbuf = tmpbuf2;
		}
	} else {
		if ((tmpbuf = realpath(s, NULL)) == NULL) {
			if ((tmpbuf = realpath(cwd, NULL)) == NULL)
				tmpbuf = realpath(".", NULL);

			may_ptr_error(tmpbuf);

			if ((*s != '/') && (*s != '\\')
#ifdef _WIN32
				&& (s[1] != ':')
#endif
				) {
				size_t buflen = strlen(tmpbuf)+1+strlen(s)+1;
				char *tmp = malloc(buflen);
				may_ptr_error(tmp, free(tmpbuf));
				snprintf(tmp, buflen, "%s/%s", tmpbuf, s);
				convert_path(tmp);
				free(tmpbuf);
				tmpbuf = fixup(tmp);
				may_ptr_error(tmpbuf);
				free(tmp);
			} else {
				free(tmpbuf);
				tmpbuf = fixup(s);
				may_ptr_error(tmpbuf);
			}
		}
	}

	//printf(", to=%s\n", tmpbuf);
	free(filename);

	if (cwd != here)
		free(cwd);

	free(here);
	cell tmp;

	if (is_string(p1))
		may_error(make_string(&tmp, tmpbuf), free(tmpbuf));
	else
		may_error(make_cstring(&tmp, tmpbuf), free(tmpbuf));

	free(tmpbuf);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT bool fn_getline_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;

	if (isatty(fileno(str->fp))) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	if (net_getline(&line, &len, str) == -1) {
		free(line);
		return false;
	}

	len = strlen(line);

	if (len && (line[len-1] == '\n')) {
		line[len-1] = '\0';
		len--;
	}

	if (len && (line[len-1] == '\r')) {
		line[len-1] = '\0';
		len--;
	}

	cell tmp;
	may_error(make_string(&tmp, line), free(line));
	free(line);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT bool fn_getline_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,any);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;

	if (isatty(fileno(str->fp))) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	if (net_getline(&line, &len, str) == -1) {
		free(line);

		if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
			clearerr(str->fp);
			return do_yield_0(q, 1);
		}

		return false;
	}

	len = strlen(line);

	if (line[strlen(line)-1] == '\n')
		line[strlen(line)-1] = '\0';

	if (line[strlen(line)-1] == '\r')
		line[strlen(line)-1] = '\0';

	cell tmp;
	may_error(make_string(&tmp, line), free(line));
	free(line);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT bool fn_access_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,atom);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STR(q, p1);

	int amode = R_OK;

	if (!CMP_STR_CSTR(q, p2, "read"))
		amode = R_OK;
	else if (!CMP_STR_CSTR(q, p2, "write"))
		amode = W_OK;
	else if (!CMP_STR_CSTR(q, p2, "append"))
		amode = W_OK;
	else if (!CMP_STR_CSTR(q, p2, "execute"))
		amode = X_OK;
	else if (!CMP_STR_CSTR(q, p2, "none")) {
		free(filename);
		return true;
	} else {
		free(filename);
		return throw_error(q, p2, p2_ctx, "domain_error", "mode");
	}

	convert_path(filename);
	struct stat st = {0};
	int status = stat(filename, &st);

	if (status && (!CMP_STR_CSTR(q, p2, "read") || !CMP_STR_CSTR(q, p2, "exist") || !CMP_STR_CSTR(q, p2, "execute") || !CMP_STR_CSTR(q, p2, "none"))) {
		free(filename);
		return false;
	}

	if (status && (!CMP_STR_CSTR(q, p2, "write") || !CMP_STR_CSTR(q, p2, "append"))) {
		free(filename);
		return true;
	}

	int ok = !access(filename, amode);
	free(filename);
	return ok;
}

static USE_RESULT bool fn_exists_file_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STR(q, p1);

	convert_path(filename);
	struct stat st = {0};

	if (stat(filename, &st)) {
		//printf("*** here %s\n", filename);
		free(filename);
		return false;
	}

	free(filename);

	if ((st.st_mode & S_IFMT) != S_IFREG)
		return false;

	return true;
}

static USE_RESULT bool fn_directory_files_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,variable);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STR(q, p1);

	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "directory");
	}

	convert_path(filename);
	DIR *dirp = opendir(filename);

	if (!dirp) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "directory");
	}

	struct dirent *dire = readdir(dirp);
	cell tmp;

	if (is_string(p1))
		may_error(make_string(&tmp, dire->d_name));
	else
		may_error(make_cstring(&tmp, dire->d_name));

	allocate_list(q, &tmp);

	for (dire = readdir(dirp); dire; dire = readdir(dirp)) {
		if (is_string(p1))
			may_error(make_string(&tmp, dire->d_name));
		else
			may_error(make_cstring(&tmp, dire->d_name));

		append_list(q, &tmp);
	}

	closedir(dirp);
	free(filename);
	cell *l = end_list(q);
	bool ok = unify(q, p2, p2_ctx, l, q->st.curr_frame);
	return ok;
}

static USE_RESULT bool fn_delete_file_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STR(q, p1);

	convert_path(filename);
	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	remove(filename);
	free(filename);
	return true;
}

static USE_RESULT bool fn_rename_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,atom_or_list);
	char *filename1, *filename2;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename1 = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename1 = DUP_STR(q, p1);

	if (is_iso_list(p2)) {
		size_t len = scan_is_chars_list(q, p2, p2_ctx, true);

		if (!len) {
			free(filename1);
			return throw_error(q, p2, p2_ctx, "type_error", "atom");
		}

		filename2 = chars_list_to_string(q, p2, p2_ctx, len);
	} else
		filename2 = DUP_STR(q, p2);

	convert_path(filename1);
	convert_path(filename2);
	struct stat st = {0};

	if (stat(filename1, &st)) {
		free(filename1);
		free(filename2);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	bool ok = !rename(filename1, filename2);
	free(filename1);
	free(filename2);
	return ok ? true : false;
}

static USE_RESULT bool fn_copy_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,atom_or_list);
	char *filename1, *filename2;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename1 = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename1 = DUP_STR(q, p1);

	if (is_iso_list(p2)) {
		size_t len = scan_is_chars_list(q, p2, p2_ctx, true);

		if (!len) {
			free(filename1);
			return throw_error(q, p2, p2_ctx, "type_error", "atom");
		}

		filename2 = chars_list_to_string(q, p2, p2_ctx, len);
	} else
		filename2 = DUP_STR(q, p2);

	convert_path(filename1);
	convert_path(filename2);
	FILE *fp1 = fopen(filename1, "rb");

	if (!fp1) {
		free(filename1);
		free(filename2);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	free(filename1);
	FILE *fp2 = fopen(filename2, "wb");

	if (!fp2) {
		fclose(fp1);
		free(filename2);
		return throw_error(q, p2, p2_ctx, "permission_error", "file");
	}

	free(filename2);
	char buffer[1024];
	size_t n;

	while ((n = fread(buffer, 1, sizeof(buffer), fp1)) > 0) {
		if (fwrite(buffer, 1, n, fp2) != n) {
			fclose(fp2);
			fclose(fp1);
			return throw_error(q, p2, p2_ctx, "system_error", "file");
		}
	}

	fclose(fp2);

	if (!feof(fp1)) {
		fclose(fp1);
		return throw_error(q, p1, p1_ctx, "system_error", "file");
	}

	fclose(fp1);
	return true;
}

static USE_RESULT bool fn_time_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,variable);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STR(q, p1);

	convert_path(filename);
	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	free(filename);
	cell tmp;
	make_float(&tmp, st.st_mtime);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT bool fn_size_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,integer_or_var);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STR(q, p1);

	convert_path(filename);
	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	free(filename);
	cell tmp;
	make_int(&tmp, st.st_size);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT bool fn_exists_directory_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STR(q, p1);

	convert_path(filename);
	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return false;
	}

	free(filename);

	if ((st.st_mode & S_IFMT) != S_IFDIR)
		return false;

	return true;
}

static USE_RESULT bool fn_make_directory_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STR(q, p1);

	convert_path(filename);
	struct stat st = {0};

	if (!stat(filename, &st)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	if (mkdir(filename, 0777)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "permission_error", "file");
	}

	free(filename);
	return true;
}

static USE_RESULT bool fn_make_directory_path_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STR(q, p1);

	convert_path(filename);
	struct stat st = {0};

	for (char *ptr = filename+1; *ptr; ptr++) {
		if (*ptr == PATH_SEP_CHAR) {
			*ptr = '\0';

			if (stat(filename, &st)) {
				if (mkdir(filename, 0777)) {
					free(filename);
					return throw_error(q, p1, p1_ctx, "permission_error", "directory");
				}
			}

			*ptr = PATH_SEP_CHAR;
		}
	}

	if (!stat(filename, &st)) {
		free(filename);
		return true;
	}

	if (mkdir(filename, 0777)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "permission_error", "directory");
	}

	free(filename);
	return true;
}

static USE_RESULT bool fn_working_directory_2(query *q)
{
	GET_FIRST_ARG(p_old,variable);
	GET_NEXT_ARG(p_new,atom_or_list_or_var);
	char tmpbuf[PATH_MAX], tmpbuf2[PATH_MAX];
	char *oldpath = getcwd(tmpbuf, sizeof(tmpbuf));
	snprintf(tmpbuf2, sizeof(tmpbuf2), "%s/", oldpath);
	convert_path(tmpbuf2);
	oldpath = tmpbuf2;
	cell tmp;
	may_error(make_string(&tmp, oldpath));

	if (is_atom_or_list(p_new)) {
		char *filename;

		if (is_iso_list(p_new)) {
			size_t len = scan_is_chars_list(q, p_new, p_new_ctx, true);

			if (!len) {
				unshare_cell(&tmp);
				return throw_error(q, p_new, p_new_ctx, "type_error", "atom");
			}

			filename = chars_list_to_string(q, p_new, p_new_ctx, len);
		} else
			filename = DUP_STR(q, p_new);

		if (chdir(filename)) {
			unshare_cell(&tmp);
			return throw_error(q, p_new, p_new_ctx, "existence_error", "path");
		}

		free(filename);
	}

	bool ok = unify(q, p_old, p_old_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT bool fn_chdir_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);
		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STR(q, p1);

	convert_path(filename);
	bool ok = !chdir(filename);
	free(filename);
	return ok;
}

static void parse_host(const char *src, char hostname[1024], char path[4096], unsigned *port, int *ssl, int *domain)
{
	if (!strncmp(src, "https://", 8)) {
		src += 8;
		*ssl = 1;
		*port = 443;
	} else if (!strncmp(src, "http://", 7)) {
		src += 7;
		*ssl = 0;
		*port = 80;
	} else if (!strncmp(src, "unix://", 7)) {
		src += 7;
		*domain = 1;
	}

	if (*src == ':')
		sscanf(src, ":%u/%4095s", port, path);
	else
		sscanf(src, "%1023[^:/]:%u/%4095s", hostname, port, path);

	hostname[1023] = '\0';
	path[4095] = '\0';
}

static USE_RESULT bool fn_server_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	GET_NEXT_ARG(p3,list_or_nil);
	char hostname[1024], path[4096];
	char *keyfile = "privkey.pem", *certfile = "fullchain.pem";
	int udp = 0, nodelay = 1, nonblock = 0, ssl = 0, domain = 0, level = 0;
	unsigned port = 80;
	snprintf(hostname, sizeof(hostname), "localhost");
	path[0] = '\0';
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p3);
		cell *c = deref(q, h, p3_ctx);

		if (is_structure(c) && (c->arity == 1)) {
			if (!CMP_STR_CSTR(q, c, "udp")) {
				c = c + 1;

				if (is_atom(c))
					udp = !CMP_STR_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STR_CSTR(q, c, "nodelay")) {
				c = c + 1;

				if (is_atom(c))
					nodelay = !CMP_STR_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STR_CSTR(q, c, "ssl")) {
				c = c + 1;

				if (is_atom(c))
					ssl = !CMP_STR_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STR_CSTR(q, c, "keyfile")) {
				c = c + 1;

				if (is_atom(c))
					keyfile = C_STR(q, c);
			} else if (!CMP_STR_CSTR(q, c, "certfile")) {
				c = c + 1;

				if (is_atom(c))
					certfile = C_STR(q, c);
			} else if (!CMP_STR_CSTR(q, c, "hostname")) {
				c = c + 1;

				if (is_atom(c))
					slicecpy(hostname, sizeof(hostname), C_STR(q, c), C_STRLEN(q, c));
			} else if (!CMP_STR_CSTR(q, c, "scheme")) {
				c = c + 1;

				if (is_atom(c)) {
					ssl = !CMP_STR_CSTR(q, c, "https") ? 1 : 0;
					port = 443;
				}
			} else if (!CMP_STR_CSTR(q, c, "port")) {
				c = c + 1;

				if (is_integer(c))
					port = get_int(c);
			} else if (!CMP_STR_CSTR(q, c, "level")) {
				c = c + 1;

				if (is_integer(c))
					level = (int)get_int(c);
			}
		}

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	const char *url = C_STR(q, p1);
	parse_host(url, hostname, path, &port, &ssl, &domain);
	nonblock = q->is_task;

	int fd = net_server(hostname, port, udp, ssl?keyfile:NULL, ssl?certfile:NULL);

	if (fd == -1)
		return throw_error(q, p1, p1_ctx, "existence_error", "server_failed");

	int n = new_stream(q->pl);

	if (n < 0) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");
	}

	stream *str = &q->pl->streams[n];
	may_ptr_error(str->filename = DUP_STR(q, p1));
	may_ptr_error(str->name = strdup(hostname));
	may_ptr_error(str->mode = strdup("update"));
	str->nodelay = nodelay;
	str->nonblock = nonblock;
	str->udp = udp;
	str->fp = fdopen(fd, "r+");
	str->ssl = ssl;
	str->level = level;
	str->sslptr = NULL;

	if (str->fp == NULL) {
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_stream");
		close(fd);
	}

	if (!str->ssl)
		net_set_nonblocking(str);

	cell tmp;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	return true;
}

static USE_RESULT bool fn_accept_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,variable);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	int fd = net_accept(str);

	if (fd == -1) {
		if (q->is_task)
			return do_yield_0(q, 1);

		return false;
	}

	n = new_stream(q->pl);

	if (n < 0) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");
	}

	stream *str2 = &q->pl->streams[n];
	may_ptr_error(str2->filename = strdup(str->filename));
	may_ptr_error(str2->name = strdup(str->name));
	may_ptr_error(str2->mode = strdup("update"));
	str->socket = true;
	str2->nodelay = str->nodelay;
	str2->nonblock = str->nonblock;
	str2->udp = str->udp;
	str2->ssl = str->ssl;
	str2->fp = fdopen(fd, "r+");

	if (str2->fp == NULL) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_stream");
	}

	if (str->ssl) {
		str2->sslptr = net_enable_ssl(fd, str->name, 1, str->level, NULL);

		if (!str2->sslptr) {
			close(fd);
			return false;
		}
	}

	if (!str->ssl)
		net_set_nonblocking(str2);

	may_error(push_choice(q));
	cell tmp;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return true;
}

static USE_RESULT bool fn_client_5(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	GET_NEXT_ARG(p3,variable);
	GET_NEXT_ARG(p4,variable);
	GET_NEXT_ARG(p5,list_or_nil);
	char hostname[1024], path[1024*4];
	char *certfile = NULL;
	int udp = 0, nodelay = 1, nonblock = 0, ssl = 0, domain = 0, level = 0;
	hostname[0] = path[0] = '\0';
	unsigned port = 80;
	LIST_HANDLER(p5);

	while (is_list(p5)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p5);
		cell *c = deref(q, h, p5_ctx);

		if (is_structure(c) && (c->arity == 1)) {
			if (!CMP_STR_CSTR(q, c, "udp")) {
				c = c + 1;

				if (is_atom(c))
					udp = !CMP_STR_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STR_CSTR(q, c, "nodelay")) {
				c = c + 1;

				if (is_atom(c))
					nodelay = !CMP_STR_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STR_CSTR(q, c, "ssl")) {
				c = c + 1;

				if (is_atom(c))
					ssl = !CMP_STR_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STR_CSTR(q, c, "certfile")) {
				c = c + 1;

				if (is_atom(c))
					certfile = C_STR(q, c);
			} else if (!CMP_STR_CSTR(q, c, "scheme")) {
				c = c + 1;

				if (is_atom(c)) {
					ssl = !CMP_STR_CSTR(q, c, "https") ? 1 : 0;
					port = 443;
				}
			} else if (!CMP_STR_CSTR(q, c, "port")) {
				c = c + 1;

				if (is_integer(c))
					port = (int)get_int(c);
			} else if (!CMP_STR_CSTR(q, c, "level")) {
				c = c + 1;

				if (is_integer(c))
					level = (int)get_int(c);
			}
		}

		p5 = LIST_TAIL(p5);
		p5 = deref(q, p5, p5_ctx);
		p5_ctx = q->latest_ctx;
	}

	const char *url = C_STR(q, p1);
	parse_host(url, hostname, path, &port, &ssl, &domain);
	nonblock = q->is_task;

	while (is_list(p5)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p5);
		cell *c = deref(q, h, p5_ctx);

		if (is_structure(c) && (c->arity == 1)) {
			if (!CMP_STR_CSTR(q, c, "host")) {
				c = c + 1;

				//if (is_atom(c))
				//	;//udp = !CMP_STR_CSTR(q, c, "true") ? 1 : 0;
			}
		}

		p5 = LIST_TAIL(p5);
		p5 = deref(q, p5, p5_ctx);
		p5_ctx = q->latest_ctx;
	}

	int fd = net_connect(hostname, port, udp, nodelay);

	if (fd == -1)
		return throw_error(q, p1, p1_ctx, "resource_error", "could_not_connect");

	int n = new_stream(q->pl);

	if (n < 0) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");
	}

	stream *str = &q->pl->streams[n];
	may_ptr_error(str->filename = DUP_STR(q, p1));
	may_ptr_error(str->name = strdup(hostname));
	may_ptr_error(str->mode = strdup("update"));
	str->socket = true;
	str->nodelay = nodelay;
	str->nonblock = nonblock;
	str->udp = udp;
	str->ssl = ssl;
	str->level = level;
	str->fp = fdopen(fd, "r+");

	if (!str->filename || !str->name || !str->mode) {
		free(str->filename);
		free(str->name);
		free(str->mode); //cehteh: maybe from pool?
		return false;
	}

	if (str->fp == NULL) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_stream");
	}

	if (str->ssl) {
		str->sslptr = net_enable_ssl(fd, hostname, 0, str->level, certfile);
		may_ptr_error (str->sslptr, close(fd));
	}

	if (nonblock && !str->ssl)
		net_set_nonblocking(str);

	cell tmp;
	may_error(make_string(&tmp, hostname));
	set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	may_error(make_string(&tmp, path));
	set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	cell tmp2;
	make_int(&tmp2, n);
	tmp2.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	set_var(q, p4, p4_ctx, &tmp2, q->st.curr_frame);
	return true;
}

static USE_RESULT bool fn_bread_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,variable);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	size_t len;

	if (is_integer(p1) && is_positive(p1)) {
		if (!str->data) {
			str->data = malloc(get_int(p1)+1);
			may_ptr_error(str->data);
			str->data_len = 0;
		}

		for (;;) {
			len = get_int(p1) - str->data_len;
			size_t nbytes = net_read(str->data+str->data_len, len, str);
			str->data_len += nbytes;
			str->data[str->data_len] = '\0';

			if (nbytes == len)
				break;

			if (feof(str->fp)) {
				free(str->data);
				str->data = NULL;
				return false;
			}

			if (q->is_task) {
				clearerr(str->fp);
				return do_yield_0(q, 1);
			}
		}

		cell tmp;
		may_error(make_stringn(&tmp, str->data, str->data_len), free(str->data));
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		free(str->data);
		str->data = NULL;
		return true;
	}

	if (is_integer(p1)) {
		if (!str->data) {
			str->data = malloc((str->alloc_nbytes=1024)+1);
			may_ptr_error(str->data);
			str->data_len = 0;
		}

		size_t nbytes = net_read(str->data, str->alloc_nbytes, str);
		str->data[nbytes] = '\0';
		str->data = realloc(str->data, nbytes+1);
		may_ptr_error(str->data);
		cell tmp;
		may_error(make_stringn(&tmp, str->data, nbytes), free(str->data));
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		free(str->data);
		str->data = NULL;
		return true;
	}

	if (!str->data) {
		str->data = malloc((str->alloc_nbytes=1024)+1);
		may_ptr_error(str->data);
		str->data_len = 0;
	}

	for (;;) {
		size_t len = str->alloc_nbytes - str->data_len;
		size_t nbytes = net_read(str->data+str->data_len, len, str);
		str->data_len += nbytes;
		str->data[str->data_len] = '\0';

		if (!nbytes || feof(str->fp))
			break;

		if (str->alloc_nbytes == str->data_len) {
			str->data = realloc(str->data, (str->alloc_nbytes*=2)+1);
			may_ptr_error(str->data);
		}
	}

	cell tmp1;
	make_int(&tmp1, str->data_len);
	set_var(q, p1, p1_ctx, &tmp1, q->st.curr_frame);
	cell tmp2;

	if (str->data_len)
		may_error(make_stringn(&tmp2, str->data, str->data_len), free(str->data));
	else
		make_atom(&tmp2, g_nil_s);

	set_var(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
	unshare_cell(&tmp2);
	free(str->data);
	str->data = NULL;
	return true;
}

static USE_RESULT bool fn_bwrite_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,atom);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	const char *src = C_STR(q, p1);
	size_t len = C_STRLEN(q, p1);

	while (len) {
		CHECK_INTERRUPT();
		size_t nbytes = net_write(src, len, str);

		if (!nbytes) {
			if (feof(str->fp) || ferror(str->fp))
				return false; // can feof() happen on writing?
		}

		// TODO: make this yieldable

		clearerr(str->fp);
		len -= nbytes;
		src += nbytes;
	}

	return true;
}

static USE_RESULT bool fn_sys_put_chars_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	size_t len;

	if (is_cstring(p1)) {
		const char *src = C_STR(q, p1);
		size_t len = C_STRLEN(q, p1);
		net_write(src, len, str);
	} else if ((len = scan_is_chars_list(q, p1, p1_ctx, true)) > 0) {
		char *src = chars_list_to_string(q, p1, p1_ctx, len);
		net_write(src, len, str);
		free(src);
	} else if (is_nil(p1)) {
		;
	} else
		return throw_error(q, p1, p1_ctx, "type_error", "chars");

	return !ferror(str->fp);
}

static USE_RESULT bool fn_sys_put_chars_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);
	size_t len;

	if (is_cstring(p1)) {
		const char *src = C_STR(q, p1);
		size_t len = C_STRLEN(q, p1);
		net_write(src, len, str);
	} else if ((len = scan_is_chars_list(q, p1, p1_ctx, true)) > 0) {
		char *src = chars_list_to_string(q, p1, p1_ctx, len);
		net_write(src, len, str);
		free(src);
	} else if (is_nil(p1)) {
		;
	} else
		return throw_error(q, p1, p1_ctx, "type_error", "chars");

	return !ferror(str->fp);
}

builtins g_files_bifs[] =
{
	// ISO...

	{"open", 4, fn_iso_open_4, NULL, false, BLAH},
	{"close", 1, fn_iso_close_1, NULL, false, BLAH},
	{"close", 2, fn_iso_close_2, NULL, false, BLAH},
	{"read_term", 2, fn_iso_read_term_2, NULL, false, BLAH},
	{"read_term", 3, fn_iso_read_term_3, NULL, false, BLAH},
	{"read", 1, fn_iso_read_1, NULL, false, BLAH},
	{"read", 2, fn_iso_read_2, NULL, false, BLAH},
	{"write_canonical", 1, fn_iso_write_canonical_1, NULL, false, BLAH},
	{"write_canonical", 2, fn_iso_write_canonical_2, NULL, false, BLAH},
	{"write_term", 2, fn_iso_write_term_2, NULL, false, BLAH},
	{"write_term", 3, fn_iso_write_term_3, NULL, false, BLAH},
	{"writeq", 1, fn_iso_writeq_1, NULL, false, BLAH},
	{"writeq", 2, fn_iso_writeq_2, NULL, false, BLAH},
	{"write", 1, fn_iso_write_1, NULL, false, BLAH},
	{"write", 2, fn_iso_write_2, NULL, false, BLAH},
	{"nl", 0, fn_iso_nl_0, NULL, false, BLAH},
	{"nl", 1, fn_iso_nl_1, NULL, false, BLAH},
	{"at_end_of_stream", 0, fn_iso_at_end_of_stream_0, NULL, false, BLAH},
	{"at_end_of_stream", 1, fn_iso_at_end_of_stream_1, NULL, false, BLAH},
	{"set_stream_position", 2, fn_iso_set_stream_position_2, NULL, false, BLAH},
	{"flush_output", 0, fn_iso_flush_output_0, NULL, false, BLAH},
	{"flush_output", 1, fn_iso_flush_output_1, NULL, false, BLAH},
	{"put_char", 1, fn_iso_put_char_1, NULL, false, BLAH},
	{"put_char", 2, fn_iso_put_char_2, NULL, false, BLAH},
	{"put_code", 1, fn_iso_put_code_1, NULL, false, BLAH},
	{"put_code", 2, fn_iso_put_code_2, NULL, false, BLAH},
	{"put_byte", 1, fn_iso_put_byte_1, NULL, false, BLAH},
	{"put_byte", 2, fn_iso_put_byte_2, NULL, false, BLAH},
	{"get_char", 1, fn_iso_get_char_1, NULL, false, BLAH},
	{"get_char", 2, fn_iso_get_char_2, NULL, false, BLAH},
	{"get_code", 1, fn_iso_get_code_1, NULL, false, BLAH},
	{"get_code", 2, fn_iso_get_code_2, NULL, false, BLAH},
	{"get_byte", 1, fn_iso_get_byte_1, NULL, false, BLAH},
	{"get_byte", 2, fn_iso_get_byte_2, NULL, false, BLAH},
	{"peek_char", 1, fn_iso_peek_char_1, NULL, false, BLAH},
	{"peek_char", 2, fn_iso_peek_char_2, NULL, false, BLAH},
	{"peek_code", 1, fn_iso_peek_code_1, NULL, false, BLAH},
	{"peek_code", 2, fn_iso_peek_code_2, NULL, false, BLAH},
	{"peek_byte", 1, fn_iso_peek_byte_1, NULL, false, BLAH},
	{"peek_byte", 2, fn_iso_peek_byte_2, NULL, false, BLAH},
	{"current_input", 1, fn_iso_current_input_1, NULL, false, BLAH},
	{"current_output", 1, fn_iso_current_output_1, NULL, false, BLAH},
	{"set_input", 1, fn_iso_set_input_1, NULL, false, BLAH},
	{"set_output", 1, fn_iso_set_output_1, NULL, false, BLAH},
	{"stream_property", 2, fn_iso_stream_property_2, NULL, false, BLAH},


	// Edinburgh...

	{"seeing", 1, fn_edin_seeing_1, "-name", false, BLAH},
	{"telling", 1, fn_edin_telling_1, "-name", false, BLAH},
	{"seen", 0, fn_edin_seen_0, NULL, false, BLAH},
	{"told", 0, fn_edin_told_0, NULL, false, BLAH},
	{"redo", 1, fn_edin_redo_1, "+integer", false, BLAH},
	{"redo", 2, fn_edin_redo_2, "+stream,+integer", false, BLAH},
	{"tab", 1, fn_edin_tab_1, "+integer", false, BLAH},
	{"tab", 2, fn_edin_tab_2, "+stream,+integer", false, BLAH},

	{"getline", 1, fn_getline_1, "-string", false, BLAH},
	{"getline", 2, fn_getline_2, "+stream,-string", false, BLAH},
	{"getlines", 1, fn_getlines_1, "-list", false, BLAH},
	{"getlines", 2, fn_getlines_2, "+stream,-list", false, BLAH},
	{"load_files", 2, fn_load_files_2, NULL, false, BLAH},
	{"unload_files", 1, fn_unload_files_1, NULL, false, BLAH},
	{"getfile", 2, fn_getfile_2, "+string,-list", false, BLAH},
	{"loadfile", 2, fn_loadfile_2, "+string,-string", false, BLAH},
	{"savefile", 2, fn_savefile_2, "+string,+string", false, BLAH},
	{"rename_file", 2, fn_rename_file_2, "+string,+string", false, BLAH},
	{"copy_file", 2, fn_copy_file_2, "+string,+string", false, BLAH},
	{"directory_files", 2, fn_directory_files_2, "+pathname,-list", false, BLAH},
	{"delete_file", 1, fn_delete_file_1, "+string", false, BLAH},
	{"exists_file", 1, fn_exists_file_1, "+string", false, BLAH},
	{"access_file", 2, fn_access_file_2, "+string,+mode", false, BLAH},
	{"time_file", 2, fn_time_file_2, "+string,-real", false, BLAH},
	{"size_file", 2, fn_size_file_2, "+string,-integer", false, BLAH},
	{"exists_directory", 1, fn_exists_directory_1, "+string", false, BLAH},
	{"make_directory", 1, fn_make_directory_1, "+string", false, BLAH},
	{"make_directory_path", 1, fn_make_directory_path_1, "+string", false, BLAH},
	{"working_directory", 2, fn_working_directory_2, "-string,+string", false, BLAH},
	{"absolute_file_name", 3, fn_absolute_file_name_3, NULL, false, BLAH},
	{"chdir", 1, fn_chdir_1, "+string", false, BLAH},
	{"$put_chars", 1, fn_sys_put_chars_1, "+chars", false, BLAH},
	{"$put_chars", 2, fn_sys_put_chars_2, "+stream,+chars", false, BLAH},
	{"read_term_from_atom", 3, fn_read_term_from_atom_3, "+atom,?term,+list", false, BLAH},
	{"read_term_from_chars", 3, fn_read_term_from_chars_3, "+chars,?term,+list", false, BLAH},
	{"write_term_to_atom", 3, fn_write_term_to_atom_3, "?atom,?term,+list", false, BLAH},
	{"write_canonical_to_atom", 3, fn_write_canonical_to_chars_3, "?atom,?term,+list", false, BLAH},
	{"write_term_to_chars", 3, fn_write_term_to_chars_3, "?chars,?term,+list", false, BLAH},
	{"write_canonical_to_chars", 3, fn_write_canonical_to_chars_3, "?chars,?term,+list", false, BLAH},
	{"read_line_to_string", 2, fn_read_line_to_string_2, "+stream,-string", false, BLAH},
	{"read_file_to_string", 3, fn_read_file_to_string_3, "+string,-string,+options", false, BLAH},

	{"client", 5, fn_client_5, "+string,-string,-string,-stream,+list", false, BLAH},
	{"server", 3, fn_server_3, "+string,-stream,+list", false, BLAH},
	{"accept", 2, fn_accept_2, "+stream,-stream", false, BLAH},
	{"bread", 3, fn_bread_3, "+stream,+integer,-string", false, BLAH},
	{"bwrite", 2, fn_bwrite_2, "+stream,-string", false, BLAH},


#ifndef _WIN32
	{"popen", 4, fn_popen_4, "+atom,+atom,-stream,+list", false, BLAH},
#endif

	{0}
};

