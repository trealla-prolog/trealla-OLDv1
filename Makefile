GIT_VERSION := "$(shell git describe --abbrev=4 --dirty --always --tags)"

CFLAGS = -Isrc -I/usr/local/include -DVERSION='$(GIT_VERSION)' -O3 \
	-Wall -Wextra -D_GNU_SOURCE -Wno-deprecated-declarations \
	-funsigned-char $(OPT) \
	-Wno-unused-function -Wno-unused-parameter -Wno-unused-variable

LDFLAGS = -L/usr/local/lib -lreadline -lm

ifdef ISOCLINE
CFLAGS += -DUSE_ISOCLINE=1
endif

ifndef NOSSL
CFLAGS += -DUSE_OPENSSL=1
LDFLAGS += -L/usr/local/opt/openssl/lib -lssl -lcrypto
endif

ifdef THREADS
CFLAGS += -DUSE_THREADS=1 -pthread
LDFLAGS += -pthread
endif

ifdef LTO
CFLAGS += -flto=$(LTO)
LDFLAGS += -flto=$(LTO)
endif

SRCOBJECTS = tpl.o src/history.o src/functions.o \
	src/predicates.o src/files.o src/contrib.o src/heap.c \
	src/control.o src/library.o src/module.o src/parser.o \
	src/print.o src/prolog.o src/query.o src/format.o src/unify.o \
	src/skiplist.o src/base64.o src/network.o src/utf8.o

LIBOBJECTS +=  library/builtins.o library/lists.o library/apply.o \
	library/http.o library/atts.o library/error.o library/dcgs.o \
	library/format.o library/charsio.o library/freeze.o \
	library/ordsets.o library/assoc.o library/dict.o library/dif.o \
	library/pairs.o library/random.o \
	library/lambda.o library/when.o

SRCOBJECTS += src/imath/imath.o

ifdef ISOCLINE
SRCOBJECTS += src/isocline/src/isocline.o
endif

OBJECTS = $(SRCOBJECTS) $(LIBOBJECTS) src/version.o

library/%.c: library/%.pl
	xxd -i $^ $@

all: tpl

tpl: $(OBJECTS) Makefile README.md LICENSE
	rm src/version.o
	$(CC) $(CFLAGS) -o src/version.o -c src/version.c
	$(CC) -o tpl $(OBJECTS) $(OPT) $(LDFLAGS)

sandbox:
	$(MAKE) 'OPT=$(OPT) -O0 -pg -DSANDBOX'

profile:
	$(MAKE) 'OPT=$(OPT) -O0 -pg -DDEBUG'

debug:
	$(MAKE) 'OPT=$(OPT) -O0 -g -DDEBUG'

release:
	$(MAKE) 'OPT=$(OPT) -DNDEBUG'

test:
	./tests/run.sh

clean:
	rm -f tpl src/*.o src/imath/*.o src/isocline/src/*.o \
		library/*.o library/*.c *.o gmon.* \
		vgcore.* *.core core core.*
	rm -f *.itf *.po samples/*.itf samples/*.po

# from [gcc|clang] -MM src/*.c src/imath/*.c src/isocline/src/isocline.c

src/base64.o: src/base64.c src/base64.h
src/contrib.o: src/contrib.c src/trealla.h src/internal.h src/map.h \
  src/skiplist.h src/cdebug.h src/imath/imath.h src/builtins.h
src/control.o: src/control.c src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/imath/imath.h src/parser.h src/module.h \
  src/prolog.h src/query.h src/builtins.h src/heap.h
src/files.o: src/files.c src/trealla.h src/internal.h src/map.h \
  src/skiplist.h src/cdebug.h src/imath/imath.h src/builtins.h
src/format.o: src/format.c src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/imath/imath.h src/network.h \
  src/parser.h src/module.h src/prolog.h src/query.h src/builtins.h \
  src/utf8.h
src/functions.o: src/functions.c src/trealla.h src/internal.h src/map.h \
  src/skiplist.h src/cdebug.h src/imath/imath.h src/query.h src/module.h \
  src/prolog.h src/builtins.h src/heap.h
src/heap.o: src/heap.c src/internal.h src/map.h src/skiplist.h src/trealla.h \
  src/cdebug.h src/imath/imath.h src/query.h src/builtins.h src/heap.h
src/history.o: src/history.c src/history.h src/utf8.h src/cdebug.h src/isocline/include/isocline.h
src/library.o: src/library.c src/library.h
src/module.o: src/module.c src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/imath/imath.h src/parser.h src/module.h \
  src/prolog.h src/query.h src/builtins.h src/utf8.h
src/network.o: src/network.c src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/imath/imath.h src/network.h
src/parser.o: src/parser.c src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/imath/imath.h src/library.h \
  src/parser.h src/module.h src/prolog.h src/query.h src/builtins.h \
  src/utf8.h
src/predicates.o: src/predicates.c src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/imath/imath.h src/network.h \
  src/base64.h src/library.h src/parser.h src/module.h src/prolog.h \
  src/query.h src/builtins.h src/heap.h src/utf8.h src/history.h
src/print.o: src/print.c src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/imath/imath.h src/parser.h src/module.h \
  src/query.h src/builtins.h src/network.h src/heap.h src/utf8.h
src/prolog.o: src/prolog.c src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/imath/imath.h src/library.h \
  src/parser.h src/module.h src/prolog.h
src/query.o: src/query.c src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/imath/imath.h src/history.h \
  src/parser.h src/module.h src/prolog.h src/query.h src/builtins.h \
  src/heap.h src/utf8.h
src/skiplist.o: src/skiplist.c src/skiplist.h
src/unify.o: src/unify.c src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/imath/imath.h src/history.h \
  src/parser.h src/module.h src/prolog.h src/query.h src/builtins.h \
  src/heap.h src/utf8.h
src/utf8.o: src/utf8.c src/utf8.h
src/version.o: src/version.c
src/imath/imath.o: src/imath/imath.c src/imath/imath.h
src/isocline/src/isocline.o: src/isocline/src/isocline.c src/isocline/include/isocline.h
