GIT_VERSION := "$(shell git describe --abbrev=4 --dirty --always --tags)"
CFLAGS = -Isrc -I/usr/local/include -DVERSION='$(GIT_VERSION)' -O3 $(OPT) -Wall -Wextra -D_GNU_SOURCE
LDFLAGS = -lreadline -L/usr/local/lib -lm

ifndef NOSSL
CFLAGS += -DUSE_OPENSSL=1
LDFLAGS += -lssl -lcrypto
endif

ifdef INT128
CFLAGS += -DUSE_INT128=1
else ifdef INT32
CFLAGS += -DUSE_INT32=1
endif

ifdef GMP
CFLAGS += -DUSE_GMP=1
LDFLAGS += -lgmp
endif

ifdef THREADS
CFLAGS += -DUSE_THREADS=1 -pthread
LDFLAGS += -pthread
endif

ifdef LTO
CFLAGS += -flto=$(LTO)
LDFLAGS += -flto=$(LTO)
endif

OBJECTS = tpl.o src/history.o src/functions.o \
	src/predicates.o src/contrib.o src/heap.c \
	src/library.o src/parse.o src/print.o src/runtime.o \
	src/skiplist.o src/base64.o src/network.o src/utf8.o

OBJECTS +=  library/builtin.o \
	library/lists.o  library/dict.o  library/apply.o \
	library/http.o  library/atts.o library/error.o  library/dcgs.o  \
	library/format.o  library/charsio.o

library/%.c: library/%.pl
	xxd -i $^ $@
all: tpl

tpl: $(OBJECTS)
	$(CC) -o tpl $(OBJECTS) $(OPT) $(LDFLAGS)

profile:
	$(MAKE) 'OPT=$(OPT) -O0 -pg -DDEBUG -DFAULTINJECT_NAME=g_faultinject'

debug:
	$(MAKE) 'OPT=$(OPT) -O0 -g -DDEBUG -DFAULTINJECT_NAME=g_faultinject'

test:
	./tests/run.sh

clean:
	rm -f tpl src/*.o library/*.o library/*.c *.o gmon.* vgcore.* *.core core core.* faultinject.*

# from [gcc|clang] -MM *.c

src/functions.o: src/functions.c src/trealla.h src/internal.h src/skiplist.h \
 src/cdebug.h src/builtins.h
src/base64.o: src/base64.c src/base64.h
src/predicates.o: src/predicates.c src/trealla.h src/internal.h src/skiplist.h \
 src/cdebug.h src/network.h src/base64.h src/library.h src/utf8.h \
 src/builtins.h
src/contrib.o: src/contrib.c src/trealla.h src/internal.h src/skiplist.h \
 src/cdebug.h src/builtins.h
src/heap.o: src/heap.c src/trealla.h src/internal.h \
 src/cdebug.h src/builtins.h
src/history.o: src/history.c src/history.h src/utf8.h src/cdebug.h
src/library.o: src/library.c src/library.h
src/network.o: src/network.c src/internal.h src/skiplist.h src/trealla.h \
 src/cdebug.h src/network.h
src/parse.o: src/parse.c src/internal.h src/skiplist.h src/trealla.h \
 src/cdebug.h src/history.h src/library.h src/builtins.h src/utf8.h
src/print.o: src/print.c src/internal.h src/skiplist.h src/trealla.h \
 src/cdebug.h src/builtins.h src/network.h src/utf8.h
src/runtime.o: src/runtime.c src/internal.h src/skiplist.h src/trealla.h \
 src/cdebug.h src/history.h src/builtins.h
src/skiplist.o: src/skiplist.c src/skiplist.h src/cdebug.h
src/utf8.o: src/utf8.c src/utf8.h
