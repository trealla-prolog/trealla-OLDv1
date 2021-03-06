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

OBJECTS = tpl.o src/history.o \
	src/arith.o src/builtins.o src/contrib.o \
	src/library.o src/parse.o src/print.o src/runtime.o \
	src/skiplist.o src/base64.o src/network.o src/utf8.o

ifndef NOLDLIBS
OBJECTS += src/lists.o src/dict.o src/apply.o src/http.o src/atts.o \
	src/error.o src/dcgs.o src/format.o src/charsio.o
CFLAGS += -DUSE_LDLIBS=1
endif

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
	rm -f tpl src/*.o *.o gmon.* vgcore.* *.core core core.* faultinject.*

# from [gcc|clang] -MM *.c

src/arith.o: src/arith.c src/trealla.h src/internal.h src/skiplist.h \
 src/cdebug.h src/builtins.h
src/base64.o: src/base64.c src/base64.h
src/builtins.o: src/builtins.c src/trealla.h src/internal.h src/skiplist.h \
 src/cdebug.h src/network.h src/base64.h src/library.h src/utf8.h \
 src/builtins.h
src/contrib.o: src/contrib.c src/trealla.h src/internal.h src/skiplist.h \
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

# Library modules

# Needed for using gmake with non-GNU linkers...

UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
	OSFLAG = -m elf_x86_64
endif
ifeq ($(UNAME_S),FreeBSD)
	OSFLAG = -m elf_x86_64
endif

src/dict.o: library/dict.pl
	$(LD) $(OSFLAG) -r -b binary -o src/dict.o library/dict.pl

src/dcgs.o: library/dcgs.pl
	$(LD) $(OSFLAG) -r -b binary -o src/dcgs.o library/dcgs.pl

src/format.o: library/format.pl
	$(LD) $(OSFLAG) -r -b binary -o src/format.o library/format.pl

src/charsio.o: library/charsio.pl
	$(LD) $(OSFLAG) -r -b binary -o src/charsio.o library/charsio.pl

src/lists.o: library/lists.pl
	$(LD) $(OSFLAG) -r -b binary -o src/lists.o library/lists.pl

src/apply.o: library/apply.pl
	$(LD) $(OSFLAG) -r -b binary -o src/apply.o library/apply.pl

src/http.o: library/http.pl
	$(LD) $(OSFLAG) -r -b binary -o src/http.o library/http.pl

src/atts.o: library/atts.pl
	$(LD) $(OSFLAG) -r -b binary -o src/atts.o library/atts.pl

src/error.o: library/error.pl
	$(LD) $(OSFLAG) -r -b binary -o src/error.o library/error.pl
