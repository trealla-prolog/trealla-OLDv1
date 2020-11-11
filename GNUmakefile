GIT_VERSION := "$(shell git describe --abbrev=4 --dirty --always --tags)"
CFLAGS = -Isrc -I/usr/local/include -DUSE_OPENSSL=$(USE_OPENSSL) -DVERSION='$(GIT_VERSION)' -O3 $(OPT) -Wall -Wextra -D_GNU_SOURCE
LDFLAGS = -lreadline -L/usr/local/lib -lm

ifndef NOSSL
USE_OPENSSL = 1
LDFLAGS += -lssl -lcrypto
else
USE_OPENSSL = 0
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

ifdef LTO
CFLAGS += -flto=$(LTO)
LDFLAGS += -flto=$(LTO)
endif

OBJECTS = tpl.o history.o builtins.o library.o \
	parse.o print.o runtime.o \
	skiplist.o base64.o network.o utf8.o

ifndef NOLDLIBS
OBJECTS += lists.o dict.o apply.o http.o atts.o \
	error.o dcgs.o format.o charsio.o
CFLAGS += -DUSE_LDLIBS=1
else
CFLAGS += -DUSE_LDLIBS=0
endif

all: tpl

tpl: $(OBJECTS)
	$(CC) -o tpl $(OBJECTS) $(OPT) $(LDFLAGS)

profile:
	$(MAKE) 'OPT=$(OPT) -O0 -pg -DDEBUG -DFAULTINJECT_VAR=g_faultinject'

debug:
	$(MAKE) 'OPT=$(OPT) -O0 -g -DDEBUG -DFAULTINJECT_VAR=g_faultinject'

test:
	./tests/run.sh

clean:
	rm -f tpl *.o gmon.* vgcore.* *.core core core.* faultinject*

# from [gcc|clang] -MM *.c

base64.o: base64.c base64.h
builtins.o: builtins.c trealla.h internal.h skiplist.h utf8.h cdebug.h \
  network.h base64.h library.h builtins.h
history.o: history.c history.h utf8.h cdebug.h
library.o: library.c library.h
network.o: network.c internal.h skiplist.h utf8.h trealla.h cdebug.h \
  network.h
parse.o: parse.c internal.h skiplist.h utf8.h trealla.h cdebug.h \
  history.h library.h builtins.h
print.o: print.c internal.h skiplist.h utf8.h trealla.h cdebug.h \
  builtins.h network.h
runtime.o: runtime.c internal.h skiplist.h utf8.h trealla.h cdebug.h \
  history.h builtins.h
skiplist.o: skiplist.c skiplist.h cdebug.h
tpl.o: tpl.c history.h trealla.h cdebug.h
utf8.o: utf8.c utf8.h

# Library modules

# Needed for using gmake with non-GNU linkers...

UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
	OSFLAG = -m elf_x86_64
endif
ifeq ($(UNAME_S),FreeBSD)
	OSFLAG = -m elf_x86_64
endif

dict.o: library/dict.pl
	$(LD) $(OSFLAG) -r -b binary -o dict.o library/dict.pl

dcgs.o: library/dcgs.pl
	$(LD) $(OSFLAG) -r -b binary -o dcgs.o library/dcgs.pl

format.o: library/format.pl
	$(LD) $(OSFLAG) -r -b binary -o format.o library/format.pl

charsio.o: library/charsio.pl
	$(LD) $(OSFLAG) -r -b binary -o charsio.o library/charsio.pl

lists.o: library/lists.pl
	$(LD) $(OSFLAG) -r -b binary -o lists.o library/lists.pl

apply.o: library/apply.pl
	$(LD) $(OSFLAG) -r -b binary -o apply.o library/apply.pl

http.o: library/http.pl
	$(LD) $(OSFLAG) -r -b binary -o http.o library/http.pl

atts.o: library/atts.pl
	$(LD) $(OSFLAG) -r -b binary -o atts.o library/atts.pl

error.o: library/error.pl
	$(LD) $(OSFLAG) -r -b binary -o error.o library/error.pl
