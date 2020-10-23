GIT_VERSION := "$(shell git describe --abbrev=4 --dirty --always --tags)"
CFLAGS = -Isrc -I/usr/local/include -DUSE_OPENSSL=$(USE_OPENSSL) -DVERSION='$(GIT_VERSION)' -O3 $(OPT) -Wall -D_GNU_SOURCE
LDFLAGS = -lreadline -L/usr/local/lib -lm

.ifndef NOSSL
USE_OPENSSL = 1
LDFLAGS += -lssl -lcrypto
.else
USE_OPENSSL = 0
.endif

.ifdef INT128
CFLAGS += -DUSE_INT128=1
.else .ifdef INT32
CFLAGS += -DUSE_INT32=1
.endif

OBJECTS = tpl.o history.o builtins.o library.o \
	parse.o print.o runtime.o \
	skiplist.o base64.o network.o utf8.o \
	lists.o dict.o apply.o http.o atts.o error.o

all: tpl

tpl: $(OBJECTS)
	$(CC) -o tpl $(OBJECTS) $(OPT) $(LDFLAGS)

profile:
	$(MAKE) 'OPT=$(OPT) -O0 -pg -DDEBUG'

debug:
	$(MAKE) 'OPT=$(OPT) -O0 -g -DDEBUG'

test:
	./tests/run.sh

clean:
	rm -f tpl *.o *.out gmon.* *.core

# from [gcc|clang] -MM *.c

base64.o: base64.c base64.h
builtins.o: builtins.c trealla.h internal.h skiplist.h utf8.h network.h \
 base64.h builtins.h
history.o: history.c history.h utf8.h
library.o: library.c library.h
network.o: network.c internal.h skiplist.h utf8.h network.h
parse.o: parse.c internal.h skiplist.h utf8.h history.h library.h \
 trealla.h builtins.h
print.o: print.c internal.h skiplist.h utf8.h builtins.h network.h
runtime.o: runtime.c internal.h skiplist.h utf8.h history.h builtins.h
skiplist.o: skiplist.c skiplist.h
tpl.o: tpl.c history.h trealla.h
utf8.o: utf8.c utf8.h

# Library modules

dict.o: library/dict.pl
	$(LD) -m elf_x86_64 -r -b binary -o dict.o library/dict.pl

lists.o: library/lists.pl
	$(LD) -m elf_x86_64 -r -b binary -o lists.o library/lists.pl

apply.o: library/apply.pl
	$(LD) -m elf_x86_64 -r -b binary -o apply.o library/apply.pl

http.o: library/http.pl
	$(LD) -m elf_x86_64 -r -b binary -o http.o library/http.pl

atts.o: library/atts.pl
	$(LD) -m elf_x86_64 -r -b binary -o atts.o library/atts.pl

error.o: library/error.pl
	$(LD) -m elf_x86_64 -r -b binary -o error.o library/error.pl
