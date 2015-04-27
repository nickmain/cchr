CC := gcc
CFLAGS := -Wall -pipe -std=c99 -pedantic -march=nocona -m32
LDFLAGS := -lm -m32

INTDIR := intermediate

MODE := normal

ifeq ($(MODE),debug)
  CFLAGS := $(CFLAGS) -O0 -ggdb3 -fomit-frame-pointer
  LDFLAGS := $(LDFLAGS) -Wl,-O0 -Wl,--enable-new-dtags -Wl,--sort-common
  OUTDIR := debug
else
  ifeq ($(MODE),efence)
    CFLAGS := $(CFLAGS) -O0 -ggdb3 -DUSE_EFENCE
    LDFLAGS := $(LDFLAGS) -lefence -Wl,-O2 -Wl,--enable-new-dtags -Wl,--sort-common
    OUTDIR := efence
  else
    CFLAGS := $(CFLAGS) -O3 -g0 -fomit-frame-pointer
    LDFLAGS := $(LDFLAGS) -Wl,-O2 -Wl,--enable-new-dtags -Wl,--sort-common -Wl,--strip-all
    OUTDIR := bin
  endif
endif


###########
# GENERAL #
###########

all: $(INTDIR) $(OUTDIR) $(OUTDIR)/cchr

clean: 
	rm -rf $(INTDIR)/* $(OUTDIR)/*

###############
# DIRECTORIES #
###############

$(OUTDIR):
	mkdir $(OUTDIR)

$(INTDIR):
	mkdir $(INTDIR)

###############
# EXECUTABLES #
###############

$(OUTDIR)/cchr: $(OUTDIR)/parser.o $(OUTDIR)/lexer.o $(OUTDIR)/analyse.o $(OUTDIR)/parsestr.o $(OUTDIR)/main.o $(OUTDIR)/output.o $(OUTDIR)/codegen.o $(OUTDIR)/gio.o $(OUTDIR)/sugar_log.o Makefile
	$(CC) $(LDFLAGS) -o $(OUTDIR)/cchr $(OUTDIR)/parser.o $(OUTDIR)/lexer.o $(OUTDIR)/analyse.o $(OUTDIR)/parsestr.o $(OUTDIR)/main.o $(OUTDIR)/output.o $(OUTDIR)/codegen.o $(OUTDIR)/gio.o $(OUTDIR)/sugar_log.o

######################
# INTERMEDIATE FILES #
######################

$(INTDIR)/cchr.tab.c $(INTDIR)/cchr.tab.h: cchr.y Makefile
	bison -dv -o $(INTDIR)/cchr.tab.c cchr.y

$(INTDIR)/cchr.lex.c: $(INTDIR)/cchr.tab.h cchr.lex Makefile
	flex -o $(INTDIR)/cchr.lex.c cchr.lex

################
# OBJECT FILES #
################

$(OUTDIR)/parser.o: $(INTDIR)/cchr.tab.c parsestr.h alist.h Makefile
	$(CC) $(CFLAGS) -I . $(INTDIR)/cchr.tab.c -c -o $(OUTDIR)/parser.o

$(OUTDIR)/lexer.o: $(INTDIR)/cchr.tab.h $(INTDIR)/cchr.lex.c parsestr.h Makefile
	$(CC) $(CFLAGS) -I . $(INTDIR)/cchr.lex.c -c -o $(OUTDIR)/lexer.o

$(OUTDIR)/analyse.o: analyse.c semtree.h parsestr.h analyse.h alist.h gio.h Makefile
	$(CC) $(CFLAGS) analyse.c -c -o $(OUTDIR)/analyse.o

$(OUTDIR)/codegen.o: codegen.c semtree.h codegen.h alist.h output.h Makefile
	$(CC) $(CFLAGS) codegen.c -c -o $(OUTDIR)/codegen.o

$(OUTDIR)/parsestr.o: parsestr.c parsestr.h alist.h Makefile
	$(CC) $(CFLAGS) parsestr.c -c -o $(OUTDIR)/parsestr.o

$(OUTDIR)/gio.o: gio.c gio.h semtree.h alist.h Makefile
	$(CC) $(CFLAGS) -DNO_IDX=0 gio.c -c -o $(OUTDIR)/gio.o

$(OUTDIR)/output.o: output.c output.h Makefile
	$(CC) $(CFLAGS) output.c -c -o $(OUTDIR)/output.o

$(OUTDIR)/sugar_log.o: sugar_log.c sugar_log.h Makefile
	$(CC) $(CFLAGS) sugar_log.c -c -o $(OUTDIR)/sugar_log.o

$(OUTDIR)/main.o: main.c codegen.h analyse.h semtree.h parsestr.h alist.h output.h timings.h Makefile
	$(CC) $(CFLAGS) main.c -c -o $(OUTDIR)/main.o
