CC := gcc -Wall -Wno-unused -pipe -std=gnu99 -march=nocona -m32
DEFS :=

MODE := normal
#MODE := libgc
#MODE := debug
#MODE := debugopt
#MODE := gdb
#MODE := efence

ifeq ($(MODE),debugopt)
  DEFS := -DCSM_CONF_DEBUG -DDEBUG
  CFLAGS := -O3 -g0 -fomit-frame-pointer -frename-registers $(DEFS)
  LDFLAGS := -Wl,-O2 -Wl,--enable-new-dtags -Wl,--sort-common -Wl,--strip-all
else
  ifeq ($(MODE),debug)
    DEFS := -DCSM_CONF_DEBUG -DDEBUG
    CFLAGS := -O0 -ggdb3 $(DEFS) 
#    CFLAGS := -O0 -ggdb3 $(DEFS) -fmudflap
    LDFLAGS := 
#    LDFLAGS := -lmudflap
  else
    ifeq ($(MODE),efence)
      DEFS := -DUSE_EFENCE
      CFLAGS := -O0 -ggdb3 $(DEFS)
      LDFLAGS := -lefence 
    else
    ifeq ($(MODE),gdb)
      DEFS :=
      CFLAGS := -O0 -ggdb3 $(DEFS)
      LDFLAGS := 
    else
    ifeq ($(MODE),libgc)
      DEFS :=
      CFLAGS := -O3 -g0 -fomit-frame-pointer -frename-registers -I $(HOME)/sys/usr/include $(DEFS)
      LDFLAGS := -Wl,-O2 -Wl,--enable-new-dtags -Wl,--sort-common -Wl,--strip-all -Wl,--rpath -L $(HOME)/sys/usr/lib -lgc
    else
      DEFS :=
      CFLAGS := -O3 -g0 -fomit-frame-pointer -frename-registers $(DEFS)
      LDFLAGS := -Wl,-O2 -Wl,--enable-new-dtags -Wl,--sort-common -Wl,--strip-all
    endif
    endif
    endif
  endif
endif

all: bin fib fib.debug

clean:
	rm -f fib fib.debug fib.c

bin:
	ln -s ../bin bin

gcd.c: gcd.cchr bin/cchr Makefile
	bin/cchr gcd.cchr
gcd: gcd.c cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o gcd gcd.c
gcd.p: gcd.c cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-generate -o gcd.a gcd.c
	time ./gcd.a 1 200000000 >/dev/null
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-use -o gcd.p gcd.c

stack: stack.c cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o stack stack.c
stack.c: stack.cchr bin/cchr Makefile
	bin/cchr stack.cchr


primes: primes.c cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o primes primes.c
primes.c: primes.cchr bin/cchr Makefile
	bin/cchr primes.cchr
primes.p: primes.c cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-generate -o primes.a primes.c
	time ./primes.a 32768 >/dev/null
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-use -o primes.p primes.c
primes.prep: primes.prep.c Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o primes.prep primes.prep.c
primes.prep.c: primes.c Makefile
	cpp -P $(DEFS) <primes.c | indent -kr -l127 >primes.prep.c

cslv: cslv.c cchr_csm.h Makefile
	$(CC) $(CFLAGS) -lm $(LDFLAGS) -o cslv cslv.c
cslv.c: cslv.cchr bin/cchr Makefile
	bin/cchr cslv.cchr
cslv.prep: cslv.prep.c Makefile
	$(CC) $(CFLAGS) -lm $(LDFLAGS) -o cslv.prep cslv.prep.c
cslv.prep.c: cslv.c Makefile
	cpp -P $(DEFS) <cslv.c | indent -kr -l127 >cslv.prep.c

fib: fib.c cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o fib fib.c
fib.p: fib.c cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-generate -o fib.a fib.c
	time ./fib.a 20000
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-use -o fib.p fib.c
fib.c: fib.cchr bin/cchr Makefile
	bin/cchr fib.cchr
fib.c.debug: fib.c cchr_csm.h Makefile
	cpp -P $(DEFS)  <fib.c | egrep -v '^#' | indent -kr -l1024 >fib.c.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o fib.c.debug fib.c.c
fib.debug: fib.c cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o fib.debug fib.c
fib.prep: fib.prep.c Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o fib.prep fib.prep.c
fib.prep.c: fib.c Makefile
	cpp -P $(DEFS) <fib.c | indent -kr -l127 >fib.prep.c

fib2: fib2.c cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o fib2 fib2.c
fib2.p: fib2.c cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-generate -o fib2.a fib2.c
	time ./fib2.a 20000
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-use -o fib2.p fib2.c
fib2.c: fib2.cchr bin/cchr Makefile
	bin/cchr fib2.cchr
fib2.prep: fib2.prep.c Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o fib2.prep fib2.prep.c
fib2.prep.c: fib2.c Makefile
	cpp -P $(DEFS) <fib2.c | indent -kr -l127 >fib2.prep.c

fib_topdown.c: fib_topdown.cchr bin/cchr Makefile
	bin/cchr fib_topdown.cchr
fib_topdown: fib_topdown.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o fib_topdown fib_topdown.c

chapeau.c: chapeau.cchr bin/cchr Makefile
	bin/cchr chapeau.cchr
chapeau: chapeau.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o chapeau chapeau.c
chapeau.prep.c: chapeau.c ../cchr_csm.h Makefile
	cpp -P $(DEFS) <chapeau.c | indent -kr -l127 >chapeau.prep.c
chapeau.prep: chapeau.prep.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o chapeau.prep chapeau.prep.c

tak.c: tak.cchr bin/cchr Makefile
	bin/cchr tak.cchr

leq.c: leq.cchr cchr_csm.h bin/cchr Makefile
	bin/cchr leq.cchr
leq: leq.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o leq leq.c
leq.p: leq.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-generate -o leq.a leq.c
	./leq.a 150
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-use -o leq.p leq.c
leq.prep: leq.prep.c Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o leq.prep leq.prep.c

leq.prep.c: leq.c Makefile
	cpp -P $(DEFS) <leq.c | indent -kr -l127 >leq.prep.c

fib_topdown.prep.c: fib_topdown.c Makefile
	cpp -P $(DEFS) <fib_topdown.c | indent -kr -l127 >fib_topdown.prep.c


gcd.prep.c: cchr_csm.h gcd.c Makefile
	cpp -P $(DEFS) <gcd.c | indent -kr -l127 >gcd.prep.c


dijkstra.prep.c: dijkstra.c ../cchr_csm.h Makefile
	cpp -P $(DEFS) <dijkstra.c | indent -kr -l127 >dijkstra.prep.c
dijkstra.p: dijkstra.c cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-generate -o dijkstra.a dijkstra.c
	time ./dijkstra.a 5000 >/dev/null
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-use -o dijkstra.p dijkstra.c

ram.prep.c: ram.c ../cchr_csm.h Makefile
	cpp -P $(DEFS) <ram.c | indent -kr -l127 >ram.prep.c
ram.c: ram.cchr bin/cchr Makefile
	bin/cchr ram.cchr
ram: ram.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o ram ram.c
ram.p: ram.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-generate -o ram.a ram.c
	time ./ram.a 3400
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-use -o ram.p ram.c

ram_fib.prep.c: ram_fib.c ../cchr_csm.h Makefile
	cpp -P $(DEFS) <ram_fib.c | indent -kr -l127 >ram_fib.prep.c
ram_fib.c: ram_fib.cchr bin/cchr Makefile
	bin/cchr ram_fib.cchr
ram_fib: ram_fib.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o ram_fib ram_fib.c
ram_fib.p: ram_fib.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-generate -o ram_fib.a ram_fib.c
	time ./ram_fib.a 200
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-use -o ram_fib.p ram_fib.c

blur.c: blur.cchr bin/cchr Makefile
	bin/cchr blur.cchr
blur: blur.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -Wno-trigraphs -o blur blur.c


dijkstra.c: dijkstra.cchr bin/cchr Makefile
	bin/cchr dijkstra.cchr

merge.c: merge.cchr bin/cchr Makefile
	bin/cchr merge.cchr

acker.c: acker.cchr bin/cchr Makefile
	bin/cchr acker.cchr


tak: tak.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o tak tak.c


merge: merge.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o merge merge.c


fib_topdown.prep: fib_topdown.prep.c Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o fib_topdown.prep fib_topdown.prep.c


acker: acker.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o acker acker.c

tak2.c: tak2.cchr bin/cchr Makefile
	bin/cchr tak2.cchr
tak2.prep.c: tak2.c Makefile
	cpp -P $(DEFS) <tak2.c | indent -kr -l127 >tak2.prep.c
tak2.prep: tak2.prep.c Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o tak2.prep tak2.prep.c
tak2: tak2.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o tak2 tak2.c
tak2.p: tak2.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-generate -o tak2.a tak2.c
	./tak2.a 1000 900 810
	$(CC) $(CFLAGS) $(LDFLAGS) -fprofile-use -o tak2.p tak2.c

consts.c: consts.cchr bin/cchr Makefile
	bin/cchr consts.cchr
consts.prep.c: consts.c Makefile
	cpp -P $(DEFS) <consts.c | indent -kr -l127 >consts.prep.c
consts.prep: consts.prep.c Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -lm -o consts.prep consts.prep.c
consts: consts.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -lm -o consts consts.c

dijkstra: dijkstra.c ../cchr_csm.h Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o dijkstra dijkstra.c

dijkstra.prep: dijkstra.prep.c Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o dijkstra.prep dijkstra.prep.c



gcd.prep: gcd.prep.c Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o gcd.prep gcd.prep.c

tak.debug: tak.c ../cchr_csm.h Makefile
	cpp -P $(DEFS) <tak.c | indent -kr -l1024 >tak.debug.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o tak.debug tak.debug.c

fib_topdown.debug: fib_topdown.c ../cchr_csm.h ../logical.h ../dcls.h ../alist.h Makefile
	cpp -P $(DEFS) <fib_topdown.c | indent -kr -l1024 >fib_topdown.debug.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o fib_topdown.debug fib_topdown.debug.c

fib_gmp: fib_gmp.c cchr_csm.h Makefile
	$(CC) -lgmp $(CFLAGS) $(LDFLAGS) -o fib_gmp fib_gmp.c
fib_gmp.c: fib_gmp.cchr bin/cchr Makefile
	bin/cchr fib_gmp.cchr
fib_gmp.p: fib_gmp.c cchr_csm.h Makefile
	$(CC) -lgmp $(CFLAGS) $(LDFLAGS) -fprofile-generate -o fib_gmp.a fib_gmp.c
	time ./fib_gmp.a 3000 >/dev/null
	$(CC) -lgmp $(CFLAGS) $(LDFLAGS) -fprofile-use -o fib_gmp.p fib_gmp.c
fib_gmp.prep: fib_gmp.prep.c Makefile
	$(CC) -lgmp $(CFLAGS) $(LDFLAGS) -o fib_gmp.prep fib_gmp.prep.c
fib_gmp.prep.c: fib_gmp.c
	cpp -P $(DEFS) <fib_gmp.c | indent -kr -l127 >fib_gmp.prep.c


fib_h: fib_h.c cchr_csm.h Makefile
	cpp -P $(DEFS) fib_h.c | indent -kr -l 384 >fib_h.prep.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o fib_h fib_h.prep.c



logtest: logtest.c ../logical.h
	$(CC) $(CFLAGS) $(LDFLAGS) -o logtest logtest.c
logtest.c.debug: logtest.c ../logical.h
	cpp -P $(DEFS) <logtest.c | indent -kr >logtest.c.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o logtest.c.debug logtest.c.c
