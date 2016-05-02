SHELL:=/usr/bin/env bash -O globstar
TESTS=$(sort $(wildcard tests/*.fun))
P4PROGS=$(patsubst %.fun, %.bin ,$(TESTS))
P4OUTS=$(patsubst %.fun,%.out,$(TESTS))
P4DIFFS=$(patsubst %.fun,%.diff,$(TESTS))
P4RESULTS=$(patsubst %.fun,%.result,$(TESTS))

PDPROGS=$(patsubst %.fun, %.pd.bin ,$(TESTS))
PDLLS=$(patsubst %.fun, %.ll ,$(TESTS))
PDOUTS=$(patsubst %.fun,%.pd.out,$(TESTS))
PDDIFFS=$(patsubst %.fun,%.pd.diff,$(TESTS))
P4RESULTS=$(patsubst %.fun,%.pd.result,$(TESTS))

SRC = src
SCALA_SOURCES = $(shell ls $(SRC)/**/*.scala)

.SECONDARY:

.PROCIOUS : %.o %.S %.out

CFLAGS=-g -std=gnu99 -O0 -Werror -Wall
CC=gcc
bin/souvikp4 : bin/souvikp4.o bin/parser.o Makefile
	@echo $(OUTS)
	$(CC) $(CFLAGS) -o bin/souvikp4 bin/souvikp4.o bin/parser.o

bin/%.o : references/%.c Makefile
	$(CC) $(CFLAGS) -MD -c references/$*.c -o bin/$*.o

bin/%.o : %.S Makefile
	$(CC) -MD -c $*.S

%.S : %.fun bin/souvikp4
	@echo "========== $* =========="
	./bin/souvikp4 < $*.fun > $*.S

p4progs : $(P4PROGS)

$(P4PROGS) : %.bin : %.S
	gcc -o $@ $*.S

p4outs : $(P4OUTS)

$(P4OUTS) : %.out : %.bin
	./$*.bin > $*.out

p4diffs : $(P4DIFFS)

$(P4DIFFS) : %.diff : Makefile %.out %.ok
	@(((diff -b $*.ok $*.out > /dev/null 2>&1) && (echo "===> $* ... pass")) || (echo "===> $* ... fail" ; echo "----- expected ------"; cat $*.ok ; echo "----- found -----"; cat $*.out)) > $*.diff 2>&1

$(P4RESULTS) : %.result : Makefile %.diff
	@cat $*.diff

p4test : Makefile $(P4DIFFS)
	@cat $(P4DIFFS)

lib/libfun.ll: Makefile
	clang -S -emit-llvm -o lib/libfun.ll lib/libfun.c

pd: $(SCALA_SOURCES)
	@./sbt compile
pdbins: $(PDLLS)

$(PDLLS): %.ll : $(TESTS) lib/libfun.ll
	@./sbt "project root" "run -i $*.fun -o $@"

$(PDPROGS): %.pd.bin : %.ll
	clang -Ofast -o $@ $*.ll

$(PDOUTS): %.pd.out : %.pd.bin
	./$*.pd.bin > $*.pd.out

$(PDDIFFS) : %.pd.diff : Makefile %.pd.out %.ok
	(((diff -b $*.ok $*.pd.out > /dev/null 2>&1) && (echo "===> $* ... pass")) || (echo "===> $* ... fail" ; echo "----- expected ------"; cat $*.ok ; echo "----- found -----"; cat $*.pd.out)) > $*.pd.diff 2>&1


pdtest: Makefile $(PDDIFFS)
	@cat $(PDDIFFS)

clean :
	rm -f $(PROGS)
	rm -f *.S
	rm -f *.out
	rm -f *.d
	rm -f *.o
	rm -f p4
	rm -f *.diff
	rm -rf tests/*.s
	rm -rf tests/*.S
	rm -rf tests/*.o
	rm -rf tests/*.out
	rm -rf tests/*.diff
	rm -rf tests/*.bin
	rm -rf tests/*.ll

-include *.d
