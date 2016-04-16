TESTS=$(sort $(wildcard tests/*.fun))
PROGS=$(subst .fun,,$(TESTS))
OUTS=$(patsubst %.fun,%.out,$(TESTS))
DIFFS=$(patsubst %.fun,%.diff,$(TESTS))
RESULTS=$(patsubst %.fun,%.result,$(TESTS))

.SECONDARY:

.PROCIOUS : %.o %.S %.out

CFLAGS=-g -std=gnu99 -O0 -Werror -Wall
CC=gcc
LLVM_C=clang

pd : pd.o parser.o Makefile
	$(CC) $(CFLAGS) -o pd pd.o parser.o

%.o : %.c Makefile
	$(CC) $(CFLAGS) -MD -c $*.c

%.ll : %.fun pd
	@echo "========== $* =========="
	./pd < $*.fun > $*.ll

progs : $(PROGS)

$(PROGS) : % : %.ll
	$(LLVM_C) -o $@ $@.ll

outs : $(OUTS)

$(OUTS) : %.out : %
	./$* > $*.out

diffs : $(DIFFS)

$(DIFFS) : %.diff : Makefile %.out %.ok
	@(((diff -b $*.ok $*.out > /dev/null 2>&1) && (echo "===> $* ... pass")) || (echo "===> $* ... fail" ; echo "----- expected ------"; cat $*.ok ; echo "----- found -----"; cat $*.out)) > $*.diff 2>&1

$(RESULTS) : %.result : Makefile %.diff
	@cat $*.diff

test : Makefile $(DIFFS)
	@cat $(DIFFS)

clean :
	rm -f $(PROGS)
	rm -f *.ll
	rm -f *.out
	rm -f *.d
	rm -f *.o
	rm -f pd
	rm -f *.diff

-include *.d
