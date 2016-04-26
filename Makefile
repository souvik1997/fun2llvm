TESTS=$(sort $(wildcard tests/*.fun))
P4PROGS=$(patsubst %.fun, %.bin ,$(TESTS))
P4OUTS=$(patsubst %.fun,%.out,$(TESTS))
P4DIFFS=$(patsubst %.fun,%.diff,$(TESTS))
P4RESULTS=$(patsubst %.fun,%.result,$(TESTS))

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

p4diffs : $(p4DIFFS)

$(P4DIFFS) : %.diff : Makefile %.out %.ok
	@(((diff -b $*.ok $*.out > /dev/null 2>&1) && (echo "===> $* ... pass")) || (echo "===> $* ... fail" ; echo "----- expected ------"; cat $*.ok ; echo "----- found -----"; cat $*.out)) > $*.diff 2>&1

$(P4RESULTS) : %.result : Makefile %.diff
	@cat $*.diff

p4test : Makefile $(P4DIFFS)
	@cat $(P4DIFFS)

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

-include *.d
