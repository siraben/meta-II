CC = gcc
FUZZ-CC = afl-clang

CFLAGS =

HDRS = vm.h

EXE = vm

LIBS = 

OBJS = $(SRCS:.c=.o)

SRCS = main.c vm.c

$(OBJS): $(HDRS) Makefile

$(EXE): $(OBJS) $(HDRS) Makefile
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)

all: $(OBJS) $(HDRS) Makefile
	$(CC) $(CFLAGS) -o vm $(OBJS) $(LIBS)

.PHONY: clean
clean:
	rm -f core $(EXE) *.o


fuzz:
	$(FUZZCC) $(CCARGS)
	afl-fuzz -i in -o out ./vm @@
