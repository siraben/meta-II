CC=gcc
FUZZCC=afl-clang

CCARGS = -o vm vm.c
all:
	$(CC) $(CCARGS)
fuzz:
	$(FUZZCC) $(CCARGS)
	afl-fuzz -i in -o out ./parser @@
