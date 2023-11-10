SCHEME      = guile
CC          = gcc
CFLAGS      = -Wall -pedantic -O2 -g -std=c2x
FILENAMES  := ${wildcard tests/*.scm}
SOURCES    := ${SOURCES:.scm=}
OBJECTS    := ${FILENAMES:.scm=.o}

%:
	$(SCHEME) petty_scheme.scm tests output $@
	$(CC) $(CFLAGS) -c output/$@.s -o obj/$@.o
	$(CC) $(CFLAGS) scheme_entry.c obj/$@.o -o bin/$@
