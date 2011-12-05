#
#	Makefile for 'Bignums'	(c) C. Jullien 2009/12/23
#

.SUFFIXES: .lsp .dat .$(SUFFIXES)

CC	= tcc
CFLAGS	= -I. -Wall
OBJECT	= bztest.o bigz.o bign.o

all: bztest.exe testkern.exe

.c.o:
	@$(CC) -c $(CFLAGS) $*.c

# KerN test

testkern.exe: bigz.o bign.o testkern.o
	@$(CC) $(CFLAGS) -o testkern testkern.o bigz.o bign.o

testkern.o: test\testkern.c
	@$(CC) -c -Itest $(CFLAGS) test\testkern.c

# BZ test

bztest.exe: $(OBJECT)
	@$(CC) $(CFLAGS) -o bztest.exe bztest.o bigz.o bign.o

bztest.o: test\bztest.c
	@$(CC) -c -Itest $(CFLAGS) test\bztest.c

# Misc

clean:
	-del *~ *.exe *.o *.pdb *.ilk *.BAK *.cgl *pure.log *pure.ini /s

lint:
	set LARCH_PATH=/usr/share/splint/lib
	splint -predboolint \
	       -exportlocal \
	       -compdef \
	       +charint \
	       -booltype Boolean \
	       -Dlint \
	       bign.c
	splint -predboolint \
	       -exportlocal \
	       -compdef \
	       -nullret \
	       -nullpass \
	       -nullderef \
	       -branchstate \
	       -bufferoverflowhigh \
	       -formatconst \
	       +charint \
	       -booltype Boolean \
	       -Dlint \
	       bigz.c
