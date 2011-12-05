#
#	Makefile for 'Bignums'	(c) C. Jullien 1999/01/23
#

.SUFFIXES: .cpp $(SUFFIXES)

CC	= gcc
CPP	= g++
DEBUG	=
CFLAGS	= $(DEBUG) -O2 -I. -Dlint
KRNLIB	= kern.lib
LISP	= clisp
OBJECT	= bztest.o bigz.o bign.o
WFLAGS		= -Wextra                  \
		  -Wall			   \
		                           \
		  -Waggregate-return       \
		  -Wcast-align	   	   \
		  -Wdisabled-optimization  \
		  -Wformat-extra-args	   \
#		  -Winline                 \
		  -Wmissing-declarations   \
		  -Wmissing-noreturn	   \
		  -Wmissing-prototypes     \
		  -Wnested-externs	   \
		  -Wold-style-definition   \
#		  -Woverlength-strings	   \
		  -Wpointer-arith	   \
		  -Wredundant-decls	   \
		  -Wsequence-point	   \
		  -Wshadow                 \
		  -Wstrict-prototypes	   \
		  -Wundef                  \
		  -Wunused                 \
#		  -Wunsafe-loop-optimizations \
		  -Wwrite-strings          \
#		  -Wunused-macros          \
#		  -Wconversion             \

all: bztest.exe testkern.exe tCBignum.exe

.c.o:
	@echo $*.c
	@$(CC) -c $(CFLAGS) $(WFLAGS) $*.c

.cpp.o:
	@echo $*.cpp
	@$(CPP) -c $(CFLAGS) $*.cpp

# KerN test

testkern.o:
	@$(CC) -c $(CFLAGS) -Itest -I. test/testkern.c

testkern.exe: bigz.o bign.o testkern.o
	@$(CC) $(CFLAGS) -otestkern testkern.o bigz.o bign.o

# BZ test

bztest.exe: $(OBJECT)
	@$(CC) $(CFLAGS) -o bztest.exe bztest.o bigz.o bign.o

bztest.o:
	@$(CC) -c $(CFLAGS) -Itest -I. test/bztest.c

# C++ test

tCBignum.exe: bigz.o bign.o CBignum.o tCBignum.o CBignum.h tCBignum.dat
	@$(CPP) $(CFLAGS) -o tCBignum tCBignum.o CBignum.o bigz.o bign.o

tCBignum.dat: gentest.lsp
	@echo generating tests with $(LISP)..
	@$(LISP) gentest.lsp

tCBignum.o: CBignum.h tCBignum.dat

# Misc

clean:
	del *.exe *.o *.BAK *.cgl
