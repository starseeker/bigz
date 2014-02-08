/*
static const char sccsid[] = "$Id: testkern.c,v 1.16 2013/06/18 05:21:59 jullien Exp $;
*/

/*
 * Simplified BSD License
 *
 * Copyright (c) 1988-1989, Digital Equipment Corporation & INRIA.
 * Copyright (c) 1992-2012, Eligis
 * All rights reserved.
 *
 * Redistribution and  use in  source and binary  forms, with  or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * o Redistributions  of  source  code must  retain  the  above copyright
 *   notice, this list of conditions and the following disclaimer.
 * o Redistributions  in  binary form  must reproduce the above copyright
 *   notice, this list of conditions and  the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE  IS PROVIDED BY  THE COPYRIGHT HOLDERS  AND CONTRIBUTORS
 * "AS  IS" AND  ANY EXPRESS  OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT
 * LIMITED TO, THE IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE  ARE DISCLAIMED. IN NO EVENT  SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL,  EXEMPLARY,  OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE  GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS  INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF  LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY,  OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING  IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 *	testkern.c :	
 */

/*
 * You  can  comment  the  line below if you want to test the C macro
 * Package instead of C or Assembly functions.
 */

#if	defined(_WIN32)      || \
	defined(_WIN64)      || \
	defined(__MINGW32__) || \
	defined(__MINGW64__)
#define	_CRT_SECURE_NO_DEPRECATE	1
#define	_CRT_NONSTDC_NO_DEPRECATE	1
#endif	/* _WIN32 || _WIN64 || __MINGW32__ || __MINGW64__ */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include "bign.h"
#include "BntoBnn.h"

static uint64_t NsTime = 0;
#define BN_CLOCK_PRECISION ((uint64_t)1000000000)

#if defined(HAVE_LIBRT) && defined(CLOCK_PROCESS_CPUTIME_ID)
#define	BN_CLOCK_TYPE	   CLOCK_PROCESS_CPUTIME_ID

static void show_time(struct timespec* start, struct timespec* end);

static void
show_time(struct timespec* start, struct timespec* end)
{
	static const uint64_t clockPrecision = BN_CLOCK_PRECISION;
	struct timespec temp;

	if ((end->tv_nsec - start->tv_nsec) < 0) {
		temp.tv_sec  = end->tv_sec - start->tv_sec - 1;
		temp.tv_nsec = clockPrecision + end->tv_nsec-start->tv_nsec;
	} else {
		temp.tv_sec  = end->tv_sec  - start->tv_sec;
		temp.tv_nsec = end->tv_nsec - start->tv_nsec;
	}

	NsTime = temp.tv_sec * clockPrecision + temp.tv_nsec;
}

static struct timespec _start_time;
static struct timespec _stop_time;

#define	CLOCK_START	clock_gettime(BN_CLOCK_TYPE, &_start_time);
#define	CLOCK_STOP	clock_gettime(BN_CLOCK_TYPE, &_stop_time);\
			show_time(&_start_time, &_stop_time);
#else
#define	CLOCK_START
#define	CLOCK_STOP
#endif

/*
 *	test structure.
 */

struct testenv {
	char	*name;		/* function name	*/
	int	flag;		/* continue flag	*/
	char	hist[2048];	/* current expression	*/
	char	*depend;	/* dependant list	*/
};

typedef struct testenv TestEnv;

extern void	seetest(int n);
extern void	dotest(TestEnv *e, int n, int repeat);
extern void	ErrorPrint(TestEnv *e);
extern void	ResetTest(int n);
extern int	CheckSubRange(int x, int nd, int nl);
extern int	Check(int n);
extern void	RangeNumberPrint(char *s, BigNum n, int nd, int nl);
extern void	ShowSubNumber(int x, char *n, int nd, int nl);
extern void	ShowOutRange(int x, char *n, int nd, int nl);
extern int	ShowDiff0(TestEnv *e, int r1, int r2);
extern int	ShowDiff1(TestEnv *e, int r1, int r2, char *n, int nd, int nl);
extern int	ShowDiff2(TestEnv *e, int r1, int r2, char *n, int nd, int nl, char *m, int md, int ml);
extern int	ShowDiff3(TestEnv *e, int r1, int r2, char *n, int nd, int nl, char *m, int md, int ml, char *o, int od, int ol);
extern int	ShowDiff4(TestEnv *e, int r1, int r2, char *n, int nd, int nl, char *m, int md, int ml, char *o, int od, int ol, char *p, int pd, int pl);
extern int	Generique(TestEnv *e);
extern void	___BnSetToZero___(BigNum n, int nd, int nl);
extern int	TestBnSetToZero(TestEnv *e);
extern void	___BnAssign___(BigNum m, int md, BigNum n, int nd, int nl);
extern int	TestBnAssign(TestEnv *e);
extern int	___BnNumDigits___(BigNum n, int nd, int nl);
extern int	TestBnNumDigits(TestEnv *e);
extern int	___BnNumLeadingZeroBitsInDigit___(BigNum n, int nd);
extern int	TestBnNumLeadingZeroBitsInDigit(TestEnv *e);
extern int	___BnIsDigitZero___(BigNum n, int nd);
extern int	TestBnIsDigitZero(TestEnv *e);
extern int	___BnIsDigitNormalized___(BigNum n, int nd);
extern int	TestBnIsDigitNormalized(TestEnv *e);
extern int	___BnIsDigitOdd___(BigNum n, int nd);
extern int	TestBnIsDigitOdd(TestEnv *e);
extern int	___BnCompareDigits___(BigNum n, int nd, BigNum m, int md);
extern int	TestBnCompareDigits(TestEnv *e);
extern void	___BnComplement___(BigNum n, int nd, int nl);
extern int	TestBnComplement(TestEnv *e);
extern void	___BnAndDigits___(BigNum n, int nd, BigNum m, int md);
extern int	TestBnAndDigits(TestEnv *e);
extern void	___BnOrDigits___(BigNum n, int nd, BigNum m, int md);
extern int	TestBnOrDigits(TestEnv *e);
extern void	___BnXorDigits___(BigNum n, int nd, BigNum m, int md);
extern int	TestBnXorDigits(TestEnv *e);
extern void	___BnShiftLeft___(BigNum n, int nd, int nl, BigNum m, int md, int s);
extern int	TestBnShiftLeft(TestEnv *e);
extern void	___BnShiftRight___(BigNum n, int nd, int nl, BigNum m, int md, int s);
extern int	TestBnShiftRight(TestEnv *e);
extern int	___BnAddCarry___(BigNum n, int nd, int nl, int r);
extern int	TestBnAddCarry(TestEnv *e);
extern int	___BnAdd___(BigNum n, int nd, int nl, BigNum m, int md, int ml, int r);
extern int	TestBnAdd(TestEnv *e);
extern int	___BnSubtractBorrow___(BigNum n, int nd, int nl, int r);
extern int	TestBnSubtractBorrow(TestEnv *e);
extern int	___BnSubtract___(BigNum n, int nd, int nl, BigNum m, int md, int ml, int r);
extern int	TestBnSubtract(TestEnv *e);
extern int	___BnMultiplyDigit___(BigNum p, int pd, int pl, BigNum n, int nd, int nl, BigNum m, int md);
extern int	TestBnMultiplyDigit(TestEnv *e);
extern int	TestBnDivideDigit(TestEnv *e);

/*
 *	Predefined numbers.
 */

static BigNum 		NumbVect[5][2];
static BigNum 		NumbProto, Ntmp2, NtmpBig;

#define RN(n)		NumbVect[n][0]
#define SN(n) 		NumbVect[n][1]

/*
 *	Number size, i.e. 4(n + 1)
 */

#define TESTLENGTH 	16
#define DTL 		TESTLENGTH/2
#define QTL 		TESTLENGTH/4

/* Nombre de test. */
int TestCount;

typedef struct {
	int	(*TestFnt)(TestEnv *);
	char	*NameFnt;
} TESTONE;

extern	TESTONE AllTest[];

void
seetest(int n)
{
	printf("%d.	Testing %s\n", n, AllTest[n].NameFnt);
}

void
dotest(TestEnv *e, int n, int repeat)
{
	int i;
	seetest(n);
	TestCount = 0;
	e->name = AllTest[n].NameFnt;
	CLOCK_START
	for (i = 0; i < repeat; ++i) {
		if (((AllTest[n].TestFnt) (e)) && e->flag > 1)
			exit(0);
	}
	CLOCK_STOP
	if (NsTime) {
		double cpuTime;
		cpuTime = ((double)NsTime/(double)BN_CLOCK_PRECISION)/repeat;

		(void)printf("%d tests were performed in %10.8f s\n",
			     TestCount, (double)cpuTime);

	} else {
		(void)printf("%d tests were performed\n", TestCount);
	}
}

void
ErrorPrint(TestEnv *e)
{
	printf("*** Error in compute : %s\n", e->hist);
	printf("  Depend on %s\n", e->depend);
}

void
ResetTest(int n)
{
	/*
	 * Reset number n to default
	 */
	BnAssign(RN(n), 0, NumbProto, 0, TESTLENGTH);
	BnAssign(SN(n), 0, NumbProto, 0, TESTLENGTH);
}

int
CheckSubRange(int x, int nd, int nl)
{
	/*
	 * Compare sub-numbers (RN(x), nd, nl) and (SN(x), nd, nl)
	 */
	while(nl) {
		nl--;
		if(BnCompareDigits(RN(x), nd, SN(x), nd)) return(nd + 1);
		nd++;
	}
	return( BN_FALSE) ;
}

int
Check(int n)
{
	int	i;
	/*
	 * Check computed agains expected numbers.
	 */
	for(i = 0; i < n; i++)
		if(CheckSubRange(i, 0, TESTLENGTH)) return(1);
	return( BN_FALSE );
}

void
RangeNumberPrint(char *s, BigNum n, int nd, int nl)
{
	int	first = 1;
	int	bnsize = sizeof(BigNumDigit);

	/* Ne marche que si BnGetDigit est garanti!!! */
	printf("%s {", s);
	while(nl) {
		size_t p = (size_t)BnGetDigit(n, nd + nl);
		nl--;
		if(!first)
			printf(", ");
		else first = 0;

		if( bnsize == 4 ) {
			printf("%.8x", (unsigned int)p);
		} else	{
			unsigned int low;
			unsigned int high;
			low = (unsigned int)(p & 0xffffffff);

			/*
			 * shift p in two operations to fool 32bit compilers.
			 */
			p >>= 16;
			high = (unsigned int)(p >> 16);
			printf("%.8x", high);
			printf("%.8x", low);
		}
	}
	printf("}\n");
}

void
ShowSubNumber(int x, char *n, int nd, int nl)
{
	printf("[%s, %d, %d] =	", n, nd, nl);
	RangeNumberPrint("", RN(x), nd, nl);
	if(CheckSubRange(x, nd, nl)) {
		RangeNumberPrint(" Before:	", NumbProto, nd, nl);
		RangeNumberPrint(" Simulated:	", SN(x), nd, nl);
}	}


char *msg = "---- Modification Out of Range of number ";

void
ShowOutRange(int x, char *n, int nd, int nl)
{
	int	i   = 0;
	int	bol = 0;

	while((i = CheckSubRange(x, i, TESTLENGTH - i)) != 0) {
		if((i <= nd) || (i > nd + nl)) {
			if(!bol) {
				bol = 1;
				printf("%s %s at index: (%d", msg, n, i - 1);
			} else {
				printf(" %d", i - 1);
	}	}	}
	if(bol) printf(").\n");
}		

int
ShowDiff0(TestEnv *e, int r1, int r2)
{
	ErrorPrint(e);
	if(r1 != r2)
		printf("---- Result is %d and must be %d----\n", r1, r2);
	return(e->flag);
}

int
ShowDiff1(TestEnv *e, int r1, int r2, char *n, int nd, int nl)
{
	ErrorPrint(e);
	if(r1 != r2)
		printf("---- Result is %d and must be %d----\n", r1, r2);
	ShowOutRange(0, n, nd, nl);
	ShowSubNumber(0, n, nd, nl);
	return(e->flag);
}

int
ShowDiff2(TestEnv *e, int r1, int r2, char *n, int nd, int nl, char *m, int md, int ml)
{
	ErrorPrint(e);
	if(r1 != r2)
		printf("---- Result is %d and must be %d----\n", r1, r2);
	ShowOutRange(0, n, nd, nl);
	ShowOutRange(1, m, md, ml);
	ShowSubNumber(0, n, nd, nl);
	ShowSubNumber(1, m, md, ml);
	return(e->flag);
}

int
ShowDiff3(TestEnv *e, int r1, int r2, char *n, int nd, int nl, char *m, int md, int ml, char *o, int od, int ol)
{
	ErrorPrint(e);
	if(r1 != r2)
		printf("---- Result is %d and must be %d----\n", r1, r2);
	ShowOutRange(0, n, nd, nl);
	ShowOutRange(1, m, md, ml);
	ShowOutRange(2, o, od, ol);
	ShowSubNumber(0, n, nd, nl);
	ShowSubNumber(1, m, md, ml);
	ShowSubNumber(2, o, od, ol);
	return(e->flag);
}

int
ShowDiff4(TestEnv *e, int r1, int r2, char *n, int nd, int nl, char *m, int md, int ml, char *o, int od, int ol, char *p, int pd, int pl)
{
	ErrorPrint(e);
	if(r1 != r2)
		printf("---- Result is %d and must be %d----\n", r1, r2);
	ShowOutRange(0, n, nd, nl);
	ShowOutRange(1, m, md, ml);
	ShowOutRange(2, o, od, ol);
	ShowOutRange(3, p, pd, pl);
	ShowSubNumber(0, n, nd, nl);
	ShowSubNumber(1, m, md, ml);
	ShowSubNumber(2, o, od, ol);
	ShowSubNumber(3, p, pd, pl);
	return(e->flag);
}

int	genlengthvec[] = {9, 8, 1, 0, 2000, 32000,};
BigNumType gentypevec[] = {0, 1, 2, 3, 4, 5,};

int
Generique(TestEnv *e)
{
	int	i;
	int	length, length2;
	BigNumType type, type2;
	BigNum n;

	
   for(i=0; i < 6; i++) {
	type = gentypevec[i];
	length = genlengthvec[i];
	n = BnCreate(type, length);
	if((type2 = BnGetType(n)) != type) {
		sprintf(e->hist,"BnGetType(BnCreate(%u, %d));", type, length);
		if(ShowDiff0(e, type, type2)) return(BN_TRUE);
	}
	if((length2 = BnGetSize(n)) != length) {
		sprintf(e->hist,"BnGetSize(BnCreate(%u, %d));", type, length);
		if(ShowDiff0(e, length, length2)) return(BN_TRUE);
	}
	if(BnFree(n) == 0) {
		sprintf(e->hist, "BnFree(BnCreate(%u, %d));", type, length);
		if(ShowDiff0(e, 1, 0)) return(BN_TRUE);
	}
	BnSetType((n = BnAlloc(length)), type);
	if((type2 = BnGetType(n)) != type) {
		sprintf(e->hist,"BnGetType(BnAlloc(%u, %d));", type, length);
		if(ShowDiff0(e, type, type2)) return(BN_TRUE);
	}
	if((length2 = BnGetSize(n)) != length) {
		sprintf(e->hist,"BnGetSize(BnAlloc(%u, %d));", type, length);
		if(ShowDiff0(e, length, length2)) return(BN_TRUE);
	}
	if(BnFree(n) == 0) {
		sprintf(e->hist, "BnFree(BnAlloc(%u, %d));", type, length);
		if(ShowDiff0(e, 1, 0)) return(BN_TRUE);
	}
   }
   return( BN_FALSE );
}

/*
 *	BnSetToZero
 */
void
___BnSetToZero___(BigNum n, int nd, int nl)
{
	int	i;
	for(i=0; i<nl; i++)
		BnSetDigit(n, nd + i, 0);
}

int
TestBnSetToZero(TestEnv *e)
{
	int	nd;
	int	nl;

	e->depend = "()";
	for(nd = 0; nd <= TESTLENGTH; nd++)
	   for(nl = 0; nl <= TESTLENGTH - nd; nl++) {
		TestCount++;
		ResetTest(0);
		   BnSetToZero   (RN(0), nd, nl);
		___BnSetToZero___(SN(0), nd, nl);
		if(Check(1)) {
			sprintf(e->hist, "%s(n, %d, %d)", e->name, nd, nl);
			if(ShowDiff1(e, 0, 0, "n", nd, nl)) return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnAssign
 */
void
___BnAssign___(BigNum m, int md, BigNum n, int nd, int nl)
{
	BnSetToZero(NtmpBig, 0, nl);
	BnAdd(NtmpBig, 0, nl, n, nd, nl, 0);
	BnSetToZero(m, md, nl);
	BnAdd(m, md, nl, NtmpBig, 0, nl, 0);
}

int
TestBnAssign(TestEnv *e)
{
	int	md, nd, nl;

	e->depend = "(BnSetToZero, BnAdd)";
	for(md = 0; md <= TESTLENGTH; md++)
	  for(nd = 0; nd <= TESTLENGTH; nd++)
	    for(nl=0; ((nl<=TESTLENGTH-nd) && (nl<=TESTLENGTH-md)); nl++) {
		TestCount++;
		ResetTest(0);
		   BnAssign   (RN(0), md, RN(0), nd, nl);
		___BnAssign___(SN(0), md, SN(0), nd, nl);
		if(Check(1)) {
			sprintf(e->hist, "%s(n, %d, n, %d, %d)", e->name,
						md, nd, nl);
			if(ShowDiff1(e, 0, 0, "n", md, nl)) return(1);
	}	}
	return(BN_FALSE);
}


/*
 *	BnNumDigits
 */
int
___BnNumDigits___(BigNum n, int nd, int nl)
{

	while(nl != 0) {
		nl--;
		if(!BnIsDigitZero(n, nd + nl)) break;
	}
	return(nl + 1);
}

int
TestBnNumDigits(TestEnv *e)
{
	int	nd0, nl0, nd, nl, l1, l2;

	e->depend = "(BnIsDigitZero)";
	for(nd0 = 0; nd0 <= TESTLENGTH; nd0++)
	  for(nl0 = 0; nl0 <= TESTLENGTH - nd0; nl0++)
	    for(nd = 0; nd <= TESTLENGTH; nd++)
	      for(nl = 0; nl <= TESTLENGTH - nd; nl++) {
		TestCount++;
		ResetTest(0);
		BnSetToZero(RN(0), nd0, nl0);
		BnSetToZero(SN(0), nd0, nl0);
		l1 =    BnNumDigits   (RN(0), nd, nl);
		l2 = ___BnNumDigits___(SN(0), nd, nl);
		if(Check(1) || l1 != l2) {
			sprintf(e->hist, "%s(n, %d, %d)", e->name, nd, nl);
			if(ShowDiff1(e, l1, l2, "n", nd, nl)) return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnNumLeadingZeroBitsInDigit
 */
int
___BnNumLeadingZeroBitsInDigit___(BigNum n, int nd)
{
	int	p = 0;

	if(BnIsDigitZero(n, nd)) return(BN_DIGIT_SIZE);
	BnAssign(Ntmp2, 0, n, nd, 1);
	BnShiftLeft(Ntmp2, 0, 1, Ntmp2, 1, 1);
	while(BnIsDigitZero(Ntmp2, 1)) {
		BnShiftLeft(Ntmp2, 0, 1, Ntmp2, 1, 1);
		p++;
	}
	return(p);
}

int
TestBnNumLeadingZeroBitsInDigit(TestEnv *e)
{
	int	nd;
	int	l1;
	int	l2;

	e->depend = "(BnShiftLeft, BnIsDigitZero)";
	ResetTest(0);
	for(nd = 0; nd < TESTLENGTH; nd++) {
		TestCount++;
		l1 =    BnNumLeadingZeroBitsInDigit   (RN(0), nd);
		l2 = ___BnNumLeadingZeroBitsInDigit___(SN(0), nd);
		if(Check(1) || l1 != l2) {
			sprintf(e->hist, "%s(n, %d)", e->name, nd);
			if(ShowDiff1(e, l1, l2, "n", nd, 1)) return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnIsDigitZero
 */
int
___BnIsDigitZero___(BigNum n, int nd)
{
	if(BnGetDigit(n, nd) == 0) return(1);
	return(0);
}

int
TestBnIsDigitZero(TestEnv *e)
{
	int	nd; int	l1, l2;

	e->depend = "()";
	ResetTest(0);
	for(nd = 0; nd < TESTLENGTH; nd++) {
		TestCount++;
		l1 =    BnIsDigitZero   (RN(0), nd);
		l2 = ___BnIsDigitZero___(SN(0), nd);
		if(Check(1) || ((l1 == 0) && (l2 != 0)) ||
			       ((l1 != 0) && (l2 == 0))) {
			sprintf(e->hist, "%s(n, %d)", e->name, nd);
			if(ShowDiff1(e, l1, l2, "n", nd, 1)) return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnIsDigitNormalized
 */
int
___BnIsDigitNormalized___(BigNum n, int nd)
{
	BnAssign(Ntmp2, 0, n, nd, 1);
	BnShiftLeft(Ntmp2, 0, 1, Ntmp2, 1, 1);
	if(BnIsDigitZero(Ntmp2, 1)) return(0);
	return(1);
}

int
TestBnIsDigitNormalized(TestEnv *e)
{
	int	nd; int	l1, l2;

	e->depend = "(BnShiftLeft, BnIsDigitZero)";
	ResetTest(0);
	for(nd = 0; nd < TESTLENGTH; nd++) {
		TestCount++;
		l1 =    BnIsDigitNormalized   (RN(0), nd);
		l2 = ___BnIsDigitNormalized___(SN(0), nd);
		if(Check(1) || ((l1 == 0) && (l2 != 0)) ||
			       ((l1 != 0) && (l2 == 0))) {
			sprintf(e->hist, "%s(n, %d)", e->name, nd);
			if(ShowDiff1(e, l1, l2, "n", nd, 1)) return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnIsDigitOdd
 */
int
___BnIsDigitOdd___(BigNum n, int nd)
{
	BnAssign(Ntmp2, 0, n, nd, 1);
	BnShiftRight(Ntmp2, 0, 1, Ntmp2, 1, 1);
	if(BnIsDigitZero(Ntmp2, 1)) return(0);
	return(1);
}

int
TestBnIsDigitOdd(TestEnv *e)
{
	int	nd; int	l1, l2;

	e->depend = "(BnShiftRight, BnIsDigitZero)";
	ResetTest(0);
	for(nd = 0; nd < TESTLENGTH; nd++) {
		TestCount++;
		l1 =    BnIsDigitOdd   (RN(0), nd);
		l2 = ___BnIsDigitOdd___(SN(0), nd);
		if(Check(1) || ((l1 == 0) && (l2 != 0)) ||
			       ((l1 != 0) && (l2 == 0))) {
			sprintf(e->hist, "%s(n, %d)", e->name, nd);
			if(ShowDiff1(e, l1, l2, "n", nd, 1)) return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnCompareDigits
 */
int
___BnCompareDigits___(BigNum n, int nd, BigNum m, int md)
{
	BnAssign(Ntmp2, 0, n, nd, 1);
	BnComplement(Ntmp2, 0, 1);
	if(BnAdd(Ntmp2, 0, 1, m, md, 1, 0)) return(-1);
	BnComplement(Ntmp2, 0, 1);
	if(BnIsDigitZero(Ntmp2, 0)) return(0);
	return(1);
}

int
TestBnCompareDigits(TestEnv *e)
{
	int	nd, md; int	l1, l2;

	e->depend = "(BnComplement, BnAdd, BnIsDigitZero)";
	ResetTest(0);
	ResetTest(1);
	for(nd = 0; nd < TESTLENGTH; nd++)
	   for(md = 0; md < TESTLENGTH; md++) {
		TestCount++;
		l1 =    BnCompareDigits   (RN(0), nd, RN(1), md);
		l2 = ___BnCompareDigits___(SN(0), nd, SN(1), md);
		if(Check(2) || l1 != l2) {
			sprintf(e->hist, "%s(n, %d, m, %d)", e->name, nd, md);
			if(ShowDiff2(e, l1, l2, "n", nd, 1, "m", md, 1))
				return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnComplement
 */
void
___BnComplement___(BigNum n, int nd, int nl)
{
	int	i;

	BnSetDigit(Ntmp2, 0, 0);
	BnSubtractBorrow(Ntmp2, 0, 1, 0);
	for(i = 0; i < nl; i++)
		BnXorDigits(n, nd + i, Ntmp2, 0);
}

int
TestBnComplement(TestEnv *e)
{
	int	nd, nl;

	e->depend = "(BnSubtractBorrow, BnXorDigits)";
	for(nd = 0; nd <= TESTLENGTH; nd++)
	   for(nl = 0; nl <= TESTLENGTH - nd; nl++) {
		TestCount++;
		ResetTest(0);
		   BnComplement   (RN(0), nd, nl);
		___BnComplement___(SN(0), nd, nl);
		if(Check(1)) {
			sprintf(e->hist, "%s(n, %d, %d)", e->name, nd, nl);
			if(ShowDiff1(e, 0, 0, "n", nd, nl)) return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnAndDigits
 */
void
___BnAndDigits___(BigNum n, int nd, BigNum m, int md)
{
	BnAssign(Ntmp2, 0, n, nd, 1);
	BnOrDigits(Ntmp2, 0, m, md);
	BnXorDigits(Ntmp2, 0, m, md);
	BnXorDigits(n, nd, Ntmp2, 0);
}

int
TestBnAndDigits(TestEnv *e)
{
	int	nd, md;

	e->depend = "(BnOrDigits, BnXorDigits)";
	ResetTest(1);
	for(nd = 0; nd < TESTLENGTH; nd++)
	   for(md = 0; md < TESTLENGTH; md++) {
		TestCount++;
		ResetTest(0);
		   BnAndDigits   (RN(0), nd, RN(1), md);
		___BnAndDigits___(SN(0), nd, SN(1), md);
		if(Check(2)) {
			sprintf(e->hist, "%s(n, %d, m, %d)", e->name, nd, md);
			if(ShowDiff2(e, 0, 0, "n", nd, 1, "m", md, 1))
				return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnOrDigits
 */
void
___BnOrDigits___(BigNum n, int nd, BigNum m, int md)
{
	BnAssign(Ntmp2, 0, n, nd, 1);
	BnAndDigits(Ntmp2, 0, m, md);
	BnXorDigits(Ntmp2, 0, m, md);
	BnXorDigits(n, nd, Ntmp2, 0);
}

int
TestBnOrDigits(TestEnv *e)
{
	int	nd, md;

	e->depend = "(BnAndDigits, BnXorDigits)";
	ResetTest(1);
	for(nd = 0; nd < TESTLENGTH; nd++)
	   for(md = 0; md < TESTLENGTH; md++) {
		TestCount++;
		ResetTest(0);
		   BnOrDigits   (RN(0), nd, RN(1), md);
		___BnOrDigits___(SN(0), nd, SN(1), md);
		if(Check(2)) {
			sprintf(e->hist, "%s(n, %d, m, %d)", e->name, nd, md);
			if(ShowDiff2(e, 0, 0, "n", nd, 1, "m", md, 1))
				return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnXorDigits
 */
void
___BnXorDigits___(BigNum n, int nd, BigNum m, int md)
{
	BnAssign(Ntmp2, 0, n, nd, 1);
	BnAndDigits(Ntmp2, 0, m, md);
	BnComplement(Ntmp2, 0, 1);
	BnOrDigits(n, nd, m, md);
	BnAndDigits(n, nd, Ntmp2, 0);
}

int
TestBnXorDigits(TestEnv *e)
{
	int	nd, md;

	e->depend = "(BnAndDigits, BnComplement, BnOrDigits)";
	ResetTest(1);
	for(nd = 0; nd < TESTLENGTH; nd++)
	   for(md = 0; md < TESTLENGTH; md++) {
		TestCount++;
		ResetTest(0);
		   BnXorDigits   (RN(0), nd, RN(1), md);
		___BnXorDigits___(SN(0), nd, SN(1), md);
		if(Check(2)) {
			sprintf(e->hist, "%s(n, %d, m, %d)", e->name, nd, md);
			if(ShowDiff2(e, 0, 0, "n", nd, 1, "m", md, 1))
				return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnShiftLeft
 */
void
___BnShiftLeft___(BigNum n, int nd, int nl, BigNum m, int md, int s)

{
	BnSetDigit(m, md, 2);
	BnSetDigit(Ntmp2, 0, 1);
	while(s--) {
		BnSetToZero(NtmpBig, 0, 2);
		BnMultiplyDigit(NtmpBig, 0, 2, Ntmp2, 0, 1, m, md);
		BnAssign(Ntmp2, 0, NtmpBig, 0, 1);
	}
	BnSetToZero(NtmpBig, 0, nl + 1);
	BnMultiplyDigit(NtmpBig, 0, nl + 1, n, nd, nl, Ntmp2, 0);
	BnAssign(n, nd, NtmpBig, 0, nl);
	BnAssign(m, md, NtmpBig, nl, 1);
}

int
TestBnShiftLeft(TestEnv *e)
{
	int	nd, nl, md; int	s;

	e->depend = "(BnSetToZero, BnMultiplyDigit)";
	ResetTest(1);
	for(nd = 0; nd <= TESTLENGTH; nd++)
	  for(nl = 0; nl <= TESTLENGTH - nd; nl++)
	    for(md = 0; md < 2; md++)
	      for(s = 0; s < (int)BN_DIGIT_SIZE; s++) {
		TestCount++;
		ResetTest(0);
		   BnShiftLeft   (RN(0), nd, nl, RN(1), md, s);
		___BnShiftLeft___(SN(0), nd, nl, SN(1), md, s);
		if(Check(2)) {
			sprintf(e->hist, "%s(n, %d, %d, m, %d, %d)",
					e->name, nd, nl, md, s);
			if(ShowDiff2(e, 0, 0, "n", nd, nl, "m", md, 1))
				return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnShiftRight
 */
void
___BnShiftRight___(BigNum n, int nd, int nl, BigNum m, int md, int s)
{
	if((nl == 0) || (s == 0)) {
		BnSetDigit(m, md, 0);
		return;
	}
	BnAssign(NtmpBig, 0, n, nd, nl);
	BnShiftLeft(NtmpBig, 0, nl, NtmpBig, nl, BN_DIGIT_SIZE - s);
	BnAssign(n, nd, NtmpBig, 1, nl);
	BnAssign(m, md, NtmpBig, 0, 1);
}

int
TestBnShiftRight(TestEnv *e)
{
	int	nd, nl, md; int	s;

	e->depend = "(BnShiftLeft)";
	ResetTest(1);
	for(nd = 0; nd <= TESTLENGTH; nd++)
	  for(nl = 0; nl <= TESTLENGTH - nd; nl++)
	    for(md = 0; md < 2; md++)
	      for(s = 0; s < (int)BN_DIGIT_SIZE; s++) {
		TestCount++;
		ResetTest(0);
		   BnShiftRight   (RN(0), nd, nl, RN(1), md, s);
		___BnShiftRight___(SN(0), nd, nl, SN(1), md, s);
		if(Check(2)) {
			sprintf(e->hist, "%s(n, %d, %d, m, %d, %d)",
					e->name, nd, nl, md, s);
			if(ShowDiff2(e, 0, 0, "n", nd, nl, "m", md, 1))
				return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnAddCarry
 */
int
___BnAddCarry___(BigNum n, int nd, int nl, int r)
{
	if(r == 0) return(0);
	BnComplement(n, nd, nl);
	r = BnSubtractBorrow(n, nd, nl, 0);
	BnComplement(n, nd, nl);
	if(r == 0) return(1);
	return(0);
}

int
TestBnAddCarry(TestEnv *e)
{
	int	nd, nl; int	r, l1, l2;

	e->depend = "(BnComplement, BnSubtractBorrow)";
	for(nd = 0; nd <= TESTLENGTH; nd++)
	  for(nl = 0; nl <= TESTLENGTH - nd; nl++)
	    for(r = 0; r < 2; r++) {
		TestCount++;
		ResetTest(0);
		l1 =    BnAddCarry   (RN(0), nd, nl, r);
		l2 = ___BnAddCarry___(SN(0), nd, nl, r);
		if(Check(1) || l1 != l2) {
			sprintf(e->hist, "%s(n, %d, %d, %d)",
					e->name, nd, nl, r);
			if(ShowDiff1(e, l1, l2, "n", nd, nl)) return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnAdd
 */
int
___BnAdd___(BigNum n, int nd, int nl, BigNum m, int md, int ml, int r)
{
	BnComplement(m, md, ml);
	r = BnSubtract(n, nd, ml, m, md, ml, r);
	BnComplement(m, md, ml);
	return(BnAddCarry(n, nd + ml, nl - ml, r));
}

int
TestBnAdd(TestEnv *e)
{
	int	nd, nl, md, ml; int	r, l1, l2;

	e->depend = "(BnComplement, BnSubtract, BnAddCarry)";
	ResetTest(1);
	for(nd = 0; nd <= TESTLENGTH; nd++)
	 for(nl = 0; nl <= TESTLENGTH - nd; nl++)
	  for(md = 0; md <= TESTLENGTH - nl; md++)
	   for(ml = 0; ml <= nl ; ml++)
	    for(r = 0; r < 2; r++) {
		TestCount++;
		ResetTest(0);
		l1 =    BnAdd   (RN(0), nd, nl, RN(1), md, ml, r);
		l2 = ___BnAdd___(SN(0), nd, nl, SN(1), md, ml, r);
		if(Check(2) || l1 != l2) {
			sprintf(e->hist, "%s(n, %d, %d, m, %d, %d, %d)",
					e->name, nd, nl, md, ml, r);
			if(ShowDiff2(e, l1, l2, "n", nd, nl, "m", md, ml))
				return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnSubtractBorrow
 */
int
___BnSubtractBorrow___(BigNum n, int nd, int nl, int r)
{
	if(r == 1) return(1);
	BnComplement(n, nd, nl);
	r = BnAddCarry(n, nd, nl, 1);
	BnComplement(n, nd, nl);
	if(r == 0) return(1);
	return(0);
}

int
TestBnSubtractBorrow(TestEnv *e)
{
	int	nd, nl; int	r, l1, l2;

	e->depend = "(BnComplement, BnAddCarry)";
	for(nd = 0; nd <= TESTLENGTH; nd++)
	  for(nl = 0; nl <= TESTLENGTH - nd; nl++)
	    for(r = 0; r < 2; r++) {
		TestCount++;
		ResetTest(0);
		l1 =    BnSubtractBorrow   (RN(0), nd, nl, r);
		l2 = ___BnSubtractBorrow___(SN(0), nd, nl, r);
		if(Check(1) || l1 != l2) {
			sprintf(e->hist, "%s(n, %d, %d, %d)",
					e->name, nd, nl, r);
			if(ShowDiff1(e, l1, l2, "n", nd, nl)) return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnSubtract
 */
int
___BnSubtract___(BigNum n, int nd, int nl, BigNum m, int md, int ml, int r)
{
	BnComplement(m, md, ml);
	r = BnAdd(n, nd, ml, m, md, ml, r);
	BnComplement(m, md, ml);
	return(BnSubtractBorrow(n, nd + ml, nl - ml, r));
}

int
TestBnSubtract(TestEnv *e)
{
	int	nd, nl, md, ml; int	r, l1, l2;

	e->depend = "(BnComplement, BnAdd, BnSubtractBorrow)";
	ResetTest(1);
	for(nd = 0; nd <= TESTLENGTH; nd++)
	 for(nl = 0; nl <= TESTLENGTH - nd; nl++)
	  for(md = 0; md <= TESTLENGTH - nl; md++)
	   for(ml = 0; ml <= nl ; ml++)
	    for(r = 0; r < 2; r++) {	
		TestCount++;
		ResetTest(0);
		l1 =    BnSubtract   (RN(0), nd, nl, RN(1), md, ml, r);
		l2 = ___BnSubtract___(SN(0), nd, nl, SN(1), md, ml, r);
		if(Check(2) || l1 != l2) {
			sprintf(e->hist, "%s(n, %d, %d, m, %d, %d, %d)",
					e->name, nd, nl, md, ml, r);
			if(ShowDiff2(e, l1, l2, "n", nd, nl, "m", md, ml))
				return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnMultiplyDigit
 */
int
___BnMultiplyDigit___(BigNum p, int pd, int pl, BigNum n, int nd, int nl, BigNum m, int md)
{
	int	r = 0, ret = 0;

	BnAssign(Ntmp2, 0, m, md, 1);
	BnAssign(NtmpBig, 0, n, nd, nl);
	BnSetToZero(NtmpBig, nl, 1);
	while(!BnIsDigitZero(Ntmp2, 0)) {
		if(BnIsDigitOdd(Ntmp2, 0)) {
			r = BnAdd(p, pd, pl, NtmpBig, 0, nl + 1, 0);
			if((ret == 0) && (r == 1)) ret = 1;
			else if((ret == 1) && (r == 1))	ret = 2;
		}
		BnShiftRight(Ntmp2, 0, 1, Ntmp2, 1, 1);
		BnShiftLeft(NtmpBig, 0, nl + 1, Ntmp2, 1, 1);
		if(!BnIsDigitZero(Ntmp2, 1)) ret = 3;
	}
	return(ret);
}

int
TestBnMultiplyDigit(TestEnv *e)
{
	int	pd;
	int	pl;
	int	nd;
	int	nl;
	int	md;
	int	l1;
	int	l2;

	e->depend = "(BnSetToZero, BnIsDigitZero, BnIsDigitOdd, BnAdd, BnShiftRight, BnShiftLeft)";
	ResetTest(1);
	ResetTest(2);
	for(pd = 0; pd <= TESTLENGTH; pd++)
	 for(pl = 0; pl <= TESTLENGTH - pd; pl++)
	  for(nd = 0; nd <= TESTLENGTH - pl; nd++)
	   for(nl = 0; nl < pl ; nl++)
	    for(md = 0; md < TESTLENGTH; md++) {
		TestCount++;
		ResetTest(0);
		l1 =    BnMultiplyDigit   (RN(0),pd,pl,RN(1),nd,nl,RN(2),md);
		l2 = ___BnMultiplyDigit___(SN(0),pd,pl,SN(1),nd,nl,SN(2),md);
		if(Check(3) || l1 != l2) {
			sprintf(e->hist,
			   "BnMultiplyDigit(p, %d, %d, n, %d, %d, m, %d)",
						pd, pl, nd, nl, md);
			if(ShowDiff3(e,l1,l2,"p",pd,pl,"n",nd,nl,"m",md,1))
				 return(1);
	}	}
	return(BN_FALSE);
}

/*
 *	BnDivideDigit
 */
int
TestBnDivideDigit(TestEnv *e)
{
	int	nd, nl, md, qd, rd, l2;

	e->depend = "(BnSetToZero, BnMultiplyDigit, BnCompareDigits)";
	ResetTest(2);
	ResetTest(3);
	for(nd = 0; nd <= TESTLENGTH - 2; nd++)
	 for(nl = 2; nl <= TESTLENGTH - nd; nl++)
	  for(md = 0; md < TESTLENGTH; md++)
	   for(qd = 0; qd < TESTLENGTH - nl + 1 ; qd++)
	    for(rd = 0; rd < 2; rd++)
	     if((!BnIsDigitZero(RN(3), md)) &&
			(BnCompareDigits(RN(2), nd+nl-1, RN(3), md) == -1)) {
		TestCount++;
		ResetTest(0);
		ResetTest(1);
		BnDivideDigit(RN(0), qd, RN(1), rd, RN(2), nd, nl, RN(3), md);
		BnAssign(SN(0), qd, RN(0), qd, nl - 1);
		BnAssign(SN(1), rd, RN(1), rd, 1);
		BnSetToZero(SN(2), nd, nl);
		BnAssign(SN(2), nd, SN(1), rd, 1);
		l2 = BnMultiplyDigit(SN(2),nd,nl, SN(0),qd,nl - 1, SN(3), md);
		if(Check(4) || l2 != 0) {
			sprintf(e->hist,
			   "BnDivideDigit(q, %d, r, %d, n, %d, %d, m, %d)",
						qd, rd, nd, nl, md);
			if(ShowDiff4(e, 0, l2, "q", qd, nl - 1, "r", rd, 1,
					"n", nd, nl, "m", md, 1))
				return(BN_TRUE);
	}	}
	return(BN_FALSE);
}

/*
 *	Main
 */

TESTONE AllTest[] = {
	{ Generique,				"fonctions generiques" },
	{ TestBnSetToZero,			"BnSetToZero" },
	{ TestBnAssign,				"BnAssign" },
	{ TestBnNumDigits,			"BnNumDigits" },
	{ TestBnNumLeadingZeroBitsInDigit,	"BnNumLeadingZeroBitsInDigit" },
	{ TestBnIsDigitZero,			"BnIsDigitZero" },
	{ TestBnIsDigitNormalized,		"BnIsDigitNormalized" },
	{ TestBnIsDigitOdd,			"BnIsDigitOdd" },
	{ TestBnCompareDigits,			"BnCompareDigits" },
	{ TestBnComplement,			"BnComplement" },
	{ TestBnAndDigits,			"BnAndDigits" },
	{ TestBnOrDigits,			"BnOrDigits" },
	{ TestBnXorDigits,			"BnXorDigits" },
	{ TestBnShiftLeft,			"BnShiftLeft" },
	{ TestBnShiftRight,			"BnShiftRight" },
	{ TestBnAddCarry,			"BnAddCarry" },
	{ TestBnAdd,				"BnAdd" },
	{ TestBnSubtractBorrow,			"BnSubtractBorrow" },
	{ TestBnSubtract,			"BnSubtract" },
	{ TestBnMultiplyDigit,			"BnMultiplyDigit" },
	{ TestBnDivideDigit,			"BnDivideDigit" }
};

int
main(int argc, char **argv)
{
	TestEnv realenv, *e = &realenv;
	int	i, j, nbtest, SizeAllTest;

	int digit_size = (int)sizeof(BigNumDigit);
	int word_size  = (int)sizeof(int *);

	if( digit_size > word_size ) {
		fprintf(stderr,
			"BigNumDigit size (%d) > sizeof(void *) (%d)\n",
			digit_size, word_size);
		fprintf(stderr,
			"Verify BnnDoesDigitFitInWord and BnnDivideDigit\n");
		exit( 0 );
	}

	printf("sizeof(BigNumDigit) == %d\n", (int)sizeof(BigNumDigit));

	/*
	 * Initialization for test env.
	 */

	e->flag   = 1;
	e->depend = "()";

	/*
	 * Allocation for 2 global numbers.
	 */

	Ntmp2	  = BnAlloc(2);
	NtmpBig	  = BnAlloc(2 * TESTLENGTH);
	NumbProto = BnAlloc(TESTLENGTH);

	/*
	 * Creation of prototype number.
	 */

	BnSetDigit(NumbProto, 0, 0);		/* First 2 -> 0 */
	BnSetDigit(NumbProto, 1, 0);

	for( i=0 ; i < TESTLENGTH/4 - 1 ; i++ ) {
		/* 1, 2, 3, ... */
		BnSetDigit(NumbProto, (BigNumDigit)(i+2), (BigNumDigit)(i+1));
	}

	/*
	 * The 2nd quarter is the 1st shifted by BN_DIGIT_SIZE - 2.
	 * 0x4000 0x8000 ...
	 */

	BnAssign(NumbProto, QTL + 1, NumbProto, 2, QTL - 1);
	BnShiftLeft(NumbProto, QTL + 1, QTL - 1, NumbProto, 0, BN_DIGIT_SIZE - 2);

	/*
	 * The 2nd half is the bit inverse of the the 1st one.
	 */

	BnAssign(NumbProto, DTL, NumbProto, 0, DTL);
	BnComplement(NumbProto, DTL, DTL);

	/*
	 * Allocation numbers.
	 */

	for(i=0; i < 5; i++) {
		RN(i) = BnAlloc(TESTLENGTH);
		SN(i) = BnAlloc(TESTLENGTH);
	}

	if(argc == 1) {
		printf("%s [v] [a] [NOTEST]\n", argv[0]);
	}

	/*
	 * Now going ...
	 */

	SizeAllTest = (sizeof(AllTest)/sizeof(AllTest[0]));
	for (i = 1; i < argc ; i++) {
		if(argv[i][0] == 'm') {
			/* 0 = No skip; 1 = skip to next; else STOP */
			e->flag = atoi(&argv[i][1]);
		} else if (argv[i][0] == 'a') {
			for (i = 0; i < SizeAllTest; i++)
				dotest(e, i, 1);
		} else if (argv[i][0] == 'A') {
      /* do all tests 10 times to compute more accurate time */
			for (i = 0; i < SizeAllTest; i++)
				dotest(e, i, 10);
		} else if (argv[i][0] == 'v') {
			for (j = 0; j < SizeAllTest; j++)
				seetest(j);
		} else {
			nbtest = atoi(argv[i]);
			if ((nbtest < 0) || (nbtest >= SizeAllTest))
				printf("test no %d is invalid\n", nbtest);
			else	dotest(e, nbtest, 1);
		}
	}
	return( 0 );
}
