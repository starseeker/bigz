#if	!defined( lint )
static	const char rcsid[] = "$Id: tCBignum.cpp,v 1.8 2011-12-27 15:11:17 jullien Exp $";
#endif

//
//	tCBignum.cpp :	
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include "CBignum.h"
#include "CRational.h"

static int testcnt = 0;
static int failcnt = 0;

CBignum
fib( const CBignum& n )
{
	if( n == BzOne ) {
		return BzOne;
	} else	if( n == BzTwo ) {
		return BzOne;
	} else	{
		return( fib( n - BzOne ) + fib( n - BzTwo ) );
	}
}

CBignum
square( const CBignum& bn )
{
	return bn * bn;
}

CBignum
ffib( const CBignum& n )
{
	if( n == BzOne ) {
		return BzOne;
	}

	if( n == BzTwo ) {
		return BzOne;
	}

	CBignum ndiv2( n / BzTwo );

	if( oddp( n ) ) {
	  return square(ffib(ndiv2))         + square(ffib(ndiv2 + BzOne));
	} else {
	  return square(ffib(ndiv2 + BzOne)) - square(ffib(ndiv2 - BzOne));
	}
}

void
Tz( int count, const char* op, const CBignum& n, const char * res )
{
	const char* s = (const char *)n;

	++testcnt;

	if( strcmp( s, res ) != 0 ) {
	    printf( "test %3d (%s) fails: expected = %16s, computed = %16s\n",
	            count, op, res, s );
	    ++failcnt;
	}

	BzFreeString( (char *)s );
}

void
Tq( int count, const char* op, const CRational& n, const char *res )
{
	const char* s = (const char *)n;

	++testcnt;

	if( strcmp( s, res ) != 0 ) {
	    printf( "test %3d (%s) fails: expected = %16s, computed = %16s\n",
	            count, op, res, s );
	    ++failcnt;
	}

	BzFreeString( (char *)s );
}

const char* X1;
const char* X2;
const char* Y1;
const char* Y2;

#include "tCBignum.dat"

int
main()
{
	(void)printf("Bignum non-regression tests. (c) 1998-2012 C. Jullien\n");
	(void)printf("Testing version %s ...\n\n", CBignum::version());

	CBignum x1( ffib( 100 ) );	/* 354224848179261915075	*/
	CBignum x2( 2 );		/* 2		     		*/
	CBignum	x3;			/* 0				*/

	Tz(   1, "++",   ++x2, 		"3"				);
	Tz(   2, "--",	 --x1, 		"354224848179261915074"		);
	Tz(   3, "*=",	 x1 *= x2,	"1062674544537785745222"	);
	Tz(   4, "-=",	 x1 -= x2,	"1062674544537785745219"	);
	Tz(   5, "+=",	 x1 += 3,	"1062674544537785745222"	);
	Tz(   6, "ctor", x3,		"0"				);
	Tz(   7, "=",	 x3 = x1,	"1062674544537785745222"	);
	Tz(   8, "/=",	 x3 /= x2+2,	"212534908907557149044"		);
	Tz(   9, "%=",	 x3 %= 19,	"13"				);
	Tz(  10, "exp",	 x2 = x3*3+x1/2, "531337272268892872650"	);

	tests();

	if( failcnt != 0 )
		(void)printf( "%d tests made, fails %d.\n", testcnt, failcnt );
	else	(void)printf( "%d tests made, Ok!.\n", testcnt );

	return( 0 );
}
