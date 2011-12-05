/*
 static	const char rcsid[] = "$Id: bigz.c,v 1.58 2011-12-05 07:25:40 jullien Exp $";
*/

/*
 *	bigz.c : provides an implementation of "unlimited-precision"
 *	         arithmetic for signed integers.
 */

/*
 * Copyright:
 *  - Digital Equipment Corporation & INRIA 1988, 1989
 *  - Eligis 1997 - 2012
 *
 * Several conventions are used in the commentary:
 *    A "BigZ" is the name for an arbitrary-precision signed integer.
 *    Capital letters (e.g., "Z") are used to refer to the value of BigZs.
 */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#if	!defined( __BIGZ_H )
#include "bigz.h"
#endif

#define MaxInt(a,b) 		(((a) < (b)) ? (b) : (a))
#define AbsInt(x)		(((x) >= 0) ? (x) : -(x))

#define BZMAXINT                ((BzInt)(((BzUInt)-1L) >> 1))

#if	!defined( OLEBCDIC )
/*
 * ASCII character class table
 */
static int BigHexToDigit[] = {
	 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
	 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
	 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
	 0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  0,  0,  0,  0,  0, 
	 0, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 
	25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,  0,  0,  0,  0,  0, 
	 0, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 
	25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,  0,  0,  0,  0,  0  
};

#define	CTOI(c)	 ((((unsigned int)c)<(unsigned int)127) \
		  ? BigHexToDigit[(unsigned int)c] \
		  : 0)

#else
/*
 * EBCDIC character class table
 */
static int BigHexToDigit[] = {
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0, 10, 11, 12, 13, 14, 15, 16, 17, 18,  0,  0,  0,  0,  0,  0,
         0, 19, 20, 21, 22, 23, 24, 25, 26, 27,  0,  0,  0,  0,  0,  0,
         0,  0, 28, 29, 30, 31, 32, 33, 34, 35,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0, 10, 11, 12, 13, 14, 15, 16, 17, 18,  0,  0,  0,  0,  0,  0,
         0, 19, 20, 21, 22, 23, 24, 25, 26, 27,  0,  0,  0,  0,  0,  0,
         0,  0, 28, 29, 30, 31, 32, 33, 34, 35,  0,  0,  0,  0,  0,  0,
         0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  0,  0,  0,  0,  0
};

#define	CTOI(c)	 ((((unsigned int)c)<(unsigned int)255) \
		  ? BigHexToDigit[(unsigned int)c] \
		  : 0)
#endif

static	int	BzStrLen( const BzChar *s ) BN_PURE_FUNCTION;

#if	defined( BZ_DEBUG )
static void	BzShowBits( BigNumDigit n );

static void
BzShowBits( BigNumDigit n )
{
	int	i;

	for( i = (int)(BN_DIGIT_SIZE - 1) ; i >= 0 ; i-- ) {
		(void)printf( "%d", (n & (1 << (unsigned int)i)) != 0 ? 1 : 0 );
	}
}

void	BnDebug(const BzChar *m,
		const BzChar *bzstr, BigNum n, BigNumLength nl, BzSign sign);

void
BnDebug(const BzChar *m,
	const BzChar *bzstr,
	BigNum	     n,
	BigNumLength nl,
	BzSign	     sign)
{
	BigNum	     p;
	BigNumLength l;
	BzChar	     c;
	char	     fmt[8];

	if( m != (BzChar *)NULL ) {
		(void)printf( "%-20s\n", m );
	}

	(void)printf( "\t: BigZ = %s at 0x%p, ", bzstr, n );
	(void)printf( "digit = %d, word = %d\n",
		      (int)BN_DIGIT_SIZE,
		      (int)BN_WORD_SIZE );
	(void)printf( "\t: <-- high %02d digit(s) low -->\n", (int)nl );

	if( sign == BZ_ZERO ) {
		c = (BzChar)'0';
	} else	if( sign == BZ_MINUS ) {
		c = (BzChar)'-';
	} else	{
		c = (BzChar)'+';
	}

	(void)printf( "      %c : ", c );
	p = n + nl - 1;
	l = nl;
	/*
	 * compute format output depending on BigNumDigit size.
	 * It requires 2 hexa digits by byte.
	 */
	sprintf(fmt, "|%%0%dx", (int)(2 * sizeof( BigNumDigit )));

	while( l-- != 0 ) {
		(void)printf( fmt, *p-- );
	}
	(void)printf( "|\n" );

	(void)printf( "      %c : ", c );
	p = n + nl - 1;
	l = nl;
	while( l-- != 0 ) {
		(void)printf( "|" );
		BzShowBits( *p-- );
	}
	(void)printf( "|\n" );
}

void
BzDebug( const BzChar *m, const BigZ y )
{
	BzChar * s = BzToString( y, (BigNumDigit)10, 0 );

	BnDebug( m, s, BzToBn( y ), BzNumDigits( y ), BzGetSign( y ) );
	BzFreeString( s );
}
#endif

const char *
BzVersion( void )
{
	return( BZ_VERSION );
}

/*
 *	constants used by BzToString() and BzFromString()
 */

#define	BZ_MIN_BASE	2
#define	BZ_MAX_BASE	36

/*
 *	following table is computed using:
 *
 *	for( i = 1 ; i <= BZ_MAX_BASE ; ++i) {
 *		printf( "\t%16.16f, // log(%2d)\n", log((double)i), i);
 *	}
 */

static double BzLog[] = {
	0.0000000000000000,
        0.0000000000000000, /* log( 1) */
        0.6931471805599453, /* log( 2) */
        1.0986122886681098, /* log( 3) */
        1.3862943611198906, /* log( 4) */
        1.6094379124341003, /* log( 5) */
        1.7917594692280550, /* log( 6) */
        1.9459101490553132, /* log( 7) */
        2.0794415416798357, /* log( 8) */
        2.1972245773362196, /* log( 9) */
        2.3025850929940459, /* log(10) */
        2.3978952727983707, /* log(11) */
        2.4849066497880004, /* log(12) */
        2.5649493574615367, /* log(13) */
        2.6390573296152584, /* log(14) */
        2.7080502011022101, /* log(15) */
        2.7725887222397811, /* log(16) */
        2.8332133440562162, /* log(17) */
        2.8903717578961645, /* log(18) */
        2.9444389791664403, /* log(19) */
        2.9957322735539909, /* log(20) */
        3.0445224377234230, /* log(21) */
        3.0910424533583161, /* log(22) */
        3.1354942159291497, /* log(23) */
        3.1780538303479458, /* log(24) */
        3.2188758248682006, /* log(25) */
        3.2580965380214821, /* log(26) */
        3.2958368660043291, /* log(27) */
        3.3322045101752038, /* log(28) */
        3.3672958299864741, /* log(29) */
        3.4011973816621555, /* log(30) */
        3.4339872044851463, /* log(31) */
        3.4657359027997265, /* log(32) */
        3.4965075614664802, /* log(33) */
        3.5263605246161616, /* log(34) */
        3.5553480614894135, /* log(35) */
        3.5835189384561099, /* log(36) */
};

BigZ
BzCreate( BigNumLength Size )
{
	/*
	 * Allocates a zeroed BigZ of the desired size.
	 */

	BigZ	z;
	size_t	chunk;

	chunk = sizeof(struct BigZHeader) + Size * sizeof(BigNumDigit);

	if( (z = (BigZ)(BzAlloc( chunk ))) != BZNULL ) {
		/*
		 * reset digits
		 */

		BnnSetToZero( BzToBn( z ), Size );

		/*
		 * init header
		 */

		BzSetSize( z, Size    );
		BzSetSign( z, BZ_ZERO );
	}

	return( z );
}

BigNumLength
BzNumDigits( const BigZ z )
{
	/*
	 * Returns the number of digits used by z.
	 */

	return( BnnNumDigits( BzToBn( z ), BzGetSize( z ) ) );
}

BigNumLength
BzLength( const BigZ z )
{
	/*
	 * Returns the number of bits used by z.
	 */

	BigNumLength nl;

	switch( BzGetSign( z ) ) {
	case BZ_MINUS :
		nl = BnnNumLength( BzToBn( z ), BzNumDigits( z ) );
		if( BnnIsPower2( BzToBn( z ), BzNumDigits( z ) ) ) {
			return( nl - 1 );
		} else	{
			return( nl );
		}
	case BZ_PLUS :
		return( BnnNumLength( BzToBn( z ), BzNumDigits( z ) ) );
	default :
		return( 0 );
	}
}

BigZ
BzCopy( const BigZ z )
{
	/*
	 * Creates a copy of the passed BigZ.
	 */

	BigZ		y;
	BigNumLength	zl;

	zl = BzNumDigits( z );

	if( (y = BzCreate( zl )) != BZNULL ) {
		/*
		 * copy the digits
		 */
		BnnAssign( BzToBn( y ), BzToBn( z ), zl );

		/*
		 * copy the header WITHOUT the size !!
		 */
		BzSetSign( y, BzGetSign( z ) );
	}

	return( y );
}

BigZ
BzNegate( const BigZ z )
{
	/*
	 * Negates the passed BigZ.
	 */

	BigZ	y;

	y = BzCopy( z );
	BzSetSign( y, BzGetOppositeSign( z ) );

	return( y );
}

BigZ
BzAbs( const BigZ z )
{
	/*
	 * Takes the absolute value of the passed BigZ.
	 */

	BigZ	y;

	y = BzCopy( z );

	switch( BzGetSign( z ) ) {
	case BZ_MINUS :
		BzSetSign( y, BZ_PLUS );
		break;
	case BZ_ZERO :
		BzSetSign( y, BZ_ZERO );
		break;
	case BZ_PLUS :
		break;
	}

	return( y );
}

BzCmp
BzCompare( const BigZ y, const BigZ z )
{
	/*
	 * Returns BZ_GT	if Y > Z,
	 *         BZ_LT	if Y < Z,
	 *         BZ_EQ	otherwise.
	 */

	if( BzGetSign( y ) > BzGetSign( z ) ) {
		return( BZ_GT );
	} else	if( BzGetSign( y ) < BzGetSign( z ) ) {
		return( BZ_LT );
	} else	if( BzGetSign( y ) == BZ_PLUS ) {
		return( BnnCompare( BzToBn( y ), BzGetSize( y ),
				    BzToBn( z ), BzGetSize( z )
				  ));
	} else	if( BzGetSign( y ) == BZ_MINUS ) {
		return( BnnCompare( BzToBn( z ), BzGetSize( z ),
				    BzToBn( y ), BzGetSize( y )
				  ));
	} else	{
		return( BZ_EQ );
	}
}

BigZ
BzAdd( const BigZ y, const BigZ z )
{
	/*
	 * Returns Y + Z.
	 */

 	BigZ		n;
	BigNumLength	yl;
	BigNumLength	zl;

	yl = BzNumDigits( y );
	zl = BzNumDigits( z );

	if( BzGetSign( y ) == BzGetSign( z ) ) {
		/*
		 * Add magnitudes if signs are the same
		 */
		switch( BnnCompare( BzToBn( y ), yl, BzToBn( z ), zl ) ) {
		case BZ_EQ:
		case BZ_GT:	/* |Y| >= |Z| */
			if( (n = BzCreate( yl+1 )) != BZNULL ) {
				BnnAssign( BzToBn( n ), BzToBn( y ), yl );
				(void)BnnAdd( BzToBn(n),
					      yl+1,
					      BzToBn(z),
					      zl,
					      BN_NOCARRY );
				BzSetSign( n, BzGetSign( y ) );
			}
			break;

		default:	/* BZ_LT: |Y| < |Z| */
			if( (n = BzCreate( zl+1 )) != BZNULL ) {
				BnnAssign( BzToBn( n ), BzToBn( z ), zl );
				(void)BnnAdd( BzToBn(n),
					      zl+1,
					      BzToBn(y),
					      yl,
					      BN_NOCARRY );
				BzSetSign( n, BzGetSign( z ) );
			}
			break;
		}
	} else	{
		/*
		 * Subtract magnitudes if signs are different
		 */
		switch( BnnCompare( BzToBn( y ), yl, BzToBn( z ), zl ) ) {
		case BZ_EQ:	/* Y = -Z */
			n = BzCreate( (BigNumLength)1 );
			break;

		case BZ_GT:	/* |Y| > |Z| */
			if( (n = BzCreate (yl)) != BZNULL ) {
			    BnnAssign( BzToBn( n ), BzToBn( y ), yl );
			    (void)BnnSubtract( BzToBn(n),
					       yl,
					       BzToBn(z),
					       zl,
					       BN_CARRY );
			    BzSetSign( n, BzGetSign( y ) );
			}
			break;

		default:	/* BZ_LT: |Y| < |Z| */
			if( (n = BzCreate( zl )) != BZNULL ) {
			    BnnAssign( BzToBn( n ), BzToBn( z ), zl );
			    (void)BnnSubtract( BzToBn(n),
					       zl,
					       BzToBn(y),
					       yl,
					       BN_CARRY );
			    BzSetSign( n, BzGetSign( z ) );
			}
			break;
		}
	}

	return( n );
}

BigZ
BzSubtract( const BigZ y, const BigZ z )
{
	/*
	 * Returns Y - Z.
	 */

	BigZ	diff;

	if( y != z ) {
		BzSetSign( z, BzGetOppositeSign( z ) );
		diff = BzAdd( y, z );
		BzSetSign( z, BzGetOppositeSign( z ) );
	} else	{
		diff = BzFromInteger( 0 );
	}

	return( diff );
}

BigZ
BzMultiply( const BigZ y, const BigZ z )
{
	/*
	 * Returns Y * Z.
	 */

	BigZ		n;
	BigNumLength	yl;
	BigNumLength	zl;

	yl = BzNumDigits( y );
	zl = BzNumDigits( z );

	if( (n = BzCreate( yl+zl )) != BZNULL ) {
	    (void)BnnMultiply(BzToBn(n), yl+zl, BzToBn(y), yl, BzToBn(z), zl);
	    BzSetSign( n, BzGetSign( y ) * BzGetSign( z ) );
	}

	return( n );
}

BigZ
BzDivide( const BigZ y, const BigZ z, BigZ *r )
{

	/*
	 * Returns Y div Z => Q
	 * Sets    Y mod Z => R,
	 *
	 * such that Y = ZQ + R
	 */

	BigZ		q;
	BigNumLength 	yl;
	BigNumLength	zl;
	BigNumLength	ql;
	BigNumLength	rl;

	if( BzGetSign( z ) == BZ_ZERO ) {
		return( BZNULL );
	}

	yl = BzNumDigits( y );
	zl = BzNumDigits( z );
	ql = (BigNumLength)MaxInt( (int)yl-(int)zl+1, 1) + 1;
	rl = (BigNumLength)MaxInt( zl, yl ) + 1;

	/*
	 * Set up quotient, remainder
	 */

	q  = BzCreate( ql );
	*r = BzCreate( rl );

	if( *r == 0 || q == 0 ) {
		return( BZNULL );
	}

	BnnAssign( BzToBn( *r ), BzToBn( y ), yl );

	/*
	 * Do the division
	 */

	BnnDivide( BzToBn( *r ), rl, BzToBn( z ), zl );
	BnnAssign( BzToBn( q ), BzToBn( *r ) + zl, rl-zl );
	BnnSetToZero( BzToBn( *r ) + zl, rl-zl );
	rl = zl;

	/*
	 * Correct the signs, adjusting the quotient and remainder
	 */

	if( BnnIsZero( BzToBn( *r ), rl ) == BN_FALSE ) {
		/*
		 * R<>0
		 */
		if( BzGetSign( y ) != BzGetSign( z ) ) {
			/*
			 * (Z-R) => R
			 */
			BnnComplement( BzToBn( *r ), rl );
			(void)BnnAdd( BzToBn(*r), rl, BzToBn(z), zl, BN_CARRY );
			/*
			 * (Q+1) => Q
			 */
			(void)BnnAddCarry( BzToBn(q), ql, BN_CARRY );
		}
		/*
		 * The sign of the result (mod) is the sign of z.
		 */
		BzSetSign( *r, BzGetSign( z ) );
	} else	{
		/*
		 * Set sign to BZ_ZERO
		 * (already made by BzCreate but makes it clear)
		 */
		BzSetSign( *r, BZ_ZERO );
	}

	/*
	 * Correct the sign of the quotient.
	 */

	if( BnnIsZero( BzToBn( q ), ql ) == BN_TRUE ) {
		BzSetSign( q, BZ_ZERO );
	} else	{
		BzSetSign( q, BzGetSign( y ) * BzGetSign( z ) );
	}

	return( q );
}

BigZ
BzDiv( const BigZ y, const BigZ z )
{
	/*
	 * Returns div( Y, Z ).
	 */

	BigZ	q;
	BigZ	r = (BigZ)0;

	q = BzDivide( y, z, &r );
	BzFree( r );

	return( q );
}

BigZ
BzTruncate( const BigZ y, const BigZ z )
{
	/*
	 * Returns truncate( Y, Z ).
	 */

	BigZ		q;
	BigZ		r = (BigZ)0;
	BigNumLength	ql;

	q  = BzDivide( y, z, &r );
	ql = BzNumDigits( q );

	if( BzGetSign( q ) == BZ_MINUS && BzGetSign( r ) != BZ_ZERO ) {
		/*
		 *	Q < 0, R <> 0, 2*R>= Z : Q-1 => Q
		 */
		(void)BnnSubtractBorrow( BzToBn( q ), ql, BN_NOCARRY );

		if( BnnIsZero( BzToBn( q ), ql ) == BN_TRUE ) {
			BzSetSign( q, BZ_ZERO );
		}
	} else	if( BnnIsZero( BzToBn(q), ql ) == BN_TRUE &&
		    BzGetSign( y ) == BzGetSign( z ) ) {
		/*
		 *	Q == 0, sign(Y) == sign(Z) : 0 => Q
		 */
		BzFree( q );
		q = BzFromInteger( 0 );
	}

	BzFree( r );

	return( q );
}

BigZ
BzFloor( const BigZ y, const BigZ z )
{
	/*
	 * Returns floor( Y, Z ).
	 */

	BigZ	q;
	BigZ	r = (BigZ)0;

	q = BzDivide( y, z, &r );
	BzFree( r );

	return( q );
}

BigZ
BzCeiling( const BigZ y, const BigZ z )
{
	/*
	 * Returns ceiling( Y, Z ).
	 */

	BigZ		q;
	BigZ		r = (BigZ)0;
	BigNumLength	ql;

	q  = BzDivide( y, z, &r );
	ql = BzNumDigits( q );

	if( BzGetSign( q ) == BZ_PLUS && BzGetSign( r ) != BZ_ZERO ) {
		/*
		 *	Q > 0, R <> 0 : Q+1 => Q
		 */
		BigNumDigit one = BN_ONE;
		(void)BnnAdd(BzToBn(q), ql, &one, (BigNumLength)1, BN_NOCARRY);
	} else	if( BzGetSign( q ) == BZ_MINUS && BzGetSign( r ) != BZ_ZERO ) {
		/*
		 *	Q < 0, R <> 0 : Q-1 => Q
		 */
		(void)BnnSubtractBorrow( BzToBn( q ), ql, BN_NOCARRY );
		if( BnnIsZero( BzToBn( q ), ql ) == BN_TRUE ) {
			BzSetSign( q, BZ_ZERO );
		}
	} else	if( BzGetSign( q ) == BZ_ZERO
		    && BzGetSign( y ) == BzGetSign( z ) ) {
		/*
		 *	Q == 0, sign(Y) == sign(Z) : 1 => Q
		 */
		BzFree( q );
		q = BzFromInteger( 1 );
	}

	BzFree( r );

	return( q );
}

BigZ
BzRound( const BigZ y, const BigZ z )
{
	/*
	 * Returns round( Y, Z ).
	 */

	BigZ		q;
	BigZ		r = (BigZ)0;
	BigNumLength	ql;

	q  = BzDivide( y, z, &r );
	ql = BzNumDigits( q );

	if( BzGetSign( q ) == BZ_PLUS && BzGetSign( r ) != BZ_ZERO ) {
		BigNumDigit one = BN_ONE;
		BigZ	roundz;
		int	sign = BzGetSign( z );

		BzSetSign( r, BZ_PLUS );
		BzSetSign( z, BZ_PLUS );

		roundz = BzAsh( r, 1 );

		switch( BzCompare( roundz, z ) ) {
		case BZ_GT :
			/*
			 *	Q > 0, R <> 0, 2*R>= Z : Q+1 => Q
			 */
			(void)BnnAdd( BzToBn(q),
				      ql,
				      &one,
				      (BigNumLength)1,
				      BN_NOCARRY );
			break;
		case BZ_EQ :
			/*
			 *	Q > 0, R <> 0, 2*R= Z :
			 *
			 * roundz is exactly halfway between two integers,
			 * choose even number.
			 */
			if( BzIsOdd( q ) ) {
				(void)BnnAdd( BzToBn(q),
					      ql,
					      &one,
					      (BigNumLength)1,
					      BN_NOCARRY );
			}
			break;
		}

		BzSetSign( z, sign );
		BzFree( roundz );
	} else	if( BzGetSign( q ) == BZ_MINUS && BzGetSign( r ) != BZ_ZERO ) {
		/*
		 *	Q < 0, R <> 0, 2*R>= Z : Q-1 => Q
		 */
		BigZ	roundz;
		BzSign	sign = BzGetSign( z );

		BzSetSign( r, BZ_PLUS );
		BzSetSign( z, BZ_PLUS );

		roundz = BzAsh( r, 1 );

		switch( BzCompare( roundz, z ) ) {
		case BZ_GT :
			(void)BnnSubtractBorrow( BzToBn( q ), ql, BN_NOCARRY );
			break;
		case BZ_EQ :
			/*
			 * roundz is exactly halfway between two integers,
			 * choose even number.
			 */
			if( BzIsOdd( q ) ) {
				(void)BnnSubtractBorrow( BzToBn( q ),
							 ql,
							 BN_NOCARRY );
			}
			break;
		}

		BzSetSign( z, sign );
		BzFree( roundz );

		if( BnnIsZero( BzToBn( q ), ql ) == BN_TRUE ) {
			BzSetSign( q, BZ_ZERO );
		}
	} else	if( (BzGetSign( q ) == BZ_ZERO)
		    && (BzGetSign( y ) == BzGetSign( z )) ) {
		/*
		 *	Q == 0, sign(Y) == sign(Z):
		 */
		BigZ	roundz;
		int	sign = BzGetSign( z );

		BzFree( q );

		BzSetSign( r, BZ_PLUS );
		BzSetSign( z, BZ_PLUS );

		roundz = BzAsh( r, 1 );

		if( BzCompare( roundz, z ) == BZ_LT ) {
			/*
			 *	2*R< Z : 0 => Q
			 */
			q = BzFromInteger( 0 );
		} else	{
			/*
			 *	2*R>= Z : 1 => Q
			 */
			q = BzFromInteger( 1 );
		}

		BzSetSign( z, sign );
		BzFree( roundz );
	}

	BzFree( r );

	return( q );
}

BigZ
BzMod( const BigZ y, const BigZ z )
{
	/*
	 * Returns Y mod Z.
	 */

	BigZ	r = (BigZ)0;

	BzFree( BzDivide( y, z, &r ) );

	return( r );
}

BigZ
BzRem( const BigZ y, const BigZ z )
{
	/*
	 * Returns Y rem Z.
	 */

	BigZ q;
	BigZ r = (BigZ)0;
	BigZ rem;

	q = BzDivide( y, z, &r );
	BzFree( q );

	if( BzGetSign( r ) == BZ_ZERO ) {
		return( r );
	} else	if( BzGetSign( y ) == BzGetSign( z ) ) {
		return( r );
	} else	if( BzGetSign( y ) == BZ_MINUS ) {
		rem = BzSubtract( z, r );
		BzSetSign( rem, BZ_MINUS );
		BzFree( r );
		return( rem );
	} else	{
		rem = BzSubtract( r, z );
		BzSetSign( rem, BZ_PLUS );
		BzFree( r );
		return( rem );
	}
}

Boolean
BzIsEven( const BigZ y )
{
	/*
	 * Returns BN_TRUE iff y is even
	 */

	return( BnnIsDigitEven( BzGetDigit( y, 0 ) ) );
}

Boolean
BzIsOdd( const BigZ y )
{
	/*
	 * Returns BN_TRUE iff y is odd
	 */

	return( BnnIsDigitOdd( BzGetDigit( y, 0 ) ) );
}

BzChar *
BzToString( const BigZ z, BigNumDigit base, int sign )
{
	/*
	 * wrapper to BzToStringBuffer that always allocate buffer.
	 */

	return( BzToStringBuffer( z, base, sign, (BzChar *)0, (size_t *)0 ) );
}

BzChar *
BzToStringBuffer( const BigZ z, BigNumDigit base, int sign, BzChar *buf, size_t *len )
{

	/*
	 * Returns  a  pointer  to  a string that represents Z in the
	 * specified   base.   Assumes   BZ_MIN_BASE   <=   base   <=
	 * BZ_MAX_BASE.  If  optional  buffer  is supplied,  len is a
	 * pointer  of  this buffer size.  If there is enough room to
	 * print  the  number  buf is used otherwise function returns
	 * NULL and len contains the required size.  If buf is passed
	 * as NULL,  this string is allocated on the heap, so it must
	 * be desallocated by the user.
	 */

	static const BzChar Digit[] = {
		(BzChar)'0', (BzChar)'1', (BzChar)'2', (BzChar)'3',
		(BzChar)'4', (BzChar)'5', (BzChar)'6', (BzChar)'7',
		(BzChar)'8', (BzChar)'9', (BzChar)'a', (BzChar)'b',
		(BzChar)'c', (BzChar)'d', (BzChar)'e', (BzChar)'f',
		(BzChar)'g', (BzChar)'h', (BzChar)'i', (BzChar)'j',
		(BzChar)'k', (BzChar)'l', (BzChar)'m', (BzChar)'n',
		(BzChar)'o', (BzChar)'p', (BzChar)'q', (BzChar)'r',
		(BzChar)'s', (BzChar)'t', (BzChar)'u', (BzChar)'v',
		(BzChar)'w', (BzChar)'x', (BzChar)'y', (BzChar)'z'
	};

	BigZ		y;
	BigZ		q;
	BigZ		v;
	BigNumDigit	r;
	BigNumLength	zl;
	BigNumLength	sl;
	BzChar *	s;
	BzChar *	strg;
	int		sd;

	if( base < (BigNumDigit)BZ_MIN_BASE
	    || base > (BigNumDigit)BZ_MAX_BASE ) {
		return( NULL );
	}

	/*
	 * Allocate BigNums and set up string
	 */

	zl = BzNumDigits( z ) + 1;
	sl = (BigNumLength)(BzLog[2] * BN_DIGIT_SIZE * zl / BzLog[base] + 3);

	if( buf != (BzChar *)NULL
	    && len != (size_t *)NULL
	    && (sl > (BigNumLength)*len) ) {
		/*
		 * a buffer is passed but there is not enough room,
		 * return NULL and set required size in len.
		 */
		*len = (size_t)sl;
		return( (BzChar *)NULL );
	}

	y = BzCreate( zl );
	q = BzCreate( zl );

	if( buf != (BzChar *)NULL ) {
		strg = buf;
	} else	{
		strg = (BzChar *)BzStringAlloc( (size_t)sl );
	}

	if( y == 0 || q == 0 || strg == 0 ) {
		return( (BzChar *)NULL );
	}

	BnnAssign( BzToBn( y ), BzToBn( z ), zl - 1 );
	s = strg + sl;

	/*
	 * Divide Z by base repeatedly; successive digits given by remainders
	 */

	*--s = (BzChar)'\0';
	if( BzGetSign( z ) == BZ_ZERO ) {
		*--s = (BzChar)'0';
#if	defined( BZ_OPTIMIZE_FOR_BASE10 )
	} else	if( base == (BigNumDigit)10 ) {
		/*
		 * This optimization makes BigZ output 10 to 20x faster.
		 */
		do {
			/*
			 * compute: y div BZ_MAX_BASE10 => q,
			 * returns r = y mod BZ_MAX_BASE10
			 *
			 * BZ_MAX_BASE10 is the greatest integer that fits
			 * in a BigNumDigit.
			 */

			r = BnnDivideDigit( BzToBn( q ),
					    BzToBn( y ),
					    zl,
					    BZ_MAX_BASE10 );
		
			if( BnnIsZero( BzToBn( q ), zl ) == BN_FALSE ) {
				/*
				 * More digits to come on left, add exactly
				 * BZ_MAX_BASE10_DIGITS digits with possible
				 * leading 0.
				 */
				int	i;
				for( i = 0 ; i < BZ_MAX_BASE10_DIGITS ; ++i ) {
					*--s = Digit[r % 10];
					r = r / 10;
				}
			} else	{
				/*
				 * Last serie (top left). Print only required
				 * digits.
				 */
				while( r != 0 ) {
					*--s = Digit[r % 10];
					r = r / 10;
				}
			}

			/*
			 * exchange y and q (to avoid BzMove( y, q ))
			 */

			v = q;
			q = y;
			y = v;
		} while( BnnIsZero( BzToBn( y ), zl ) == BN_FALSE );
#endif
	} else	do {
		/* compute: y div base => q, returns r = y mod base */

		r = BnnDivideDigit( BzToBn( q ), BzToBn( y ), zl, base );
		*--s = Digit[r];

		/*
		 * exchange y and q (to avoid BzMove( y, q ))
		 */

		v = q;
		q = y;
		y = v;
	} while( BnnIsZero( BzToBn( y ), zl ) == BN_FALSE );

    	/*
	 * Set sign if negative
	 */

	if( BzGetSign( z ) < 0 ) {
		*--s = (BzChar)'-';
	} else	if( sign == BZ_FORCE_SIGN ) {
		*--s = (BzChar)'+';
	}

	/*
	 * and move string into position
	 */

	if( (sd = (int)(s - strg)) > 0 ) {
		while( s < (strg + sl) ) {
			*(s - sd) = *s;
			s++;
		}
	}

	/*
	 * Free temporary BigNums and return the string
	 */

	BzFree( y );
	BzFree( q );

	return( strg );
}

static	int
BzStrLen( const BzChar *s )
{
	int	len;

	for( len = 0; *s++ != (BzChar)'\000' ; ++len ) {
		continue;
	}

	return( len );
}

BigZ
BzFromString( const BzChar *s, BigNumDigit base )
{
	/*
	 * Creates  a  BigZ whose value is represented by "string" in
	 * the  specified  base.  The  "string"  may  contain leading
	 * spaces,  followed  by  an  optional  sign,  followed  by a
	 * series of digits. Assumes BZ_MIN_BASE<=base <=BZ_MAX_BASE.
	 * When called from C, only the first 2 arguments are passed.
	 */

	BigZ		z;
	BigZ		p;
	BigZ		v;
	BzSign		sign;
	BigNumLength 	zl;

	/*
	 * Throw away any initial space
	 */

	while( *s == (BzChar)' ' ) {
		s++;
	}

	/*
	 * Allocate BigNums
	 */

	zl = (BigNumLength)(BzStrLen(s)*BzLog[base]/(BzLog[2]*BN_DIGIT_SIZE)+1);

	z = BzCreate( zl );
	p = BzCreate( zl );

	if( z == 0 || p == 0 ) {
		return( BZNULL );
	}

	/*
	 * Set up sign, base, initialize result
	 */

	sign = (*s=='-' ? (s++, BZ_MINUS) : *s=='+' ? (s++, BZ_PLUS) : BZ_PLUS);

	/*
	 * Multiply in the digits of the string, one at a time
	 */

	for( ; *s != (BzChar)'\000' ;  s++ ) {
		BigNumDigit next = (BigNumDigit)CTOI( *s );

		if( next >= base ) {
			BzFree( p );
			BzFree( z );
			return( BZNULL );
		}

		BnnSetToZero( BzToBn( p ), zl );
		BnnSetDigit( BzToBn( p ), next );
		(void)BnnMultiplyDigit( BzToBn(p), zl, BzToBn(z), zl, base );

		/*
		 * exchange z and p (to avoid BzMove (z, p)
		 */

		v = p;
		p = z;
		z = v;
	}

	/*
	 * Set sign of result
	 */

	BzSetSign( z, BnnIsZero( BzToBn( z ), zl ) ? BZ_ZERO : sign );

	/*
	 * Free temporary BigNums
	 */

	BzFree( p );

	return( z );
}

BigZ
BzFromInteger( BzInt i )
{
	BigZ	z;

	z = BzCreate( (BigNumLength)1 );

	BzSetDigit( z, 0, (BigNumDigit)AbsInt( i ) );

	if( i > 0 ) {
		BzSetSign( z, BZ_PLUS  );
	} else	if( i < 0 ) {
		BzSetSign( z, BZ_MINUS );
	} else	{
		BzSetSign( z, BZ_ZERO  );
	}

	return( z );
}

BigZ
BzFromUnsignedInteger( BzUInt i )
{
	BigZ	z;

	z = BzCreate( (BigNumLength)1 );

	BzSetDigit( z, 0, (BigNumDigit)i );
	BzSetSign( z, ((i > 0) ? BZ_PLUS : BZ_ZERO) );

	return( z );
}

BzInt
BzToInteger( const BigZ z )
{
	if( BzNumDigits( z ) > (BigNumLength)1 ) {
		return( BZMAXINT );
	} else	if( BzGetSign( z ) == BZ_MINUS ) {
		return( - (BzInt)BzGetDigit( z, 0 ) );
	} else	{
		return(   (BzInt)BzGetDigit( z, 0 ) );
	}
}

int
BzToIntegerPointer( const BigZ z, BzInt *p )
{
	BzInt value;

	if( BzNumDigits( z ) > (BigNumLength)1 ) {
		*p = BZMAXINT;
		return( 0 );
	}

	value = (BzInt)BzGetDigit( z, 0 );

	if( value < 0 ) {
		*p = BZMAXINT;
		return( 0 );
	} else	if( BzGetSign( z ) == BZ_MINUS ) {
		*p = -value;
	} else	{
		*p =  value;
	}

	return( 1 );
}

BzUInt
BzToUnsignedInteger( const BigZ z )
{
	if( BzNumDigits( z ) > (BigNumLength)1 ) {
		return( (BzUInt)BZMAXINT );
	} else	{
		return( (BzUInt)BzGetDigit( z, 0 ) );
	}
}

int
BzToUnsignedIntegerPointer( const BigZ z, BzUInt *p )
{
	if( BzNumDigits( z ) > (BigNumLength)1 ) {
		*p = (BzUInt)BZMAXINT;
		return( 0 );
	} else	{
		*p = (BzUInt)BzGetDigit( z, 0 );

		return( 1 );
	}
}

BigZ
BzFromBigNum( BigNum n, BigNumLength nl )
{
	BigZ		z;
	BigNumLength	i;

	z = BzCreate( nl );

	/*
	 * set the sign of z such that the pointer n is unchanged yet
	 */

	if( BnnIsZero( n, nl ) == BN_TRUE  ) {
		BzSetSign( z, BZ_ZERO );
	} else	{
		BzSetSign( z, BZ_PLUS );
	}

	for( i = 0 ; i < nl ; i++, n++ ) {
		BzSetDigit( z, i, *n );
	}

	return( z );
}

BigNum
BzToBigNum( const BigZ z, BigNumLength *nl )
{
	BigNum		n;
	BigNum		m;
	BigNumLength	i;

	if( BzGetSign( z ) == BZ_MINUS ) {
		return( (BigNum)NULL );
	}

	*nl = BzNumDigits( z );

	if( (n = (BigNum)(BzAlloc(((*nl+1) * sizeof(BigNumDigit))))) != NULL ) {
		*n = (BigNumDigit)*nl; /* set size */

		for( i = 0, m = ++n ; i < *nl ; i++, m++ ) {
			*m = BzGetDigit( z, i );
		}
	}

	return( n );
}

/*
 * The  logical  operations  provide a convenient way to represent an
 * infinite  vector of bits.  Let such a conceptual vector be indexed
 * by the non-negative integers.  Then bit j is assigned a ``weight''
 * 2**j.  Assume  that only a finite number of bits are 1's or only a
 * finite number of bits are 0's.  A vector with only a finite number
 * of  one-bits  is  represented  as  the  sum  of the weights of the
 * one-bits,  a positive integer.  A vector with only a finite number
 * of  zero-bits is represented as -1 minus the sum of the weights of
 * the zero-bits, a negative integer.
 *
 * This  method  of  using  integers  to represent bit-vectors can in
 * turn  be  used  to  represent  sets.  Suppose  that some (possibly
 * countably  infinite) universe of discourse for sets is mapped into
 * the non-negative integers.  Then a set can be represented as a bit
 * vector;  an  element  is  in  the  set  if  the  bit  whose  index
 * corresponds  to that element is a one-bit.  In this way all finite
 * sets  can  be  represented (by positive integers),  as well as all
 * sets whose complements are finite (by negative integers).
 */

#define	BZ_SIGN1	0x1	/* 01 */
#define	BZ_SIGN2	0x2	/* 10 */

BigZ
BzNot( const BigZ z )
{
	/*
	 * Negates the passed BigZ.
	 */

	BigZ	y;

	switch( BzGetSign( z ) ) {
	case BZ_MINUS:
		y = BzCopy( z );
		BnnComplement2( BzToBn( y ), BzNumDigits( y ) );
		BnnComplement( BzToBn( y ), BzNumDigits( y ) );
		if( BnnIsZero( BzToBn( y ), BzNumDigits( y ) ) == BN_TRUE ) {
			/*
			 * ~(-1) -> 0
			 */
			BzSetSign( y, BZ_ZERO );
		} else	{
			BzSetSign( y, BZ_PLUS );
		}
		break;
	case BZ_ZERO:
		y = BzFromInteger( -1 );
		break;
	default: /* case BZ_PLUS: */
		y = BzCopy( z );
		BnnComplement( BzToBn( y ), BzNumDigits( y ) );
		BnnComplement2( BzToBn( y ), BzNumDigits( y ) );
		BzSetSign( y, BZ_MINUS );
		break;
	}

	return( y );
}

BigZ
BzAnd( const BigZ y, const BigZ z )
{
	/*
	 * Returns Y & Z.
	 */

	BigZ		n;
	BigZ		yy;
	BigZ		zz;
	BigNumLength	yl;
	BigNumLength	zl;
	unsigned int	sign = 0;

	yl = BzNumDigits( y );
	zl = BzNumDigits( z );

	if( BzGetSign( y ) == BZ_MINUS ) {
		yy = BzCopy( y );
		BnnComplement2( BzToBn( yy ), yl );
		sign |= BZ_SIGN1;
	} else	{
		yy = y;
	}

	if( BzGetSign( z ) == BZ_MINUS ) {
		zz = BzCopy( z );
		BnnComplement2( BzToBn( zz ), zl );
		sign |= BZ_SIGN2;
	} else	{
		zz = z;
	}

	if( yl < zl ) {
		n = BzCopy( zz );
		BzSetSign( n, BZ_PLUS );
		if( (sign & BZ_SIGN1) == 0 ) {
			while( zl-- != yl ) {
				BnnAndDigits( BzToBn( n ) + zl, BN_NOCARRY );
			}
		}
		while( yl-- != 0 ) {
			BnnAndDigits( BzToBn( n ) + yl, *(BzToBn( yy ) + yl) );
		}
	} else	{
		n = BzCopy( yy );
		BzSetSign( n, BZ_PLUS );
		if( (sign & BZ_SIGN2) == 0 ) {
			while( zl != yl-- ) {
				BnnAndDigits( BzToBn( n ) + yl, BN_NOCARRY );
			}
		}
		while( zl-- != 0 ) {
			BnnAndDigits( BzToBn( n ) + zl, *(BzToBn( zz ) + zl) );
		}
	}

	if( BnnIsZero( BzToBn( n ), BzNumDigits( n ) ) == BN_TRUE ) {
		BzSetSign( n, BZ_ZERO );
	} else	if( sign == (unsigned int)(BZ_SIGN1 | BZ_SIGN2) ) {
		BnnComplement2( BzToBn( n ), BzNumDigits( n ) );
		BzSetSign( n, BZ_MINUS );
	}

	/*
	 *  Free copies.
	 */

	if( (sign & BZ_SIGN1) != 0 ) {
		BzFree( yy );
	}

	if( (sign & BZ_SIGN2) != 0 ) {
		BzFree( zz );
	}

	return( n );
}

BigZ
BzOr( const BigZ y, const BigZ z )
{
	/*
	 * Returns Y | Z.
	 */

	BigZ		n;
	BigZ		yy;
	BigZ		zz;
	BigNumLength	yl;
	BigNumLength	zl;
	unsigned int	sign = 0;

	yl = BzNumDigits( y );
	zl = BzNumDigits( z );

	if( BzGetSign( y ) == BZ_MINUS ) {
		yy = BzCopy( y );
		BnnComplement2( BzToBn( yy ), yl );
		sign |= BZ_SIGN1;
	} else	{
		yy = y;
	}

	if( BzGetSign( z ) == BZ_MINUS ) {
		zz = BzCopy( z );
		BnnComplement2( BzToBn( zz ), zl );
		sign |= BZ_SIGN2;
	} else	{
		zz = z;
	}

	if( yl < zl ) {
		n = BzCopy( zz );
		BzSetSign( n, BZ_PLUS );
		if( (sign & BZ_SIGN1) != 0 ) {
			while( zl-- != yl ) {
				BnnAndDigits( BzToBn( n ) + zl, BN_NOCARRY );
			}
		}
		while( yl-- != 0 ) {
			BnnOrDigits( BzToBn( n ) + yl, *(BzToBn( yy ) + yl) );
		}
	} else	{
		n = BzCopy( yy );
		BzSetSign( n, BZ_PLUS );
		if( (sign & BZ_SIGN2) != 0 ) {
			while( zl != yl-- ) {
				BnnAndDigits( BzToBn( n ) + yl, BN_NOCARRY );
			}
		}
		while( zl-- != 0 ) {
			BnnOrDigits( BzToBn( n ) + zl, *(BzToBn( zz ) + zl) );
		}
	}

	if( BnnIsZero( BzToBn( n ), BzNumDigits( n ) ) == BN_TRUE ) {
		BzSetSign( n, BZ_ZERO );
	} else	if( sign != 0 ) {
		BnnComplement2( BzToBn( n ), BzNumDigits( n ) );
		BzSetSign( n, BZ_MINUS );
	}

	/*
	 *  Free copies.
	 */

	if( (sign & BZ_SIGN1) != 0 ) {
		BzFree( yy );
	}

	if( (sign & BZ_SIGN2) != 0 ) {
		BzFree( zz );
	}

	return( n );
}

BigZ
BzXor( const BigZ y, const BigZ z )
{
	/*
	 * Returns Y ^ Z.
	 */

	BigZ		n;
	BigZ		yy;
	BigZ		zz;
	BigNumLength	yl;
	BigNumLength	zl;
	int		sign = 0;

	yl = BzNumDigits( y );
	zl = BzNumDigits( z );

	if( BzGetSign( y ) == BZ_MINUS ) {
		yy = BzCopy( y );
		BnnComplement2( BzToBn( yy ), yl );
		sign |= BZ_SIGN1;
	} else	{
		yy = y;
	}

	if( BzGetSign( z ) == BZ_MINUS ) {
		zz = BzCopy( z );
		BnnComplement2( BzToBn( zz ), zl );
		sign |= BZ_SIGN2;
	} else	{
		zz = z;
	}

	if( yl < zl ) {
		n = BzCopy( zz );
		BzSetSign( n, BZ_PLUS );
		if( (sign & BZ_SIGN1) != 0 ) {
			while( zl-- != yl ) {
				BnnXorDigits( BzToBn(n) + zl, (BigNumDigit)-1 );
			}
		}
		while( yl-- != 0 ) {
			BnnXorDigits( BzToBn( n ) + yl, *(BzToBn( yy ) + yl) );
		}
	} else	{
		n = BzCopy( yy );
		BzSetSign( n, BZ_PLUS );
		if( (sign & BZ_SIGN2) != 0 ) {
			while( zl != yl-- ) {
				BnnXorDigits( BzToBn(n) + yl, (BigNumDigit)-1 );
			}
		}
		while( zl-- != 0 ) {
			BnnXorDigits( BzToBn( n ) + zl, *(BzToBn( zz ) + zl) );
		}
	}

	if( BnnIsZero( BzToBn( n ), BzNumDigits( n ) ) == BN_TRUE ) {
		BzSetSign( n, BZ_ZERO );
	} else	if( sign == BZ_SIGN1 || sign == BZ_SIGN2 ) {
		BnnComplement2( BzToBn( n ), BzNumDigits( n ) );
		BzSetSign( n, BZ_MINUS );
	}

	/*
	 *  Free copies.
	 */

	if( (sign & BZ_SIGN1) != 0 ) {
		BzFree( yy );
	}

	if( (sign & BZ_SIGN2) != 0 ) {
		BzFree( zz );
	}

	return( n );
}

Boolean
BzTestBit( unsigned int bit, const BigZ z )
{
	BigNumLength	zl;
	Boolean		res;

	/*
	 * Returns  BN_TRUE  iff bit is on (i.e.  2**bit is one).  It
	 * assumes that bit is a non-negative integer.
	 */

	zl  = (BigNumLength)(bit / BN_DIGIT_SIZE);

	if( zl >= BzNumDigits( z ) ) {
		return( (BzGetSign( z ) == BZ_MINUS) ? BN_TRUE : BN_FALSE );
	}

	bit = (bit % BN_DIGIT_SIZE);

	if( BzGetSign( z ) == BZ_MINUS ) {
		BigZ y = BzCopy( z );
		BigNumLength yl = BzNumDigits( y );

		BnnComplement2( BzToBn( y ), yl );
		BzSetSign( y, BZ_PLUS );
		res = (Boolean)(((*(BzToBn( y ) + zl)) & (BN_ONE << bit)) != 0);
		BzFree( y );
	} else	{
		res = (Boolean)(((*(BzToBn( z ) + zl)) & (BN_ONE << bit)) != 0);
	}

	return( res );
}

/*
 *	Simple logical equivalence rules.
 */

BigZ
BzNand( const BigZ x, const BigZ y )
{
	BigZ tmp = BzAnd( x, y );
	BigZ res = BzNot( tmp );
	BzFree( tmp );
	return( res );
}

BigZ
BzNor( const BigZ x, const BigZ y )
{
	BigZ tmp = BzOr( x, y );
	BigZ res = BzNot( tmp );
	BzFree( tmp );
	return( res );
}

BigZ
BzEqv( const BigZ x, const BigZ y )
{
	BigZ tmp = BzXor( x, y );
	BigZ res = BzNot( tmp );
	BzFree( tmp );
	return( res );
}

BigZ
BzAndC1( const BigZ x, const BigZ y )
{
	BigZ tmp = BzNot( x );
	BigZ res = BzAnd( tmp, y );
	BzFree( tmp );
	return( res );
}

BigZ
BzAndC2( const BigZ x, const BigZ y )
{
	BigZ tmp = BzNot( y );
	BigZ res = BzAnd( x, tmp );
	BzFree( tmp );
	return( res );
}

BigZ
BzOrC1( const BigZ x, const BigZ y )
{
	BigZ tmp = BzNot( x );
	BigZ res = BzOr( tmp, y );
	BzFree( tmp );
	return( res );
}

BigZ
BzOrC2( const BigZ x, const BigZ y )
{
	BigZ tmp = BzNot( y );
	BigZ res = BzOr( x, y );
	BzFree( tmp );
	return( res );
}

BigZ
BzAsh( const BigZ y, int n )
{
	BigZ		z;
	BigNumLength	zl;
	BigNumLength	ll;

	if( n > 0 ) {
		/*
		 *	Create a copy + space for the shift.
		 */

		ll = (BigNumLength)AbsInt( n );
		zl = (BigNumLength)(ll / BN_DIGIT_SIZE);

		if( (ll % BN_DIGIT_SIZE) != 0 ) {
			zl++;
		}

		zl += BzNumDigits( y );

		z = BzCreate( zl );

		if( z == BZNULL ) {
			return( z );
		}

		BnnAssign( BzToBn( z ), BzToBn( y ), BzNumDigits( y ) );
		BzSetSign( z, BzGetSign( y ) );

		/*
		 *	Now do the shift by BN_DIGIT_SIZE increment.
		 */

		while( ll >= (BigNumLength)BN_DIGIT_SIZE ) {
			BigNumLength len = (BigNumLength)(BN_DIGIT_SIZE - 1);
			(void)BnnShiftLeft( BzToBn( z ), zl, len );
			ll -= BN_DIGIT_SIZE - 1;
		}
		(void)BnnShiftLeft( BzToBn( z ), zl, ll );
	} else	{
		BigZ	one;
		BigZ	d;

		if( BzGetSign( y ) == BZ_ZERO ) {
			return( BzCopy( y ) );
		}

		one = BzFromInteger( 1 );
		d   = BzAsh( one, -n );

		z = BzFloor( y, d );
		BzFree( d );
		BzFree( one );
	}

	return( z );
}

#if	defined( _LISP_CODE )
(defun isqrt (i)
   (unless (and (integerp i) (>= i 0))
	   (error "~S is not a non-negative integer." i))
   (if (zerop i)
       0
       (let ((n (integer-length i)))
	    (do ((x (ash 1 (ceiling n 2)))
		 (y))
		(nil)
		(setq y (floor i x))
		(when (<= x y)
		      (return x))
		(setq x (floor (+ x y) 2))))))
#endif

BigZ
BzSqrt( const BigZ z )
{
	BigNumLength	n;
	BigZ		x;
	BigZ		v;
	BigZ		one;
	BigZ		two;

	if( BzGetSign( z ) == BZ_ZERO ) {
		return( BzFromInteger( 0 ) );
	}

	one = BzFromInteger( 1 );
	two = BzFromInteger( 2 );
	n   = BzLength( z );

	if( (n % 2) != 0 ) {
		n = n / 2 + 1;
	} else	{
		n = n / 2;
	}

	x = BzAsh( one, (int)n );

	for( ;; ) {
		BigZ y = BzFloor( z, x );

		if( BzCompare( x, y ) != BZ_GT ) {
			BzFree( y );
			break;
		}

		v = BzAdd( x, y );

		BzFree( x );
		x = BzFloor( v, two );
		BzFree( v );
		BzFree( y );
	}

	/*
	 * Free temporary BigNums
	 */

	BzFree( two );
	BzFree( one );

	return( x );
}

BigZ
BzLcm( const BigZ y, const BigZ z )
{
	BigZ	a;
	BigZ	b;
	BigZ	r;

	/*
	 * Returns lcm( Y, Z ).
	 */

	a = BzMultiply( y, z );
	b = BzGcd( y, z );

	if( BzGetSign( a ) == BZ_MINUS ) {
		BzSetSign( a, BZ_PLUS );
	}

	r = BzTruncate( a, b );

	BzFree( b );
	BzFree( a );

	return( r );
}

BigZ
BzGcd( const BigZ y, const BigZ z )
{
	/*
	 * Returns gcd( Y, Z ).
	 */

	if( BzGetSign( y ) == BZ_ZERO ) {
		return( BzAbs( z ) );
	} else	if( BzGetSign( z ) == BZ_ZERO ) {
		return( BzAbs( y ) );
	} else	{
		BigZ yc = BzAbs( y ); /* a fresh copy */
		BigZ zc = BzAbs( z ); /* a fresh copy */

		while( BzGetSign( zc ) != BZ_ZERO ) {
			BigZ tmp = BzMod( yc, zc );
			BzFree( yc );
			yc = zc;
			zc = tmp;
		}
	
		BzFree( zc );

		return( yc );
	}
}

BigZ
BzRandom( const BigZ n )
{
	BigZ res;
	BigZ r;
	BigNumLength len;
	BigNumLength ilen;
	BigNumLength i;

	r    = BzCopy( n );
	len  = BzGetSize( n );
	ilen = (BigNumLength)(len * sizeof( BigNumDigit ) / sizeof( int ));

	/*
	 * Algo: make a copy of n and call rand() to replace all its bits.
	 * Assume any bit has an equiprobable [0-1] value.
	 */

	if( ilen > len ) {
		/*
		 * It means a BigNumDigit is greater than an int.
		 * fill as many int as can fit in a bignum (i.e. ilen)
		 */
		int *nn = (int *)BzToBn( r );

		for( i = 0 ; i < ilen ; ++i ) {
			*nn++ = rand();
		}
	} else	{
		BigNumDigit *nn = BzToBn( r );

		for( i = 0 ; i < len ; ++i ) {
			/*
			 * truncate rand() if ever BigNumDigit is smaller than
			 * an int.
			 */
			*nn++ = (BigNumDigit)rand();
		}
	}

	/*
	 * Call BzMod to insure result is less than n.
	 */
	res = BzMod( r, n );

	BzFree( r );

	return( res );
}

void
BzSetRandom( const BigZ n )
{
	BigNumLength len;
	BigNumLength ilen;
	BigNumLength i;
	unsigned int seed;

	len  = BzGetSize( n );
	ilen = (BigNumLength)(len * sizeof( BigNumDigit ) / sizeof( int ));
	seed = 0;

	/*
	 * Algo: sum all unsigned int of n to get a different seed values
	 * even for 2 very close big numbers.
	 */

	if( ilen > len ) {
		/*
		 * It means a BigNumDigit is greater than an int.
		 * seed is the sum of all int.
		 */
		int *nn = (int *)BzToBn( n );

		for( i = 0 ; i < ilen ; ++i ) {
			seed += *nn++;
		}
	} else	{
		/*
		 * seed is the sum of all BigNumDigit (after possible
		 * truncation to unsigned int).
		 */
		BigNumDigit *nn = BzToBn( n );

		for( i = 0 ; i < len ; ++i ) {
			seed = (unsigned int)*nn++;
		}
	}

	/*
	 * Set random.
	 */

	srand( seed );
}
