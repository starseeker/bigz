/*
 * $Id: bigq.c,v 1.5 2011-12-24 08:31:43 jullien Exp $
 */

/*
 * Simplified BSD License
 *
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

#if	!defined( __BIGQ_H )
#include "bigq.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

static	void BqNormalize(BigQ q);

BigQ
BqCreate( const BigZ n, const BigZ d )
{
	BigQ	q;
	BigZ	cn;
	BigZ	cd;

	if( BzGetSign( d ) == BZ_ZERO ) {
		return( BQNULL );
	}

	if( (q = (BigQ)BqAlloc()) == 0 ) {
		return( BQNULL );
	}

	if( BzGetSign( n ) == BZ_ZERO ) {
		BqSetNumerator(   q, BzFromInteger((BzInt)0) );
		BqSetDenominator( q, BzFromInteger((BzInt)1) );
		return( q );
	}

	cn = BzCopy( n );
	cd = BzCopy( d );

	BzSetSign( cn, BZ_PLUS );
	BzSetSign( cd, BZ_PLUS );

	if( BzGetSign( n ) != BzGetSign( d ) ) {
		BzSetSign( cn, BZ_MINUS );
	} else	if( BzGetSign( n ) == BZ_MINUS ) {
		BzSetSign( cn, BZ_MINUS );
	}

	BqSetNumerator(   q, cn );
	BqSetDenominator( q, cd );

	BqNormalize( q );
	return( q );
}

static	void
BqNormalize( BigQ q )
{
	BigZ n   = BqGetNumerator( q );
	BigZ d   = BqGetDenominator( q );
	BigZ gcd = BzGcd( n, d );
	BigZ one = BzFromInteger( (BzInt) 1 );

	if( BzCompare( gcd, one ) != BZ_EQ ) {
		BigZ nn = BzDiv( n, gcd );
		BigZ nd = BzDiv( d, gcd );

		BzFree( n );
		BzFree( d );
		
		BqSetNumerator( q, nn );
		BqSetDenominator( q, nd );
	}

	BzFree( one );
	BzFree( gcd );
}

BigQ
BqAdd( const BigQ a, const BigQ b )
{
	const BigZ an = BqGetNumerator(   a );
	const BigZ ad = BqGetDenominator( a );
	const BigZ bn = BqGetNumerator(   b );
	const BigZ bd = BqGetDenominator( b );
	BigZ n;
	BigZ d;
	BigZ tmp1;
	BigZ tmp2;
	BigQ res;

	tmp1 = BzMultiply( an, bd );
	tmp2 = BzMultiply( ad, bn );

	n = BzAdd( tmp1, tmp2 );
	BzFree( tmp2 );
	BzFree( tmp1 );

	d = BzMultiply( ad, bd );

	res = BqCreate( n, d );

	BzFree( d );
	BzFree( n );

	return( res );
}

BigQ
BqSubtract( const BigQ a, const BigQ b )
{
	const BigZ an = BqGetNumerator(   a );
	const BigZ ad = BqGetDenominator( a );
	const BigZ bn = BqGetNumerator(   b );
	const BigZ bd = BqGetDenominator( b );
	BigZ n;
	BigZ d;
	BigZ tmp1;
	BigZ tmp2;
	BigQ res;

	tmp1 = BzMultiply( an, bd );
	tmp2 = BzMultiply( ad, bn );

	n = BzSubtract( tmp1, tmp2 );
	BzFree( tmp2 );
	BzFree( tmp1 );

	d = BzMultiply( ad, bd );

	res = BqCreate( n, d );
	BzFree( d );
	BzFree( n );

	return( res );
}

BigQ
BqMultiply( const BigQ a, const BigQ b )
{
	const BigZ an = BqGetNumerator(   a );
	const BigZ ad = BqGetDenominator( a );
	const BigZ bn = BqGetNumerator(   b );
	const BigZ bd = BqGetDenominator( b );
	BigZ n;
	BigZ d;
	BigQ res;

	n = BzMultiply( an, bn );
	d = BzMultiply( ad, bd );

	res = BqCreate( n, d );

	BzFree( d );
	BzFree( n );

	return( res );
}

BigQ
BqDiv( const BigQ a, const BigQ b )
{
	const BigZ an = BqGetNumerator(   a );
	const BigZ ad = BqGetDenominator( a );
	const BigZ bn = BqGetNumerator(   b );
	const BigZ bd = BqGetDenominator( b );
	BigZ n;
	BigZ d;
	BigQ res;

	if( BzGetSign( bn ) == BZ_ZERO ) {
		return( BQNULL );
	}

	n = BzMultiply( an, bd );
	d = BzMultiply( ad, bn );

	res = BqCreate( n, d );

	BzFree( d );
	BzFree( n );

	return( res );
}

BqCmp
BqCompare( const BigQ a, const BigQ b )
{
	const BigZ an = BqGetNumerator(   a );
	const BigZ ad = BqGetDenominator( a );
	const BigZ bn = BqGetNumerator(   b );
	const BigZ bd = BqGetDenominator( b );
	BigZ tmp1;
	BigZ tmp2;
	BzCmp cmp;

	if( BzGetSign( an ) != BzGetSign( bn ) ) {
		/*
		 *	Sign differs, easy case!
		 */
		if( BzGetSign( an ) == BZ_MINUS ) {
			return( BQ_LT );
		} else	{
			return( BQ_GT );
		}
	}

	if( BzCompare( an, bn ) == BZ_EQ && BzCompare( an, bn ) == BZ_EQ ) {
		/*
		 *	Numerators and denominators are equal.
		 */
		return( BQ_EQ );
	}

	tmp1 = BzMultiply( an, bd );
	tmp2 = BzMultiply( ad, bn );
	cmp  = BzCompare( tmp1, tmp2 );

	BzFree( tmp2 );
	BzFree( tmp1 );

	switch( cmp ) {
	case BZ_LT:
		if( BzGetSign( an ) == BZ_MINUS ) {
			return( BQ_GT );
		} else	{
			return( BQ_LT );
		}
	case BZ_GT:
		if( BzGetSign( an ) == BZ_MINUS ) {
			return( BQ_LT );
		} else	{
			return( BQ_GT );
		}
	default:
		return( BQ_EQ );
	}
}

BigQ
BqNegate( const BigQ a )
{
	const BigZ an  = BqGetNumerator(   a );
	const BigZ ad  = BqGetDenominator( a );
	BigQ       res = BqCreate( an, ad );

	switch( BzGetSign( an ) ) {
	case BZ_MINUS:
		BzSetSign( BqGetNumerator(res), BZ_PLUS );
		return( res );
	case BZ_PLUS:
		BzSetSign( BqGetNumerator(res), BZ_MINUS );
		return( res );
	default:
		return( res );
	}
}

BigQ
BqAbs( const BigQ a )
{
	const BigZ an  = BqGetNumerator(   a );
	const BigZ ad  = BqGetDenominator( a );
	BigQ       res = BqCreate( an, ad );

	if( BzGetSign( an ) == BZ_MINUS ) {
		BzSetSign( an, BZ_PLUS );
	}

	return( res );
}

BigQ
BqInverse( const BigQ a )
{
	const BigZ an  = BqGetNumerator(   a );
	const BigZ ad  = BqGetDenominator( a );

	return( BqCreate( ad, an ) );
}

BzChar *
BqToString(const BigQ q, int sign)
{
	BzChar * n;
	BzChar * d;
	BzChar * res;
	size_t	len;
	int	i;

	if( q == BQNULL ) {
		return( (BzChar *)NULL );
	} else	if( BzLength(BqGetDenominator(q)) == (BigNumLength)1 ) {
		return( BzToString(BqGetNumerator(q), (BigNumDigit)10, sign) );
	} else	{
		/*
		 * Get numerator string.
		 */
		n = BzToString( BqGetNumerator(q), (BigNumDigit)10, sign );

		if( n == (BzChar *)NULL ) {
			return( n );
		}

		/*
		 * Get denominator string.
		 */
		d = BzToString( BqGetDenominator(q), (BigNumDigit)10, 0 );

		if( d == (BzChar *)NULL ) {
			BzFreeString( n );
			return( d );
		}

		/*
		 * Compute total length (don't use strlen because we
		 * don't know the exact type of BzChar - it may be wchar_t).
		 */

		len = 0;
		for( i = 0 ; n[i] != (BzChar)'\000' ; ++i ) {
			len += (size_t)i;
		}
		++len; /* for '\\' */
		for( i = 0 ; d[i] != (BzChar)'\000' ; ++i ) {
			len += (size_t)i;
		}
		++len; /* for '\000' */

		/*
		 * Alloc result string and catenate n/d.
		 */

		if( (res = (BzChar *)BzStringAlloc(len)) != (BzChar *)NULL ) {
			len = 0;
			for( i = 0 ; n[i] != (BzChar)'\000' ; ++i ) {
				res[ len++ ] = n[ i ];
			}

			res[ len++ ] = (BzChar)'/';

			for( i = 0 ; d[i] != (BzChar)'\000' ; ++i ) {
				res[ len++ ] = d[ i ];
			}

			res[ len ] = (BzChar)'\000';
		}

		BzFreeString( d );
		BzFreeString( n );

		return( res );
	}
}
