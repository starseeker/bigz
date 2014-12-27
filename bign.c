/*
 static	const char rcsid[] = "$Id: bign.c,v 1.46 2014/12/26 19:01:30 jullien Exp $";
*/

/*
 * Simplified BSD License
 *
 * Copyright (c) 1988-1989, Digital Equipment Corporation & INRIA.
 * Copyright (c) 1992-2015, Eligis
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
 *	bign.c : the kernel written in pure C (it uses no C library)
 */

/*
 *	Description of types and constants.
 *
 * Several conventions are used in the commentary:
 *    A "BigNum" is the name for an infinite-precision number.
 *    Capital letters (e.g., "N") are used to refer to the value of BigNums.
 *    The word "digit" refers to a single BigNum digit.
 *    The notation "Size(N)" refers to the number of digits in N,
 *	 which is typically passed to the subroutine as "nl".
 *    The notation "Length(N)" refers to the number of digits in N,
 *       not including any leading zeros.
 *    The word "Base" is used for the number 2 ** BN_DIGIT_SIZE, where
 *       BN_DIGIT_SIZE is the number of bits in a single BigNum digit.
 *    The expression "BBase(N)" is used for Base ** NumDigits(N).
 *    The term "leading zeros" refers to any zeros before the most
 *       significant digit of a number.
 *
 * In the code, we have:
 *
 *    "nn" is a pointer to a big number,
 *    "nl" is the number of digits from nn,
 *    "d" is a digit.
 *
 */

#if	!defined( __BIGN_H )
#include "bign.h"
#endif

static void BnnDivideHelper(BigNum nn, BigNumLength nl, BigNum dd, BigNumLength dl);

#include <stdio.h>

void
BnnSetToZero( BigNum nn, BigNumLength nl )
{
	/*
	 * Sets all the specified digits of the BigNum to BN_ZERO (0).
	 */

	BigNumLength d;

	for( d = 0 ; d < nl ; ++d ) {
		nn[d] = BN_ZERO;
	}
}

void
BnnAssign( BigNum mm, const BigNum nn, BigNumLength nl )
{
	/*
	 * Copies N => M
	 */

	int	d;

	if( (mm < nn) || (mm > (nn + nl)) ) {
		/*
		 * no memory overlap using classic loop 
		 */
		for( d = 0 ; d < (int)nl ; ++d ) {
			mm[d] = nn[d];
		}
	} else	if( mm > nn ) {
		/*
		 * memory overlap, loop starting from most significant digit
		 */
		for( d = (int)(nl - 1) ; d >= 0 ; --d ) {
			mm[d] = nn[d];
		}
	}
}

void
BnnSetDigit( BigNum nn, BigNumDigit d )
{
	/*
	 * Sets a single digit of N to the passed value
	 */

	*nn = d;
}

BigNumDigit
BnnGetDigit( const BigNum nn )
{
	/*
	 * Returns the single digit pointed by N
	 */

	return( *nn );
}

BigNumLength
BnnNumDigits( const BigNum nn, BigNumLength nl )
{
	/*
	 * Returns the number of digits of N, not counting leading zeros
	 */

	int	d;

	/*
	 * loop starting from most significant digit
	 */

	for( d = (int)(nl - 1) ; d >= 0 ; --d ) {
		if( nn[d] != BN_ZERO ) {
			/*
			 * length = d+1
			 */
			return( (BigNumLength)(d + 1) );
		}
	}

	return( (BigNumLength)1 );
}

BigNumLength
BnnNumLength( const BigNum nn, BigNumLength nl )
{
	/*
	 * Returns the number of bits of N, not counting leading zeros
	 */

	const BigNumDigit d = nn[nl - 1];
	int	i;

	for( i = (int)(BN_DIGIT_SIZE - 1) ; i >= 0 ; --i ) {
		if( (d & (BN_ONE << (BigNumLength)i)) != 0 ) {
			return((BigNumLength)(((nl-1) * BN_DIGIT_SIZE) + i+1));
		}
	}

	return( 0 );
}

BigNumLength
BnnNumCount( const BigNum nn, BigNumLength nl )
{
	/*
	 * Returns the count of bits set of N.
	 */

	BigNumLength	count = 0;
	int	j;

	for( j = 0 ; j < (int)nl ; ++j ) {
		const BigNumDigit d = nn[ j ];
		int	i;

		for( i = (int)(BN_DIGIT_SIZE - 1) ; i >= 0 ; --i ) {
			if( (d & (BN_ONE << (BigNumLength)i)) != 0 ) {
				++count;
			}
		}
	}

	return( count );
}

BigNumLength
BnnNumLeadingZeroBitsInDigit( BigNumDigit d )
{
	/*
	 * Returns the number of leading zero bits in a digit
	 */

	BigNumDigit	mask = (BigNumDigit)(BN_ONE << (BN_DIGIT_SIZE - 1));
	BigNumLength	p;

    	if( d == BN_ZERO ) {
		return( (BigNumLength)BN_DIGIT_SIZE );
	}

	for( p = 0 ; (d & mask) == 0 ; ++p ) {
		mask >>= 1;
	}

	return( p );
}

BigNumBool
BnnIsPower2( const BigNum nn, BigNumLength nl )
{
	/*
	 * Returns BN_TRUE iff nn is a power of 2.
	 */

	BigNumLength i;
	BigNumLength nbits;
	BigNumDigit  d;

	/*
	 *	The n-1 digits must be 0
	 */

	for( i = 0 ; i < (nl - 1) ; ++i ) {
		if( nn[ i ] != BN_ZERO ) {
			return( BN_FALSE );
		}
	}

	/*
	 *	There must be only 1 bit set on the last Digit.
	 */

	d     = nn[ i ];
	nbits = 0;

	for( i = 0 ; i < (BigNumLength)BN_DIGIT_SIZE ; ++i ) {
		if( (d & (BN_ONE << i)) != 0 ) {
			if( nbits++ > 0 ) {
				/*
				 * More than two digits.
				 */
				return( BN_FALSE );
			}
		}
	}

	return( BN_TRUE );
}

BigNumBool
BnnIsDigitZero( BigNumDigit d )
{
	/*
	 * Returns BN_TRUE iff digit = 0
	 */

	return( (BigNumBool)(d == 0) );
}

BigNumBool
BnnIsDigitNormalized( BigNumDigit d )
{
	/*
	 * Returns BN_TRUE iff Base/2 <= digit < Base
	 * i.e. if digit's leading bit is 1
	 */

	if( (d & (BN_ONE << (BN_DIGIT_SIZE - 1))) != 0 ) {
		return( BN_TRUE );
	} else	{
		return( BN_FALSE );
	}
}

BigNumBool
BnnIsDigitOdd( BigNumDigit d )
{
	/*
	 * Returns BN_TRUE iff digit is odd
	 */

	if( (d & 1) != 0 ) {
		return( BN_TRUE );
	} else	{
		return( BN_FALSE );
	}
}

BigNumBool
BnnIsDigitEven( BigNumDigit d )
{
	/*
	 * Returns BN_TRUE iff digit is even
	 */

	if( (d & 1) == 0 ) {
		return( BN_TRUE );
	} else	{
		return( BN_FALSE );
	}
}

BigNumCmp
BnnCompareDigits( BigNumDigit d1, BigNumDigit d2 )
{
	/*
	 * Returns 	BN_GREATER 	if digit1 > digit2
	 *		BN_EQUAL	if digit1 = digit2
	 *		BN_LESS		if digit1 < digit2
	 */

	return( (BigNumCmp)(d1 > d2 ? BN_GT : (d1 == d2 ? BN_EQ : BN_LT)) );
}

void
BnnComplement( BigNum nn, BigNumLength nl )
{
	/*
	 * Performs the computation BBase(N) - N - 1 => N
	 */

	BigNumLength d;

	for( d = 0 ; d < nl ; ++d ) {
		nn[d] ^= BN_COMPLEMENT;
	}
}

void
BnnComplement2( BigNum nn, BigNumLength nl )
{
	/*
	 * Performs the computation neg( N ) => N
	 */

	BigNumDigit one = BN_ONE;

	/*
	 * Initialize constants
	 */

	BnnComplement( nn, nl );
	(void)BnnAdd( nn, nl, &one, (BigNumLength)1, BN_NOCARRY );
}

void
BnnAndDigits( BigNum n, BigNumDigit d )
{
	/*
	 * Returns the logical computation n[0] AND d in n[0]
	 */

	*n &= d;
}

void
BnnOrDigits( BigNum n, BigNumDigit d )
{
	/*
	 * Returns the logical computation n[0] OR d2 in n[0].
	 */
	*n |= d;
}

void
BnnXorDigits( BigNum n, BigNumDigit d )
{
	/*
	 * Returns the logical computation n[0] XOR d in n[0].
	 */

	*n ^= d;
}

/*
 *	Shift operations
 */

BigNumDigit
BnnShiftLeft( BigNum mm, BigNumLength ml, BigNumLength nbits )
{
	/*
	 * Shifts  M  left by "nbits",  filling with 0s.  Returns the
	 * leftmost  "nbits"  of  M in a digit.  Assumes 0 <= nbits <
	 * BN_DIGIT_SIZE.
	 */

	BigNumDigit res = BN_ZERO;

	if( nbits != 0 ) {
		BigNumLength rnbits = (BigNumLength)(BN_DIGIT_SIZE - nbits);
		BigNumLength d;

		for( d = 0 ; d < ml ; ++d ) {
			BigNumDigit save = mm[d];
			mm[d] = (save << nbits) | res;
			res   = save >> rnbits;
		}
	}

	return( res );
}

BigNumDigit
BnnShiftRight( BigNum mm, BigNumLength ml, BigNumLength nbits )
{
	/*
	 * Shifts  M right by "nbits",  filling with 0s.  Returns the
	 * rightmost  "nbits"  of M in a digit.  Assumes 0 <= nbits <
	 * BN_DIGIT_SIZE.
	 */

	BigNumDigit res = BN_ZERO;

	if( nbits != 0 ) {
		BigNumLength lnbits = (BigNumLength)BN_DIGIT_SIZE - nbits;
		int d;

		/*
		 * loop starting from most significant digit
		 */

		for( d = (int)(ml - 1) ; d >= 0 ; --d ) {
			BigNumDigit save = mm[d];
			mm[d] = (save >> nbits) | res;
			res   = save << lnbits;
		}
	}

	return( res );
}

/*
 *	Additions
 */

BigNumCarry
BnnAddCarry( BigNum nn, BigNumLength nl, BigNumCarry carryin )
{
	/*
	 * Performs the sum N + CarryIn => N. Returns the CarryOut.
	 */

	if( carryin == BN_NOCARRY ) {
		return( BN_NOCARRY );
	} else	if( nl == 0 ) {
		return( BN_CARRY );
	} else	{
		BigNumLength d;

		for( d = 0 ; d < nl ; ++d ) {
			if( ++nn[d] != 0 ) {
				return( BN_NOCARRY );
			}
		}

		return( BN_CARRY );
	}
}

BigNumCarry
BnnAdd( BigNum mm, BigNumLength ml, const BigNum nn, BigNumLength nl, BigNumCarry carryin )
{
	/*
	 * Performs the sum M + N + CarryIn => M. Returns the CarryOut.
	 * Assumes Size(M) >= Size(N).
	 */

	BigNumProduct c = (BigNumProduct)carryin;
	BigNumLength i;

	for( i = 0 ; i < nl ; ++i ) {
		BigNumProduct save = (BigNumProduct)*mm;

		c   += save;
		if( c < save ) {
			*(mm++) = nn[ i ];
			c	= (BigNumProduct)1;
		} else	{
			save	= (BigNumProduct)nn[ i ];
			c      += save;
			*(mm++) = (BigNumDigit)c;
			c	= (BigNumProduct)((c < save) ? 1 : 0);
		}
	}

	return( BnnAddCarry( mm, ml-nl, ((c == 0) ? BN_NOCARRY : BN_CARRY) ) );
}

/*
 *	Subtraction
 */

BigNumCarry
BnnSubtractBorrow( BigNum nn, BigNumLength nl, BigNumCarry carryin )
{
	/*
	 * Performs the difference N + CarryIn - 1 => N.
	 * Returns the CarryOut.
	 */

	if( carryin == BN_CARRY ) {
		return( BN_CARRY );
	} else	if( nl == 0 ) {
		return( BN_NOCARRY );
	} else	{
		BigNumLength d;

		for( d = 0 ; d < nl ; ++d ) {
			if( nn[d]-- != 0 ) {
				return( BN_CARRY );
			}
		}

		return( BN_NOCARRY );
	}
}

BigNumCarry
BnnSubtract( BigNum mm, BigNumLength ml, const BigNum nn, BigNumLength nl, BigNumCarry carryin )
{
	/*
	 * Performs the difference M - N + CarryIn - 1 => M.
	 * Returns the CarryOut. Assumes Size(M) >= Size(N).
	 */

	BigNumProduct 	c = (BigNumProduct)((carryin == BN_CARRY) ? 1 : 0);
	BigNumDigit 	invn;
	BigNumProduct	save;
	BigNumLength    i;

	for( i = 0 ; i < nl ; ++ i ) {
		save = (BigNumProduct)*mm;
		invn = nn[ i ] ^ BN_COMPLEMENT;
		c += save;

		if( c < save ) {
			*(mm++) = invn;
			c	= (BigNumProduct)1;
		} else	{
			c      += invn;
			*(mm++) = (BigNumDigit)c;
			c	= (BigNumProduct)((c < invn) ? 1 : 0);
		}
	}

	if( c == 0 ) {
		return( BnnSubtractBorrow( mm, ml-nl, BN_NOCARRY ) );
	} else	{
		return( BnnSubtractBorrow( mm, ml-nl, BN_CARRY ) );
	}
}

/*
 *	Multiplication
 */

#define LOW(x) 		   (BigNumDigit)(x & ((BN_ONE<<(BN_DIGIT_SIZE / 2)) -1))
#define HIGH(x) 	   (BigNumDigit)(x >> (BN_DIGIT_SIZE / 2))
#define L2H(x) 		   (BigNumDigit)(x << (BN_DIGIT_SIZE / 2))
#define UPDATE_S(c,V,X3)   c += V; if (c < V) { X3++; }

BigNumCarry
BnnMultiplyDigit( BigNum pp, BigNumLength pl, const BigNum mm, BigNumLength ml, BigNumDigit d )
{
	/*
	 * Performs the product:
	 * Q = P + M * d
	 * BB = BBase(P)
	 * Q mod BB => P
	 * Q div BB => CarryOut
	 * Returns the CarryOut.
	 * Assumes Size(P) >= Size(M) + 1.
	 */

	BigNumLength	i;
	BigNumProduct	c = 0;
	BigNumDigit	save;

    	if( d == BN_ZERO ) {
		return( BN_NOCARRY );
	}

	if( d == BN_ONE ) {
		return( BnnAdd( pp, pl, mm, ml, BN_NOCARRY ) );
	}

	for( i = 0 ; i < ml ; ++i ) {
		BigNumDigit	Lm;
		BigNumDigit	Hm;
		BigNumDigit	Ld;
		BigNumDigit	Hd;
		BigNumDigit	X0;
		BigNumDigit	X1;
		BigNumDigit	X2;
		BigNumDigit	X3;

		Ld = LOW( d );
		Hd = HIGH( d );
		Lm = LOW( mm[ i ] );
		Hm = HIGH( mm[ i ] );
		X0 = Ld * Lm;
		X1 = Ld * Hm;
		X2 = Hd * Lm;
		X3 = Hd * Hm;

		UPDATE_S( c, X0,        X3 );
		UPDATE_S( c, L2H( X1 ), X3 );
		UPDATE_S( c, L2H( X2 ), X3 );
		UPDATE_S( c, *pp,       X3 );

		--pl;
		*(pp++) = (BigNumDigit)c;
		c = X3 + HIGH(X1) + HIGH(X2);
	}

	if( pl == 0 ) {
		return( BN_NOCARRY );
	}

	save	= *pp;
	c      += save;
	*pp	= (BigNumDigit)c;

	if( c >= save ) {
		return( BN_NOCARRY );
	}

	++pp;
	--pl;

	while( pl != 0 && (++(*pp++)) == 0 ) {
		pl--;
	}

	return( (pl != 0) ? BN_NOCARRY : BN_CARRY );
}

/*
 * Division
 */

/* xh:xl -= yh:yl */

#define SUB(xh, xl, yh, yl)					\
	if( yl > xl ) {						\
		xl -= yl;					\
		xh -= yh + 1;					\
	} else  {						\
		xl -= yl;					\
		xh -= yh;					\
	}

BigNumDigit
BnnDivideDigit( BigNum qq, BigNum nn, BigNumLength nl, BigNumDigit d )
{
	/*
	 * Performs the quotient: N div d => Q
	 * Returns R = N mod d
	 * Assumes leading digit of N < d, and d > 0.
	 */

	BigNumLength	k;
	BigNumLength	orig_nl;
	BigNumDigit	rh;	/* Two halves of current remainder */
	BigNumDigit 	rl;	/* Correspond to quad above	   */
	BigNumDigit	ph;
	BigNumDigit	pl;	/* product of c and qa		   */
	BigNumDigit 	ch;
	BigNumDigit	cl;
	BigNumDigit	prev_qq;

	/*
	 * Normalize divisor
	 */

	k = BnnNumLeadingZeroBitsInDigit( d );

	if( k != 0 ) {
		prev_qq = qq[ -1 ];
		orig_nl = nl;
		d <<= k;
		(void)BnnShiftLeft( nn, nl, k );
	} else	{
		prev_qq = 0;
		orig_nl = 0;
	}

	nn += nl;
	nl--;
	qq += nl;

	ch = HIGH( d );
	cl = LOW( d );

	/*
	 * At this point ch can't be == 0; d has been shifted by k
	 * (the number of leading 0).
	 */

	rl = *(--nn);

	while( nl-- != 0 ) {
		BigNumDigit qa; /* Current appr. to quotient */

		rh = rl;
		rl = *(--nn);
		qa = rh / ch; 	/* appr. quotient */

		/*
		 * Compute ph, pl
		 */

		pl = cl * qa;
		ph = ch * qa;
		ph += HIGH( pl );
		pl = L2H( pl );

		/*
		 * While ph:pl > rh:rl, decrement qa, adjust qh:ql
		 */

		while( (ph > rh) || ((ph == rh) && (pl > rl)) ) {
			qa--;
			SUB( ph, pl, ch, L2H( cl ) );
		}

		SUB( rh, rl, ph, pl );

		/*
		 * Top half of quotient is correct; save it
		 */

		*(--qq) = L2H( qa );
		qa = (L2H( rh ) | HIGH( rl )) / ch;

		/*
		 * Approx low half of q. Compute ph, pl, again
		 */

		pl = cl * qa;
		ph = ch * qa;
		ph += HIGH( pl );
		pl = LOW( pl ) | L2H( LOW( ph ) );
		ph = HIGH( ph );

		/*
		 * While ph:pl > rh:rl, decrement qa, adjust qh:ql
		 */

		while( (ph > rh) || ((ph == rh) && (pl > rl)) ) {
			qa--;
			SUB( ph, pl, 0, d );
		}

		/*
		 * Subtract ph:pl from rh:rl; we know rh will be 0
		 */

		rl -= pl;
		*qq |= qa;
	}

	/*
	 * Denormalize dividend
	 */

	if( k != 0 ) {
		if( (qq > nn) && (qq < &nn[orig_nl]) ) {
			/*
			 * Overlap between qq and nn. Care of *qq!
			 */
			orig_nl = (BigNumLength)(qq - nn);
			(void)BnnShiftRight( nn, orig_nl, k );
			nn[orig_nl - 1] = prev_qq;
		} else	if( qq == nn ) {
			(void)BnnShiftRight(&nn[orig_nl-1], (BigNumLength)1, k);
		} else	{
			(void)BnnShiftRight(nn, orig_nl, k);
		}
	}
	return( rl >> k );
}

BigNumBool
BnnIsZero( const BigNum nn, BigNumLength nl )
{
	/*
	 * Returns BN_TRUE iff N = 0
	 */

	if( (BnnNumDigits(nn, nl) == (BigNumLength)1)
	    && (nl == 0 || BnnIsDigitZero(*nn) != BN_FALSE) ) {
		return( BN_TRUE );
	} else	{
		return( BN_FALSE );
	}
}

BigNumCarry
BnnMultiply( BigNum pp, BigNumLength pl, const BigNum mm, BigNumLength ml, const BigNum nn, BigNumLength nl )
{
	/*
	 * Performs the product:
	 *    Q = P + M * N
	 *    BB = BBase(P)
	 *    Q mod BB => P
	 *    Q div BB => CarryOut
	 *
	 * Returns the CarryOut.
	 *
	 * Assumes:
	 *    Size(P) >= Size(M) + Size(N), 
	 *    Size(M) >= Size(N).
	 */

	BigNumLength i;
	BigNumCarry  c = BN_NOCARRY;

	/*
	 * Multiply one digit at a time
	 */

	for( i = 0 ; i < nl ; ++i ) {
		if(BnnMultiplyDigit(&pp[i], pl--, mm, ml, nn[i]) == BN_CARRY) {
			c = BN_CARRY;
		}
	}

	return( c );
}

static void
BnnDivideHelper( BigNum nn, BigNumLength nl, BigNum dd, BigNumLength dl )
{
	/*
	 * In-place division.
	 *
	 * Input (N has been EXTENDED by 1 PLACE; D is normalized):
	 *	+-----------------------------------------------+----+
	 *	|  			N			  EXT|
	 *	+-----------------------------------------------+----+
	 *
	 *	+-------------------------------+
	 *	|		D	       1|
	 *	+-------------------------------+
	 *
	 * Output (in place of N):
	 *	+-------------------------------+---------------+----+
	 *	|		R	 	|	   Q	     |
	 *	+-------------------------------+---------------+----+
	 *
	 * Assumes:
	 *    N > D
	 *    Size(N) > Size(D)
	 *    last digit of N < last digit of D
	 *    D is normalized (Base/2 <= last digit of D < Base)
	 */

   	BigNumDigit 	DDigit;
	BigNumDigit	BaseMinus1;
	BigNumDigit	QApp;
	BigNumLength	ni;

	/*
	 * Initialize constants
	 */

	/*
	 * BaseMinus1 = BN_COMPLEMENT;
	 * ->
	 *      BnnSetDigit( &BaseMinus1, BN_ZERO );
	 *      BnnComplement( &BaseMinus1, (BigNumLength)1 );
	 */

	BaseMinus1 = BN_COMPLEMENT;

	/*
	 * Save the most significant digit of D
	 */

	DDigit = BN_ZERO;
	BnnAssign( &DDigit, dd+dl-1, (BigNumLength)1 );

	/*
	 * Replace D by Base - D
	 */

	BnnComplement( dd, dl );
	(void)BnnAddCarry( dd, dl, BN_CARRY );

	/*
	 * For each digit of the divisor, from most significant to least:
	 */

	QApp = BN_ZERO;
	nl += 1;
	ni = nl-dl;

	while( ni != 0 ) {
		/*
		 * Compute the approximate quotient
		 */

		ni--;
		nl--;

		/*
		 * If first digits of numerator and denominator are the same,
		 */

		if( BnnCompareDigits( *(nn+nl), DDigit ) == BN_EQ ) {
			/*
			 * Use "Base - 1" for the approximate quotient
			 */
			BnnAssign( &QApp, &BaseMinus1, (BigNumLength)1 );
		} else	{
			/*
		 	 * Divide  the  first  2  digits  of N by the
		 	 * first digit of D
			 */
			(void)BnnDivideDigit( &QApp,
					      nn+nl-1,
					      (BigNumLength)2,
					      DDigit );
		}

		/*
		 * Compute the remainder
		 */

		(void)BnnMultiplyDigit( nn+ni, dl+1, dd, dl, QApp );

      		/*
		 * Correct the approximate quotient, in case it was too large
		 */

		while( BnnCompareDigits( *(nn + nl), QApp ) != BN_EQ ) {
			/*
			 * Subtract D from N
			 */

			(void)BnnSubtract(nn+ni, dl+1, dd, dl, BN_CARRY );

			/*
			 * Q -= 1
			 */

			(void)BnnSubtractBorrow( &QApp,
						 (BigNumLength)1,
						 BN_NOCARRY );
		}
	}

	/*
	 * Restore original D
	 */

	BnnComplement( dd, dl );
	(void)BnnAddCarry( dd, dl, BN_CARRY );
}

void
BnnDivide( BigNum nn, BigNumLength nl, BigNum dd, BigNumLength dl )
{
	/*
	 * Performs the quotient:
	 *    N div D => high-order bits of N, starting at N[dl]
	 *    N mod D => low-order dl bits of N
	 *
	 * Assumes 
	 *    Size(N) > Size(D),
	 *    last digit of N < last digit of D (if N > D).
	 */

	BigNumLength 	nshift;

	/*
	 * Take care of easy cases first
	 */

	switch( BnnCompare( nn, nl, dd, dl ) ) {
	case BN_LT:	/* n < d */
							 /* N => R */
		BnnSetToZero( nn+dl, nl-dl );		 /* 0 => Q */
		return;
	case BN_EQ:	/* n == d */
		BnnSetToZero( nn, nl );			 /* 0 => R */
		BnnSetDigit( nn+nl-1, BN_ONE );          /* 1 => Q */
		return;
	case BN_GT:	/* n > d */
		/*
		 * If divisor is just 1 digit, use a special divide
		 */

		if( dl == (BigNumLength)1 ) {
			/*
			 * note: nn+1 = nn+dl
			 */

			*nn = BnnDivideDigit( nn+1, nn, nl, *dd );

			/*
			 * Otherwise, divide one digit at a time
			 */
		} else	{
			/*
			 * Normalize
			 */

			nshift = BnnNumLeadingZeroBitsInDigit( *(dd+dl-1) );
			(void)BnnShiftLeft( dd, dl, nshift );
			(void)BnnShiftLeft( nn, nl, nshift );

			/*
			 * Divide
			 */

			BnnDivideHelper( nn, nl-1, dd, dl );

			/*
			 * Unnormalize
			 */

			(void)BnnShiftRight( dd, dl, nshift );
			(void)BnnShiftRight( nn, dl, nshift ); 

			/*
			 * note: unnormalize N <=> unnormalize R (with R < D)
			 */
		}
	}
}

BigNumCmp
BnnCompare( const BigNum mm, BigNumLength ml, const BigNum nn, BigNumLength nl )
{
	/*
	 * return
	 *	 	BN_GT 	iff M > N
	 *		BN_EQ	iff N = N
	 *		BN_LT	iff N < N
	 */

	ml = BnnNumDigits( mm, ml );
	nl = BnnNumDigits( nn, nl );

	if( ml != nl ) {
		return( ml > nl ? BN_GT : BN_LT );
	} else	{
		int	d;

		/*
		 * loop starting from most significant digit
		 */

		for( d = (int)(nl - 1) ; d >= 0 ; --d ) {
			if( mm[d] > nn[d] ) {
				return( BN_GT );
			}
			if( mm[d] < nn[d] ) {
				return( BN_LT );
			}
		}

		return( BN_EQ );
	}
}
