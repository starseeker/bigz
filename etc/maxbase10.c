/*
 * $Id: maxbase10.c,v 1.7 2012-01-01 19:02:39 jullien Exp $
 */

/*
 *	maxbase10.c : computes the max power of 10 that fits in a BigNumDigit.
 */

#include <stdio.h>
#include "../bigz.h"

static	void	BzMaxBase(int base, BigNumDigit* maxval, unsigned int* digits);

/*
 * Returns two values:
 * - maxval: the maximal value in base 'base' that can fit in a BigNumDigit
 * - digits: the number of digits in base 'base' that can be printed.
 */

static	void
BzMaxBase(int base, BigNumDigit* maxval, unsigned int* digits)
{
	BigNumDigit  i = (BigNumDigit)base;
	double 	     f = (double)base;
	unsigned int b = 1;

	for( b = 1 ; (double)(i * base) > (f * ((double)base - 0.01)) ; ++b ) {
		i *= base;
		f *= (double)base;
	}

	*maxval = i;
	*digits = b;
}

static	void
BzMaxBaseAlt( BigNumDigit base, BigNumDigit* maxval, unsigned int* digits )
{
	BigNumDigit  i;
	BigNumDigit  v = (BigNumDigit)1;
	unsigned int b = 0;

	for( i = (~(BigNumDigit)0) ; i > base ; i /= (BigNumDigit)base ) {
		++b;
		v *= base;
	}

	*maxval = v;
	*digits = b;
}

static	void
BzPrintMacros(int base, BigNumDigit maxval, unsigned int digits)
{
	if( base == 10 ) {
#if	defined( _WIN64 )
		printf("#define BZ_MAX_BASE%02d\t\t%I64u\n", base, maxval );
#else
		printf("#define BZ_MAX_BASE%02d\t\t%u\n", base, maxval );
#endif
	} else	{
#if	defined( _WIN64 )
		printf("#define BZ_MAX_BASE%02d\t\t0x%0I64x\n", base, maxval );
#else
		printf("#define BZ_MAX_BASE%02d\t\t0x%x\n", base, maxval );
#endif
	}
	printf("#define BZ_MAX_BASE%02d_DIGITS\t%d\n", base, digits );
}

int
main(void)
{
	BigNumDigit  maxval = (BigNumDigit)0;
	unsigned int digits = (unsigned int)0;
	BigNumDigit  maxvalalt = (BigNumDigit)0;
	unsigned int digitsalt = (unsigned int)0;
	int	i;

	for( i = 2 ; i <= 36 ; ++i ) {
		BzMaxBase(i, &maxval, &digits);
//		BzPrintMacros(i, maxval, digits);
		BzMaxBaseAlt(i, &maxvalalt, &digitsalt);
		BzPrintMacros(i, maxval, digits);
		if( maxval != maxvalalt || digits != digitsalt ) {
			BzPrintMacros(i, maxval, digits);
			BzPrintMacros(i, maxvalalt, digitsalt);
		}
	}

	return( 0 );
}
