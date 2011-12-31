/*
 * $Id: maxbase10.c,v 1.6 2011-12-31 11:08:06 jullien Exp $
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
		printf("#define BZ_MAX_BASE%02d\t\t0x%I64x\n", base, maxval );
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
		

	BzMaxBase(2, &maxval, &digits);
	BzPrintMacros(2, maxval, digits);

	BzMaxBase(10, &maxval, &digits);
	BzPrintMacros(10, maxval, digits);

	BzMaxBase(16, &maxval, &digits);
	BzPrintMacros(16, maxval, digits);

	return( 0 );
}
