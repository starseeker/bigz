#if	!defined( lint )
static	char *sccsid = "@(#)maxbase10.c	(c) C. Jullien 2010/06/08";
#endif

/*
 *	maxbase10.c : computes the max power of 10 that fits in a BigNumDigit.
 */

#include <stdio.h>

int
main(int argc, char *argv[])
{
	size_t i = 10;
	double f = (double)10;
	unsigned int b = 1;

	for( b = 1 ; (i * 10) > (f * 9.5) ; ++b ) {
		i *= 10;
		f *= 10;
	}

#if	defined( _WIN64 )
	printf("#define BZ_MAX_BASE10		%I64u\n", i );
#else
	printf("#define BZ_MAX_BASE10		%u\n", i );
#endif
	printf("#define BZ_MAX_BASE10_DIGITS	%d\n", b );

	return( 0 );
}
