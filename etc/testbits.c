#if	!defined( lint )
static	char *sccsid = "@(#)testbits.c	(c) C. Jullien 1999/01/18";
#endif

/*
 *	testbits.c :	
 */

#include <stdio.h>

void
showbits( unsigned int n )
{
	int	j = 1;
	int	i;

	printf( "%16d %08x -> ", n, n );

	for( i = 31 ; i >= 0 ; i-- ) {
		printf( "%d", (n & (1 << i)) ? 1 : 0 );
	}
}

int
main( argc, argv )
int	argc;
char	*argv[];
{
	int	x = -45967;
	int	y = -3;

	printf( "x = %lx, y = %lx, x >> y = %lx\n", x, y, x >> y );

	printf( "\n" );
	showbits( x );
	printf( "\n" );
	showbits( ~x );
	printf( "\n" );

	return( 0 );
}
