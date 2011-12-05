#if	!defined( lint )
static	char *sccsid = "@(#)hextable.c	(c) C. Jullien 1999/01/01";
#endif

/*
 *	hextable.c :	
 */

#include <stdio.h>

int
main( argc, argv )
int	argc;
char	*argv[];
{
	int	i;
	int	tab[128];

	for( i = 0 ; i < 128 ; i++ )
		tab[i] = 0;
		
	for( i = '0' ; i <= '9' ; i++ )
		tab[i] = i - '0';
		
	for( i = 'a' ; i <= 'z' ; i++ )
		tab[i] = i - 'a' + 10;
		
	for( i = 'A' ; i <= 'Z' ; i++ )
		tab[i] = i - 'A' + 10;
		

	(void)printf( "static BigNumDigit BigHexToDigit[] = {" );

	for( i = 0 ; i < 128 ; i++ ) {
		if( (i % 16) == 0 )
			printf( "\n\t" );
		(void)printf( "%2d%c ", tab[i], ((i < 127) ? ',' : ' ') );
	}

	(void)printf( "\n};" );

	return( 0 );
}
