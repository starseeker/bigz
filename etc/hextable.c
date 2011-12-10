#if	!defined( lint )
static	char *sccsid = "@(#)hextable.c	(c) C. Jullien 1999/01/01";
#endif

/*
 *	hextable.c :	
 */

#include <stdio.h>
#include <ctype.h>

#define	ASCII_A		((char)0x61)

int
main( int argc, char *argv[] )
{
	const char *letter = "abcdefghijklmnopqrstuvwxyz";
	const char *digit  = "0123456789";
	const char *s;

	int	i;
	int	size;
	char *  tab;

	if( *letter == ASCII_A ) {
		size = 128;
	} else	{
		size = 256;
	}

	tab = (char *)malloc( size );

	for( i = 0 ; i < size ; ++i ) {
		tab[i] = 0;
	}
		
	i = 0;

	for( s = digit; *s != 0 ; ++s ) {
		tab[(int)*s] = i++;
	}

	for( s = letter; *s != 0 ; ++s ) {
		tab[(int)*s] = i;
		tab[(int)toupper(*s)] = i++;
	}

	if( size == 128 ) {
		(void)printf( "/*\n" );
		(void)printf( " * ANSI character class table\n");
		(void)printf( " */\n" );
	} else	{
		(void)printf( "/*\n" );
		(void)printf( " * EBCDIC character class table\n");
		(void)printf( " */\n" );
	}

	(void)printf( "static const BigNumDigit BigHexToDigit[] = {" );

	for( i = 0 ; i < size ; i++ ) {
		if( (i % 16) == 0 ) {
			printf( "\n\t" );
		}
		(void)printf( "%2d%c ", tab[i], ((i < (size-1)) ? ',' : ' ') );
	}

	(void)printf( "\n};\n\n" );

	(void)printf( "#define\tCTOI(c)\t((((unsigned int)c)<(unsigned int)%d)",
		      size - 1);
	(void)printf( "\t\\\n\t\t ? BigHexToDigit[(unsigned int)c]" );
	(void)printf( "\t\\\n\t\t : 0)\n" );

	free( tab );

	return( 0 );
}
