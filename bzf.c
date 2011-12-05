/*
static	const char rcsid[] = "$Id: bzf.c,v 1.4 2011-12-05 06:54:38 jullien Exp $";
*/

/*
 *	bzf.c :	 Miscellaneous functions built on top of BigZ.
 */

extern	BigZ	BzFactorial(BigZ z);

BigZ
BzFactorial( BigZ z )
{
	/*
	 * Returns Z!
	 * Assumes Z < Base.
	 */

	BigZ		f;
	BigNumDigit	zval;
	int		fl = 1;

	zval = BnnGetDigit( BzToBn( z ) );
	f = BzCreate( zval+1 );
	BnnSetDigit( BzToBn( f ), 1);
	BzSetSign( f, BzGetSign( z ) );

	while( zval-- > 1 ) {
		BnnMultiplyDigit( BzToBn( f ), fl+1, BzToBn( f ), fl, zval );
		fl = BnnNumDigits( BzToBn( f ), fl+1);
	}
    
	return( f );
}
