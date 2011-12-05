#if	!defined( lint )
static	const char rcsid[] = "$Id: CBignum.cpp,v 1.3 2010-07-21 06:21:56 C_JULLIEN Exp $";
#endif

//
//	CBignum.cpp :	
//

#include "CBignum.h"

const CBignum
CBignum::operator++( int ) {
	CBignum	bn( *this );
	*this += BzOne;
	return( bn );
}

const CBignum
CBignum::operator--( int ) {
	CBignum	bn( *this );
	*this -= BzOne;
	return( bn );
}
