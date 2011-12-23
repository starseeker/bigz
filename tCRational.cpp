#if	!defined( lint )
static	const char rcsid[] = "$Id: tCRational.cpp,v 1.7 2011-12-03 13:23:21 jullien Exp $";
#endif

//
//	tCRational.cpp :	
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include "CRational.h"

static int testcnt = 0;
static int failcnt = 0;

int
main()
{
	(void)printf("Rational non-regression tests. (c) 2012 C. Jullien\n");

	CRational q1(CBignum(-4), CBignum(6));
	CRational q2(CBignum(4), CBignum(-6));
	CRational q3(CBignum(5), CBignum(9));

	std::cout << q1 << std::endl;
	std::cout << q2 << std::endl;
	std::cout << (q1 + q2) << std::endl;
	std::cout << (q1 * q2) << std::endl;
	std::cout << (q1 * -3) << std::endl;
	std::cout << ((q1 * q2) + q3) << std::endl;
	std::cout << (q1 - q3) << std::endl;
	std::cout << (q1 / q3) << std::endl;
	std::cout << (q3 - q1) << std::endl;

	return( 0 );
}
