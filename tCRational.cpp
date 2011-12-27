#if	!defined( lint )
static	const char rcsid[] = "$Id: tCRational.cpp,v 1.8 2011-12-27 15:11:17 jullien Exp $";
#endif

//
//	tCRational.cpp :	
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include "CRational.h"

int
main()
{
	(void)printf("Rational non-regression tests. (c) 2012 C. Jullien\n");

	CRational q1(-4, 6);
	CRational q2(4, -6);
	CRational q3(5, 9);
	CRational q4("87384004098848212735349875629364987687687667720",
		     "93495466257274887265487820984675290489826754805");
	CRational q5(-4, -6);
	CRational pi("22/7");
	CRational one(1);
	CRational zero(0);
	CRational x("1/6");
	CRational y("1/3");
	CRational err(5, 0);

	std::cout << q5 << std::endl;
	std::cout << one << std::endl;
	std::cout << zero << std::endl;
	std::cout << -zero << std::endl;
	std::cout << err << std::endl;
	std::cout << pi << std::endl;
	std::cout << q1 << std::endl;
	std::cout << q4 << std::endl;
	std::cout << -q1 << std::endl;
	std::cout << q2 << std::endl;
	std::cout << abs(q2) << std::endl;
	std::cout << (q1 + q2) << std::endl;
	std::cout << (q1 * q2) << std::endl;
	std::cout << (q1 * -3) << std::endl;
	std::cout << ((q1 * q2) + q3) << std::endl;
	std::cout << (q1 - q3) << std::endl;
	std::cout << (q1 / q3) << std::endl;
	std::cout << (q3 - q1) << std::endl;
	std::cout << (q3 < q1) << std::endl;
	std::cout << (q3 > q1) << std::endl;
	std::cout << (q1 < zero) << std::endl;

	return( 0 );
}
