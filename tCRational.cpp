#if	!defined( lint )
static	const char rcsid[] = "$Id: tCRational.cpp,v 1.12 2013-03-31 09:14:38 jullien Exp $";
#endif

//
//	tCRational.cpp :	
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include "CRational.h"

using namespace rational;

int
main()
{
  (void)printf("Rational non-regression tests. (c) 2012-2013 C. Jullien\n");

  CRational q1(-4, 6);
  CRational q2(4, 6);
  CRational q3(5, 9);
  CRational q4("87384004098848212735349875629364987687687667720",
               "93495466257274887265487820984675290489826754805");
  CRational pi("22/7");
  CRational one(1);
  CRational zero(0);
  CRational x("1/6");
  CRational y("1/3");
  CRational z("-1/3");
  CRational err(5, 0);

  std::cout << z << std::endl;
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
//  std::cout << (q1 * -3) << std::endl;
  std::cout << ((q1 * q2) + q3) << std::endl;
  std::cout << (q1 - q3) << std::endl;
  std::cout << (q1 / q3) << std::endl;
  std::cout << (q3 - q1) << std::endl;
  std::cout << (q3 < q1) << std::endl;
  std::cout << (q3 > q1) << std::endl;
  std::cout << (q1 < zero) << std::endl;

  return( 0 );
}
