#if	!defined( lint )
static	const char rcsid[] = "$Id: tCRational.cpp,v 1.13 2013-04-01 08:23:31 jullien Exp $";
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

void
compare(const CRational& r1, const CRational& r2, const char* op, bool res) {
  bool cmp = false;

  if (strcmp(op, "==") == 0) {
    cmp = (r1 == r2);
  } else if (strcmp(op, "!=") == 0) {
    cmp = (r1 != r2);
  } else if (strcmp(op, ">") == 0) {
    cmp = (r1 > r2);
  } else if (strcmp(op, ">=") == 0) {
    cmp = (r1 >= r2);
  } else if (strcmp(op, "<") == 0) {
    cmp = (r1 < r2);
  } else if (strcmp(op, "<=") == 0) {
    cmp = (r1 <= r2);
  }

  if (cmp != res) {
   std::cerr << r1 << " " << op << " " << r2
             << " should be " << (res ? "true" : "false")
             << std::endl;
  }
}

void
check(const CRational& r, const CBignum& n, const CBignum& d) {
  if (r.numerator() != n || r.denominator() != d) {
    std::cerr << r << " != " << n << "/" << d << std::endl;
  }
}

int
main()
{
  (void)printf("Rational non-regression tests. (c) 2012-2013 C. Jullien\n");

  CRational q1(-4, 6);
  CRational q2(4, 6);
  CRational q3(5, 9);
  CRational q4("87384004098848212735349875629364987687687667720",
               "93495466257274887265487820984675290489826754805");
  CRational q5(1, 3);

  CRational pi("22/7");
  CRational one(1);
  CRational zero(0);
  CRational err(5, 0);

  check(CRational( 0 ),     0, 1);
  check(CRational( 1 ),     1, 1);
  check(CRational( -2 ),   -2, 1);
  check(CRational( 0,  1),  0, 1);
  check(CRational( 0,  9),  0, 1);
  check(CRational( 0, -9),  0, 1);
  check(CRational( 3,  9),  1, 3);
  check(CRational(-3,  9), -1, 3);
  check(CRational(-3, -9),  1, 3);
  check(CRational(-3, -9),  1, 3);

  check(CRational("3/9"),    1, 3);
  check(CRational("-3/ 9"), -1, 3);
  check(CRational("-3/-9"),  1, 3);
  check(CRational("-3/-9"),  1, 3);

  check(q1,          -2, 3);
  check(inverse(q1), -3, 2);
  check(abs(q1),      2, 3);
  check(abs(q2),      2, 3);
  check(q2 + q5,      1, 1);
  check(q1 - q5,     -1, 1);
  check(q1 + q2,      0, 1);
  check(q1 + q2,      0, 1);
  check((q1 * q2),   -4, 9);
  check((q2 * q2),    4, 9);
  check(q1 / q3,     -6, 5);
  check(q3 - q1,     11, 9);
  check(q1 * 3,      -2, 1);
  check(q2 * 3,       2, 1);
  check(q1 * zero,    0, 1);
  check(q2 * zero,    0, 1);

  std::cout << err << std::endl;
  compare(q3, q1, "<", false);
  compare(q3, q1, ">", true);
  compare(q3, q1, ">=", true);
  compare(q3, q3, ">=", true);
  compare(q1, q1, ">=", true);
  compare(q3, q1, "==", false);
  compare(q3, q1, "!=", true);

  return( 0 );
}
