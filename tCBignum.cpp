#if     !defined( lint )
static  const char rcsid[] = "$Id: tCBignum.cpp,v 1.21 2014/02/23 06:42:31 jullien Exp $";
#endif

//
//      tCBignum.cpp :  
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <iomanip>
#ifdef WIN32
#include <crtdbg.h>
#endif
#include "CBignum.h"
#include "CRational.h"

using bignum::CBignum;
using bignum::one;
using bignum::two;
using rational::CRational;

static int testcnt = 0;
static int failcnt = 0;

CBignum
fib(const CBignum& n) {
  if (n == one) {
    return one;
  } else if (n == two) {
    return one;
  } else {
    return fib(n - one) + fib(n - two);
  }
}

CBignum
square(const CBignum& bn) {
  return bn * bn;
}

CBignum
ffib(const CBignum& n) {
  if (n == one) {
    return one;
  } else if (n == two) {
    return one;
  } else {
    const CBignum ndiv2(n / two);

    if (oddp(n)) {
      return square(ffib(ndiv2))       + square(ffib(ndiv2 + one));
    } else {
      return square(ffib(ndiv2 + one)) - square(ffib(ndiv2 - one));
    }
  }
}

static void
checkResult(int count,
            const char* op,
            const std::string& expected,
            const std::string& res) {
  ++testcnt;

  if (res != expected) {
    (void)printf("test %3d (%s) fails: expected = %16s, computed = %16s\n",
                 count, op, res.c_str(), expected.c_str());
    ++failcnt;
  }
}

void
Tz(int count, const char* op, unsigned int n, const char* expected) {
 checkResult(count, op, CBignum(n), expected);
}

void
Tz(int count, const char* op, const CBignum& n, const char* expected) {
  checkResult(count, op, n, expected);
}

void
Tz(int count, const char* op, const CRational& n, const char *expected) {
  checkResult(count, op, n, expected);
}

void
Tz(int count, const char* op, bool b, const char *res) {
  checkResult(count, op, (b ? "1" : "0"), res);
}

const char* X1;
const char* X2;
const char* Y1;
const char* Y2;

#include "tCBignum.dat"

int
main()
{
#if     defined( _WINDEBUG )
   _CrtMemState state;
   int          res;

  res = _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG);
  res |= _CRTDBG_ALLOC_MEM_DF;
  res |= _CRTDBG_CHECK_ALWAYS_DF;
  res |= _CRTDBG_DELAY_FREE_MEM_DF;
  res |= _CRTDBG_LEAK_CHECK_DF;
  _CrtSetDbgFlag(res);

  /*
   * Send all reports to stdout.
   */

  _CrtSetReportMode( _CRT_WARN,   _CRTDBG_MODE_FILE   );
  _CrtSetReportFile( _CRT_WARN,   _CRTDBG_FILE_STDOUT );
  _CrtSetReportMode( _CRT_ERROR,  _CRTDBG_MODE_FILE   );
  _CrtSetReportFile( _CRT_ERROR,  _CRTDBG_FILE_STDOUT );
  _CrtSetReportMode( _CRT_ASSERT, _CRTDBG_MODE_FILE   );
  _CrtSetReportFile( _CRT_ASSERT, _CRTDBG_FILE_STDOUT );

  _CrtMemCheckpoint( &state );
  { // Force a new block to let dtor do cleanups.
#endif

    (void)printf("Bignum non-regression tests. (c) 1998-2014 C. Jullien\n");
    (void)printf("Testing version %s ...\n\n", CBignum::version());

    CBignum x1(ffib(100));      /* 354224848179261915075        */
    CBignum x2(2);              /* 2                            */
    CBignum x3;                 /* 0                            */
    CRational r1("0123456789/333");

    CRational q1(-4, 6);
    CRational q2(4, 6);
    CRational q3(5, 9);
    CRational q4("87384004098848212735349875629364987687687667720",
                 "9349546625727488726548782098467529048982675");
    CRational q5(1, 3);

    CRational pi("22/7");
    CRational rOne(1);
    CRational rZero(0);
    CBignum bn = q4;

    // Check pi to double conversion
    double dpi = static_cast<double>(pi);
    if (dpi < 3.1428 || dpi > 3.1429) {
      std::cerr << dpi << " != 3.14286" << std::endl;
    }

    Tz(   1, "++",   ++x2,             "3"                            );
    Tz(   2, "--",   --x1,             "354224848179261915074"        );
    Tz(   3, "*=",   x1 *= x2,         "1062674544537785745222"       );
    Tz(   4, "-=",   x1 -= x2,         "1062674544537785745219"       );
    Tz(   5, "+=",   x1 += 3,          "1062674544537785745222"       );
    Tz(   6, "ctor", x3,               "0"                            );
    Tz(   7, "=",    x3 = x1,          "1062674544537785745222"       );
    Tz(   8, "/=",   x3 /= x2+2,       "212534908907557149044"        );
    Tz(   9, "%=",   x3 %= 19,         "13"                           );
    Tz(  10, "exp",  x2 = x3*3+x1/2,   "531337272268892872650"        );
    // C++11 move operator
    Tz(  11, "mov",  x2 = x1+x2+x3,    "1594011816806678617885"       );
    Tz(  12, "nan",  CRational(5, 0),  "#.QNaN"                       );
    Tz(  12, "q->z", bn == 9346,       "1"                            );

    tests();

#if 1
    CBignum y(1);
    y <<= 80;
    std::cout << std::setw(32) << std::setfill('.')
              << std::dec << y << std::endl;
    std::cout << std::setw(32) << std::setfill('.')
              << std::hex << y << std::endl;
    std::cout << std::setw(32) << std::setfill('.')
              << std::oct << y << std::endl;
    std::cout << std::setw(32) << std::setfill('.')
              << std::dec << r1 << std::endl;
#endif

    if (failcnt != 0) {
      (void)printf("%d tests made, fails %d.\n", testcnt, failcnt);
    } else  {
      (void)printf("%d tests made, Ok!.\n", testcnt);
    }

#if     defined( _WINDEBUG )
  }
  _CrtCheckMemory();
  _CrtDumpMemoryLeaks();
// _CrtMemDumpStatistics( &state );
  (void)state;
#endif
  return 0;
}
