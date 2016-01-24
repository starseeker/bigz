/*
 * Simplified BSD License
 *
 * Copyright (c) 1988-1989, Digital Equipment Corporation & INRIA.
 * Copyright (c) 1992-2016, Eligis
 * All rights reserved.
 *
 * Redistribution and  use in  source and binary  forms, with  or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * o Redistributions  of  source  code must  retain  the  above copyright
 *   notice, this list of conditions and the following disclaimer.
 * o Redistributions  in  binary form  must reproduce the above copyright
 *   notice, this list of conditions and  the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE  IS PROVIDED BY  THE COPYRIGHT HOLDERS  AND CONTRIBUTORS
 * "AS  IS" AND  ANY EXPRESS  OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT
 * LIMITED TO, THE IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE  ARE DISCLAIMED. IN NO EVENT  SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL,  EXEMPLARY,  OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE  GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS  INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF  LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY,  OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING  IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * $Id: tCBignum.cpp,v 1.36 2016/01/24 09:13:41 jullien Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>
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

class Matrix {
 public:
  Matrix(const CBignum& n0,
         const CBignum& n1,
         const CBignum& n2,
         const CBignum& n3)
   : _n0(n0),
     _n1(n1),
     _n2(n2),
     _n3(n3) {
  }

  Matrix(const Matrix& rhs)
   : _n0(rhs._n0),
     _n1(rhs._n1),
     _n2(rhs._n2),
     _n3(rhs._n3) {
  }

  friend Matrix operator*(const Matrix& m1, const Matrix& m2) {
    return Matrix((m1._n0 * m2._n0) + (m1._n1 * m2._n2),
                  (m1._n0 * m2._n1) + (m1._n1 * m2._n3),
                  (m1._n2 * m2._n0) + (m1._n3 * m2._n2),
                  (m1._n2 * m2._n1) + (m1._n3 * m2._n3));
  }

  friend Matrix power(const Matrix& m, uint32_t n) {
    if (n == 0) {
      return Matrix(1, 0, 0, 1);
    } else if (n == 1) {
      return m;
    } else if ((n % 2) == 0) {
      return power((m * m), n / 2);
    } else {
      return m * power((m * m), (n - 1) / 2);
    }
  }

  inline const CBignum& operator[](int n) const {
    if (n == 0) {
      return _n0;
    } else if (n == 1) {
      return _n1;
    } else if (n == 2) {
      return _n2;
    } else {
      return _n3;
    }
  }

  const CBignum _n0;
  const CBignum _n1;
  const CBignum _n2;
  const CBignum _n3;
};

CBignum
fibm(uint32_t n) {
  if (n < 2) {
    return CBignum(1);
  } else {
    const Matrix& m = power(Matrix(1, 1, 1, 0), n-1);
    return m[0];
  }
}

static void
checkResult(int count,
            const char* op,
            const std::string& expected,
            const std::string& res) {
  ++testcnt;

  if (res != expected) {
   std::cout << res.length() << " V.S. " << expected.length() << std::endl;
    (void)printf("test %3d (%s) fails: expected = '%16s', computed = '%16s'\n",
                 count, op, res.c_str(), expected.c_str());
    ++failcnt;
  }
}

void
Tz(int count, const char* op, unsigned int n, const char* expected) {
 checkResult(count, op, CBignum(n), expected);
}

#if defined(_WIN64) || (defined(HAVE_STDINT_H) && (SIZEOF_VOID_P >= 8))
void
Tz(int count, const char* op, size_t n, const char* expected) {
 checkResult(count, op, CBignum(n), expected);
}
#endif

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

  _CrtSetReportMode(_CRT_WARN,   _CRTDBG_MODE_FILE);
  _CrtSetReportFile(_CRT_WARN,   _CRTDBG_FILE_STDOUT);
  _CrtSetReportMode(_CRT_ERROR,  _CRTDBG_MODE_FILE);
  _CrtSetReportFile(_CRT_ERROR,  _CRTDBG_FILE_STDOUT);
  _CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_FILE);
  _CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDOUT);

  _CrtMemCheckpoint(&state);
  {  // Force a new block to let dtor do cleanups.
#endif

    (void)printf("Bignum non-regression tests. (c) 1998-2016 C. Jullien\n");
    (void)printf("Testing version %s ...\n\n", CBignum::version());

    {
     CBignum bone(1);
     for (int i = 30; i < 70; ++ i) {
       std::cout << i << " => " << (bone << i) << std::endl;
     }
    }

    CBignum x1(ffib(100));      /* 354224848179261915075        */
    CBignum x2(2);              /* 2                            */
    CBignum x3;                 /* 0                            */

#if 0
//    std::cout << "0x100"_bn << std::endl;
    //  std::cout << 0100_bn << std::endl;
    std::cout << "100"_bn << std::endl;

    unsigned int seed = 0;
    std::cout << CBignum(1000, &seed) << std::endl;
    std::cout << CBignum(1000, &seed) << std::endl;
    std::cout << CBignum(1000, &seed) << std::endl;
#endif

    CRational r1("0123456789/333");

    CRational q1(-4, 6);
    CRational q2(4, 6);
    CRational q3(5, 9);
    CRational q4("87384004098848212735349875629364987687687667720",
                 "9349546625727488726548782098467529048982675");
    CRational q5(1, 3);
    CRational q6(1);

    CRational pi("355/113");
    CRational rOne(1);
    CRational rZero(0);
    CBignum bn = q4;
    // Check pi to double conversion
    double dpi = static_cast<double>(pi);
    if (dpi < 3.14159 || dpi > 3.14160) {
      std::cerr << dpi << " != 3.14286" << std::endl;
    }

    CBignum xxx = 1;
    xxx = xxx << 1000;
    double d(static_cast<double>(xxx));
    std::cout << xxx << std::endl;
    printf("double <<%200.30f>>\n", d);
    std::cout << static_cast<double>(xxx) << std::endl;

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
    Tz(  13, "fibm", fibm(128),        "251728825683549488150424261"  );
    tests();

    CBignum y(1);
    y <<= 80;

    // dec
    {
     std::ostringstream oss;
     oss << std::setw(32) << std::setfill('.') << std::showpos
         << std::dec << -y;
     checkResult(1, "<<", oss.str(), "......-1208925819614629174706176");
    }
    {
     std::ostringstream oss;
     oss << std::setw(32) << std::setfill('.')
         << std::showpos
         << std::dec << y;
     checkResult(1, "<<", oss.str(), "......+1208925819614629174706176");
    }

    // hex
    {
     std::ostringstream oss;
     oss << std::setw(32) << std::setfill('.')
         << std::hex << y;
     checkResult(1, "<<", oss.str(), "...........100000000000000000000");
    }
    {
     std::ostringstream oss;
     oss << std::setw(32) << std::setfill('.')
         << std::showpos << std::right
         << std::hex << std::showbase << y;
     checkResult(1, "<<", oss.str(), ".........0x100000000000000000000");
    }
    {
     std::ostringstream oss;
     oss << std::setw(32) << std::setfill('.')
         << std::hex << std::showbase << -y;
     checkResult(1, "<<", oss.str(), "........-0x100000000000000000000");
    }
    {
     std::ostringstream oss;
     oss << std::setw(32) << std::setfill('.')
         << std::showpos << std::left
         << std::hex << std::showbase << y << std::right;
     checkResult(1, "<<", oss.str(), "0x100000000000000000000.........");
    }
    // oct
    {
     std::ostringstream oss;
     oss << std::setw(32) << std::setfill('.') << std::showpos
         << std::oct << std::showbase << y;
     checkResult(1, "<<", oss.str(), "....0400000000000000000000000000");
    }
    // rational
    {
     std::ostringstream oss;
     oss << std::setw(32) << std::setfill('.')
         << std::dec << r1;
     checkResult(1, "<<", oss.str(), ".....................13717421/37");
    }

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
