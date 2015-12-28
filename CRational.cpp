/*
 * Simplified BSD License
 *
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
 * $Id: CRational.cpp,v 1.7 2015/12/19 15:03:01 jullien Exp $
 */

#include <string.h>
#include <stdio.h>
#include <ostream>
#include <iomanip>
#include <string>
#include "CRational.h"

namespace rational {

CRational::operator std::string () const throw() {
  char* s = BqToString(m_q, 0);
  std::string res(s);
  BzFreeString(s);
  return res;
}

/**
 * Print CRational only in base 10, honnor setw and setfill options.
 */
std::ostream& operator<<(std::ostream& os, const CRational& q) {
  BzChar* res = BqToString(q.m_q, 0);

  std::ios_base::fmtflags ioflags = os.flags();
  size_t len = strlen(res);

  size_t width = (size_t)os.width();
  os << std::setw(0);

  if (len < width) {
    const std::string pad(width - len, os.fill());
    if ((ioflags & std::ios::left) == 0) {
      os << pad << res;
    } else {
      os << res << pad;
    }
  } else {
    os << res;
  }

  BzFreeString(res);
  os.flags(ioflags);
  return os;
}

inline CRational
CRational::operator++(int) {
  CRational q(*this);
  *this = *this + CRational(bignum::one);
  return q;
}

inline CRational
CRational::operator--(int) {
  CRational q(*this);
  *this = *this - CRational(bignum::one);
  return q;
}
} /* namespace rational */

using bignum::CBignum;
CBignum::CBignum(const rational::CRational& rhs)
 : m_bz(0) {
  const CBignum tmp(rhs.numerator() / rhs.denominator());
  m_bz = BzCopy(tmp.m_bz);
}
