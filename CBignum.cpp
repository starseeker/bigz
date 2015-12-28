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
 * $Id: CBignum.cpp,v 1.20 2015/12/28 13:47:04 jullien Exp $
 */

#include <string.h>
#include <stdio.h>
#include <string>
#include <ostream>
#include <iomanip>
#include "CBignum.h"

namespace bignum {

extern const CBignum one(1);
extern const CBignum two(2);

/*
 * compute string length with BzChar of any type.
 */
static size_t
stringLength(const BzChar* s) {
  size_t len;
  for (len = 0; *s != 0; ++s) {
    ++len;
  }
  return len;
}

CBignum::operator std::string () const throw() {
  const BzChar* s = BzToString(m_bz, 10, 0);
  std::string res(s);
  BzFreeString(const_cast<BzChar*>(s));
  return res;
}

std::ostream& operator<<(std::ostream& os, const CBignum& bn) {
  const BzChar* res;

  std::ios_base::fmtflags ioflags = os.flags();
  bool showBase = ((ioflags & std::ios::showbase) != 0);
  size_t len;
  size_t width = (size_t)os.width();
  os << std::setw(0);

  if (ioflags & std::ios::hex) {
    // hexadecimal output
    res = BzToString(bn.m_bz, 16, 0);
    len = stringLength(res) + (showBase ? 2 : 0);
    if ((len < width) && !(ioflags & std::ios::left)) {
     const std::string pad(width - len, os.fill());
     os << pad;
    }
    if (res[0] == '-') {
      os << "-";
    }
    if (showBase) {
      os << "0x";
    }
    if (res[0] == '-') {
      os << &res[1];
    } else {
      os << res;
    }
    if ((len < width) && (ioflags & std::ios::left)) {
      const std::string pad(width - len, os.fill());
      os << pad;
    }
  } else if (ioflags & std::ios::oct) {
    // octal output
    res = BzToString(bn.m_bz, 8, 0);
    len = stringLength(res) + (showBase ? 1 : 0);
    if ((len < width) && !(ioflags & std::ios::left)) {
      const std::string pad(width - len, os.fill());
      os << pad;
    }
    if (res[0] == '-') {
      os << "-";
    }
    if (showBase) {
      os << "0";
    }
    if (res[0] == '-') {
      os << &res[1];
    } else {
      os << res;
    }
    if ((len < width) && (ioflags & std::ios::left)) {
      const std::string pad(width - len, os.fill());
      os << pad;
    }
  } else {
    // decimal output
    res = BzToString(bn.m_bz, 10, (ioflags && std::ios::showpos));
    len = stringLength(res);
    if (len < width) {
      const std::string pad(width - len, os.fill());
      if (!(ioflags & std::ios::left)) {
        os << pad << res;
      } else {
        os << res << pad;
      }
    } else {
      os << res;
    }
  }
  BzFreeString(const_cast<BzChar*>(res));
  os.flags(ioflags);
  return os;
}

CBignum
CBignum::operator++(int) {
  CBignum bn(*this);
  *this += one;
  return bn;
}

CBignum
CBignum::operator--(int) {
  CBignum bn(*this);
  *this -= one;
  return bn;
}
} /* namespace bignum */
