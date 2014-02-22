#if	!defined( lint )
static	const char rcsid[] = "$Id: CBignum.cpp,v 1.10 2014/02/16 18:16:20 jullien Exp $";
#endif

/*
 * Simplified BSD License
 *
 * Copyright (c) 1988-1989, Digital Equipment Corporation & INRIA.
 * Copyright (c) 1992-2014, Eligis
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

//
//	CBignum.cpp :	
//

#include <string.h>
#include <stdio.h>
#include <string>
#include <ostream>
#include <iomanip>
#include "CBignum.h"

namespace bignum {

extern const CBignum one(1);
extern const CBignum two(2);

CBignum::operator std::string () const throw() {
 const char* s = BzToString(m_bz, 10, 0);
 std::string res(s);
 BzFreeString((void *)s);
 return res;
}

std::ostream& operator<<(std::ostream& os, const CBignum& bn) {
  const char* res;

  std::ios_base::fmtflags ioflags = os.flags();
  if (ioflags & std::ios::hex) {
    res = BzToString(bn.m_bz, 16, 0);
    size_t len = strlen(res) + 2;
    if (len < (size_t)os.width()) {
     const std::string pad((size_t)os.width() - len, os.fill());
     os << std::setw(0) << pad;
    }
    os << "0x";
  } else if (ioflags & std::ios::oct) {
    res = BzToString(bn.m_bz, 8, 0);
    size_t len = strlen(res) + 1;
    if (len < (size_t)os.width()) {
     const std::string pad((size_t)os.width() - len, os.fill());
     os << std::setw(0) << pad;
    }
    os << "0";
  } else {
    res = BzToString(bn.m_bz, 10, 0);
    size_t len = strlen(res);
    if (len < (size_t)os.width()) {
     const std::string pad((size_t)os.width() - len, os.fill());
     os << std::setw(0) << pad;
    }
  }
  os << res;
  BzFreeString((void *)res);
  return os;
}

const CBignum
CBignum::operator++(int) {
	CBignum	bn(*this);
	*this += one;
	return bn;
}

const CBignum
CBignum::operator--(int) {
	CBignum	bn(*this);
	*this -= one;
	return bn;
}
} // namespace bignum
