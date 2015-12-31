//
// $Id: CBignum.h,v 1.41 2015/12/29 15:08:10 jullien Exp $
//

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

//
//      CBignum.h :
//

#if     !defined(__CBIGNUM_H)
#define __CBIGNUM_H

#if __cplusplus >= 201103L
#define BN_CPP11
#endif

#include <stdlib.h>
#include <iostream>
#include <string>
#include <utility>
#include "bigz.h"

namespace rational {
class CRational;
}

namespace bignum {
class CBignum {
  friend class rational::CRational;
 public:
  CBignum() : m_bz(BzFromInteger(0)) {}
  template<typename T>
  CBignum(T init) : m_bz(signedType<T>()
			 ? BzFromUnsignedInteger(static_cast<BzUInt>(init))
			 : BzFromInteger(static_cast<BzInt>(init))) {
  }
  CBignum(const CBignum& rhs) : m_bz(BzCopy(rhs.m_bz)) {}
  CBignum(const rational::CRational& rhs);
#if defined(BN_CPP11)
  CBignum(CBignum&& rhs) : m_bz(rhs.m_bz) { rhs.m_bz = 0; }
  // Thanks to C++11, allows nnnnnn_BN syntax
  friend CBignum operator"" _BN(const char* init) { return CBignum(init); }
#endif
  CBignum(const char* init, unsigned int base = 10)
    : m_bz(BzFromString(init, base, BZ_UNTIL_END)) {}
  explicit CBignum(bool b) : m_bz(BzFromInteger(b ? 1 : 0)) {}
  ~CBignum() {
    if (m_bz) {
      BzFree(m_bz);
    }
  }

  // convertions

  operator BzInt() const throw() {
    return BzToInteger(m_bz);
  }
  operator BzUInt() const throw() {
    return BzToUnsignedInteger(m_bz);
  }
  operator bool() const throw() {
    return BzGetSign(m_bz) != BZ_ZERO;
  }
  operator std::string() const throw();

 public:
  // unary +, -, ++, --

  friend CBignum operator+(const CBignum& bz1) {
    return CBignum(BzNegate(bz1.m_bz), ASSIGN);
  }
  friend CBignum operator-(const CBignum& bz1) {
    return CBignum(BzNegate(bz1.m_bz), ASSIGN);
  }

  inline CBignum&       operator++();
         CBignum        operator++(int);
  inline CBignum&       operator--();
         CBignum        operator--(int);

  // binary +, - *, /, %

  friend CBignum operator+(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzAdd(bz1.m_bz, bz2.m_bz), ASSIGN);
  }

  template<typename T>
  friend CBignum operator+(const CBignum& bz1, T i) {
    return (bz1 + CBignum(i));
  }

  template<typename T>
  friend CBignum operator+(T i, const CBignum& bz1) {
    return (bz1 + CBignum(i));
  }

  friend CBignum operator-(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzSubtract(bz1.m_bz, bz2.m_bz), ASSIGN);
  }
  template<typename T>
  friend CBignum operator-(const CBignum& bz1, T i) {
    return (bz1 - CBignum(i));
  }
  template<typename T>
  friend CBignum operator-(T i, const CBignum& bz1) {
    return (CBignum(i) - bz1);
  }

  friend CBignum operator*(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzMultiply(bz1.m_bz, bz2.m_bz), ASSIGN);
  }
  template<typename T>
  friend CBignum operator*(const CBignum& bz1, T i) {
    return (bz1 * CBignum(i));
  }
  template<typename T>
  friend CBignum operator*(T i, const CBignum& bz1) {
    return (CBignum(i) * bz1);
  }

  friend CBignum operator/(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzDiv(bz1.m_bz, bz2.m_bz), ASSIGN);
  }
  template<typename T>
  friend CBignum operator/(const CBignum& bz1, T i) {
    return (bz1 / CBignum(i));
  }
  template<typename T>
  friend CBignum operator/(T i, const CBignum& bz1) {
    return (CBignum(i) / bz1);
  }

  friend CBignum operator%(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzMod(bz1.m_bz, bz2.m_bz), ASSIGN);
  }
  template<typename T>
  friend CBignum operator%(const CBignum& bz1, T i) {
    return (bz1 % CBignum(i));
  }
  template<typename T>
  friend CBignum operator%(T i, const CBignum& bz1) {
    return (CBignum(i) % bz1);
  }

  // comparisons

  friend bool operator==(const CBignum& bz1, const CBignum& bz2) {
    return BzCompare(bz1.m_bz, bz2.m_bz) == BZ_EQ;
  }
  template<typename T>
  friend bool operator==(const CBignum& bz1, T i) {
    return (bz1 == CBignum(i));
  }
  template<typename T>
  friend bool operator==(T i, const CBignum& bz1) {
    return (CBignum(i) == bz1);
  }
  friend bool operator!=(const CBignum& bz1, const CBignum& bz2) {
    return !(bz1 == bz2);
  }

  friend bool operator>(const CBignum& bz1, const CBignum& bz2) {
    return BzCompare(bz1.m_bz, bz2.m_bz) == BZ_GT;
  }
  template<typename T>
  friend bool operator>(const CBignum& bz1, T i) {
    return (bz1 > CBignum(i));
  }
  template<typename T>
  friend bool operator>(T i, const CBignum& bz1) {
    return (CBignum(i) > bz1);
  }
  friend bool operator<=(const CBignum& bz1, const CBignum& bz2) {
    return !(bz1 > bz2);
  }

  friend bool operator<(const CBignum& bz1, const CBignum& bz2) {
    return BzCompare(bz1.m_bz, bz2.m_bz) == BZ_LT;
  }
  template<typename T>
  friend bool operator<(const CBignum& bz1, T i) {
    return (bz1 < CBignum(i));
  }
  template<typename T>
  friend bool operator<(T i, const CBignum& bz1) {
    return (CBignum(i) < bz1);
  }
  friend bool operator>=(const CBignum& bz1, const CBignum& bz2) {
    return !(bz1 < bz2);
  }

  // logical operators

  friend CBignum operator&(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzAnd(bz1.m_bz, bz2.m_bz), ASSIGN);
  }
  template<typename T>
  friend CBignum operator&(const CBignum& bz1, T i) {
    return (bz1 & CBignum(i));
  }
  template<typename T>
  friend CBignum operator&(T i, const CBignum& bz1) {
    return (CBignum(i) & bz1);
  }

  friend CBignum operator|(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzOr(bz1.m_bz, bz2.m_bz), ASSIGN);
  }
  template<typename T>
  friend CBignum operator|(const CBignum& bz1, T i) {
    return (bz1 | CBignum(i));
  }
  template<typename T>
  friend CBignum operator|(T i, const CBignum& bz1) {
    return (CBignum(i) | bz1);
  }

  friend CBignum operator^(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzXor(bz1.m_bz, bz2.m_bz), ASSIGN);
  }
  template<typename T>
  friend CBignum operator^(const CBignum& bz1, T i) {
    return (bz1 ^ CBignum(i));
  }
  template<typename T>
  friend CBignum operator^(T i, const CBignum& bz1) {
    return (CBignum(i) ^ bz1);
  }

  CBignum operator~ () {
    return CBignum(BzNot(m_bz), ASSIGN);
  }

  friend bool logbitp(unsigned int bitnb, const CBignum& bz1) {
    return BzTestBit(bitnb, bz1.m_bz) == BN_TRUE;
  }

  friend bool logbitp(const CBignum& bitnb, const CBignum& bz1) {
    const BzUInt bn(bitnb);
    return BzTestBit(static_cast<unsigned int>(bn), bz1.m_bz) == BN_TRUE;
  }

  // shifts

  friend CBignum operator<<(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzAsh(bz1.m_bz,
                         static_cast<int>(BzToInteger(bz2.m_bz))),
		   ASSIGN);
  }
  template<typename T>
  friend CBignum operator<<(const CBignum& bz1, T i) {
    return (bz1 << CBignum(i));
  }
  template<typename T>
  friend CBignum operator<<(T i, const CBignum& bz1) {
    return (CBignum(i) << bz1);
  }

  friend CBignum operator>>(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzAsh(bz1.m_bz,
                         static_cast<int>(BzToInteger(bz2.m_bz))),
		   ASSIGN);
  }
  template<typename T>
  friend CBignum operator>>(const CBignum& bz1, T i) {
    return (bz1 >> CBignum(i));
  }
  template<typename T>
  friend CBignum operator>>(T i, const CBignum& bz1) {
    return (CBignum(i) >> bz1);
  }

  // general functions

  friend CBignum floor(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzFloor(bz1.m_bz, bz2.m_bz), ASSIGN);
  }
  template<typename T>
  friend CBignum floor(T i, const CBignum& bz2) {
    return floor(CBignum(i), bz2);
  }
  template<typename T>
  friend CBignum floor(const CBignum& bz1, T i) {
    return floor(bz1, CBignum(i));
  }

  friend CBignum ceiling(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzCeiling(bz1.m_bz, bz2.m_bz), ASSIGN);
  }
  template<typename T>
  friend CBignum ceiling(T i, const CBignum& bz2) {
    return ceiling(CBignum(i), bz2);
  }
  template<typename T>
  friend CBignum ceiling(const CBignum& bz1, T i) {
    return ceiling(bz1, CBignum(i));
  }

  friend CBignum round(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzRound(bz1.m_bz, bz2.m_bz), ASSIGN);
  }
  template<typename T>
  friend CBignum round(T i, const CBignum& bz2) {
    return round(CBignum(i), bz2);
  }
  template<typename T>
  friend CBignum round(const CBignum& bz1, T i) {
    return round(bz1, CBignum(i));
  }

  friend CBignum gcd(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzGcd(bz1.m_bz, bz2.m_bz), ASSIGN);
  }
  template<typename T>
  friend CBignum gcd(T i, const CBignum& bz2) {
    return gcd(CBignum(i), bz2);
  }
  template<typename T>
  friend CBignum gcd(const CBignum& bz1, T i) {
    return gcd(bz1, CBignum(i));
  }

  friend CBignum lcm(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzLcm(bz1.m_bz, bz2.m_bz), ASSIGN);
  }
  template<typename T>
  friend CBignum lcm(T i, const CBignum& bz2) {
    return lcm(CBignum(i), bz2);
  }
  template<typename T>
  friend CBignum lcm(const CBignum& bz1, T i) {
    return lcm(bz1, CBignum(i));
  }

  friend CBignum pow(const CBignum& bz, const CBignum& exp) {
    BzInt i = BzToInteger(exp.m_bz);
    if (i < 0) {
      // consider 0 as an error.
      return BzFromInteger(0);
    } else {
      return CBignum(BzPow(bz.m_bz, BzToInteger(exp.m_bz)), ASSIGN);
    }
  }
  friend CBignum pow(const CBignum& bz, unsigned int exp) {
    return CBignum(BzPow(bz.m_bz, exp), ASSIGN);
  }
  template<typename T>
  friend CBignum pow(T base, unsigned int exp) {
    return CBignum(BzPow(CBignum(base), exp));
  }

  friend CBignum isqrt(const CBignum& bz) {
    return CBignum(BzSqrt(bz.m_bz), ASSIGN);
  }

  template<typename T>
  friend CBignum isqrt(T i) {
    return isqrt(CBignum(i));
  }

  friend bool evenp(const CBignum& bz) {
    return (BzIsEven(bz.m_bz) == BN_TRUE);
  }

  friend bool evenp(int i) {
    return ((i % 2) == 0);
  }

  friend bool oddp(const CBignum& bz) {
    return (BzIsEven(bz.m_bz) == BN_FALSE);
  }

  friend bool oddp(int i) {
    return ((i % 2) == 1);
  }

  size_t length() const throw() {
    return BzLength(m_bz);
  }

  friend size_t length(const CBignum& bz) {
    return bz.length();
  }

  template<typename T>
  friend size_t length(T i) {
    const CBignum tmp(i);
    return tmp.length();
  }

  friend CBignum abs(const CBignum& bz) {
    return CBignum(BzAbs(bz.m_bz), ASSIGN);
  }

  friend CBignum random(const CBignum& bz, unsigned int* seed) {
    return CBignum(BzRandom(bz.m_bz, reinterpret_cast<BzSeed*>(&seed)));
  }

  // assignments

  CBignum& operator=(const CBignum& rhs) {
    if (this != &rhs) {
      BzFree(m_bz);
      m_bz = BzCopy(rhs.m_bz);
    }
    return *this;
  }

#if defined(BN_CPP11)
  // Move assignment
  CBignum& operator=(CBignum&& rhs) {
    if (this != &rhs) {
      BzFree(m_bz);
      m_bz = rhs.m_bz;
      rhs.m_bz = 0;
    }
    return *this;
  }
#endif

  template<typename T>
  CBignum& operator=(T i) {
    BzFree(m_bz);
    *this = CBignum(i);
    return *this;
  }

  CBignum& operator+=(const CBignum& rhs) {
    return replace(BzAdd(m_bz, rhs.m_bz));
  }
  CBignum& operator-=(const CBignum& rhs) {
    return replace(BzSubtract(m_bz, rhs.m_bz));
  }
  CBignum& operator*=(const CBignum& rhs) {
    return replace(BzMultiply(m_bz, rhs.m_bz));
  }
  CBignum& operator/=(const CBignum& rhs) {
    return replace(BzDiv(m_bz, rhs.m_bz));
  }
  CBignum& operator%=(const CBignum& rhs) {
    return replace(BzMod(m_bz, rhs.m_bz));
  }
  CBignum& operator<<=(int i) {
    return replace(BzAsh(m_bz, i));
  }
  CBignum& operator>>=(int i) {
    return replace(BzAsh(m_bz, -i));
  }

  // output

  friend std::ostream& operator<<(std::ostream& os, const CBignum& bn);

  // version
  static const char *version() { return BzVersion(); }

 private:
  template<typename T>
  inline static bool
  signedType() {
    return T(~0) < T(0);
  }

  enum Flags { ASSIGN };

  operator BigZ() const throw() {
    return m_bz;
  }

  CBignum(const BigZ init) : m_bz(BzCopy(init)) {}

  CBignum& replace(BigZ bz) {
    BzFree(m_bz);
    m_bz = bz;
    return *this;
  }

  CBignum(const BigZ init, Flags) : m_bz(init) {}

  BigZ m_bz;
};

extern const CBignum one;
extern const CBignum two;

inline CBignum&
CBignum::operator++() {
  *this += one;
  return *this;
}

inline CBignum&
CBignum::operator--() {
  *this -= one;
  return *this;
}
} /* namespace bignum */
#endif  /* __CBIGNUM_H */
