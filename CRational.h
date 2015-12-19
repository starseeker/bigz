//
// $Id: CRational.h,v 1.31 2015/12/19 08:15:22 jullien Exp $
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
//      CRational.h :
//

#if     !defined(__CRATIONAL_H)
#define __CRATIONAL_H_H

#if __cplusplus >= 201103L
#define BN_CPP11
#endif

#include <stdlib.h>
#include <ostream>
#include <bigq.h>
#include <string>
#include "CBignum.h"

namespace rational {

using bignum::CBignum;

class CRational {
 private:
  enum Flags { ASSIGN };

 public:
  explicit CRational(const CBignum& n = 0)
    : m_q(BqCreate(n, bignum::one)) {
  }
  CRational(const CBignum& n, const CBignum& d)
    : m_q(BqCreate(n, d)) {
  }
#if 0
  explicit CRational(int n)
     : m_q(BqCreate(CBignum(n), bignum::one))) {
  }
#endif
  CRational(const CRational& q)
     : m_q(BqCreate(BqGetNumerator(q.m_q), BqGetDenominator(q.m_q))) {
  }
#if defined(BN_CPP11)
  CRational(CRational&& rhs)
    : m_q(rhs.m_q) {
    rhs.m_q = 0;
  }
#endif
  explicit CRational(const char* s)
    : m_q(BqFromString(static_cast<const BzChar *>(s), 10)) {
  }
#if     defined(HAVE_BQ_FROM_DOUBLE)
  explicit CRational(double n)
    : m_q(BqFromDouble(n)) {
  }
#endif
  ~CRational() {
    if (m_q) {
      BqDelete(m_q);
    }
  }

  const CBignum numerator() const {
    return BqGetNumerator(m_q);
  }

  friend CBignum numerator(const CRational& q) {
    return q.numerator();
  }

  const CBignum denominator() const {
    return BqGetDenominator(m_q);
  }

  friend CBignum denominator(const CRational& q) {
    return q.denominator();
  }

  CRational& operator=(const CRational& rhs) {
    BqDelete(m_q);
    m_q = BqCreate(rhs.numerator(), rhs.denominator());
    return *this;
  }

#if defined(BN_CPP11)
  CRational& operator=(CRational&& rhs) {
    BqDelete(m_q);
    m_q = rhs.m_q;
    rhs.m_q = 0;
    return *this;
  }
#endif

  // convertions

  operator double () const throw() {
    return BqToDouble(m_q);
  }
  operator std::string () const throw();
  operator CBignum () const throw() {
    return numerator() / denominator();
  }

  // unary -

  friend CRational operator-(const CRational& q) {
    return CRational(BqNegate(q.m_q), ASSIGN);
  }

  // unary ++, --

  inline CRational&     operator++();
         CRational      operator++(int);
  inline CRational&     operator--();
         CRational      operator--(int);

  // binary +

  friend CRational operator+(const CRational& q1, const CRational& q2) {
    return CRational(BqAdd(q1.m_q, q2.m_q), ASSIGN);
  }

  // binary -

  friend CRational operator-(const CRational& q1, const CRational& q2) {
    return CRational(BqSubtract(q1.m_q, q2.m_q), ASSIGN);
  }

  // binary *

  friend CRational operator*(const CRational& q1, const CRational& q2) {
    return CRational(BqMultiply(q1.m_q, q2.m_q), ASSIGN);
  }

  // binary /

  friend CRational operator/(const CRational& q1, const CRational& q2) {
    return CRational(BqDiv(q1.m_q, q2.m_q), ASSIGN);
  }

  // comparisons

  friend bool operator==(const CRational& q1, const CRational& q2) {
    return BqCompare(q1.m_q, q2.m_q) == BQ_EQ;
  }
  friend bool operator==(const CRational& q, const CBignum& bn) {
    return (q == CRational(bn));
  }
  friend bool operator==(const CBignum& bn, const CRational& q) {
    return (CRational(bn) == q);
  }

  friend bool operator!=(const CRational& q1, const CRational& q2) {
    return !(q1 == q2);
  }

  friend bool operator>(const CRational& q1, const CRational& q2) {
    return BqCompare(q1.m_q, q2.m_q) == BQ_GT;
  }
  friend bool operator>(const CRational& q, const CBignum& bn) {
    return (q == CRational(bn));
  }
  friend bool operator>(const CBignum& bn, const CRational& q) {
    return (CRational(bn) == q);
  }
  friend bool operator<=(const CRational& a, const CRational& b) {
    return !(a > b);
  }

  friend bool operator<(const CRational& q1, const CRational& q2) {
    return BqCompare(q1.m_q, q2.m_q) == BQ_LT;
  }
  friend bool operator<(const CRational& q, const CBignum& bn) {
    return (q == CRational(bn));
  }
  friend bool operator<(const CBignum& bn, const CRational& q) {
    return (CRational(bn) == q);
  }
  friend bool operator>=(const CRational& a, const CRational& b) {
    return !(a < b);
  }

  friend CRational abs(const CRational& q) {
    return CRational(BqAbs(q.m_q), ASSIGN);
  }
  friend CRational inverse(const CRational& q) {
    return CRational(BqInverse(q.m_q), ASSIGN);
  }

  // output
  friend std::ostream& operator<<(std::ostream& os, const CRational& q);

 private:
  BigQ m_q;
  CRational(const BigQ init, Flags) : m_q(init) {}
};

inline CRational&
CRational::operator++() {
  *this = *this + CRational(bignum::one);
  return *this;
}

inline CRational&
CRational::operator--() {
  *this = *this - CRational(bignum::one);
  return *this;
}
} /* namespace rational */
#endif  /* __CRATIONAL_H */
