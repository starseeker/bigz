//
// $Id: CBignum.h,v 1.54 2017/05/11 04:54:45 jullien Exp $
//

/*
 * Simplified BSD License
 *
 * Copyright (c) 1992-2017, Eligis
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

#if (__cplusplus >= 201103L) || (defined(_MSC_VER) && (_MSC_VER >= 1800))
#define BN_CPP11
#endif

#include <stdlib.h>
#include <iostream>
#include <string>
#include <utility>
#include <stdexcept>
#include "bigz.h"

namespace rational {
class CRational;
}

namespace bignum {
/**
 * Immutable arbitrary-precision integers. All operations behave as if
 * CBignum were represented in two's-complement notation (like
 * C/C++ integer types).
 */ 
class CBignum {
  friend class rational::CRational;

 public:
  /**
   * Make a default CBignum (0).
   */
  CBignum() : m_bz(BzFromInteger(0)) {}

  /**
   * Make a CBignum from an initial value.
   * @param [in] init initial value.
   * @tparam T type of initial value.
   */
  template<typename T>
  CBignum(T init)
    : m_bz(signedType<T>(init)
           ? BzFromInteger(static_cast<BzInt>(init))
           : BzFromUnsignedInteger(static_cast<BzUInt>(init))) {
  }
  /**
   * Constructs a randomly generated positive CBignum with the specified
   * bitLength.
   * @param [in] bitLength of the returned CBignum.
   * @param [in, out] seed source of random bits used.
   */
  CBignum(unsigned int bitLength, unsigned int* seed);

  /**
   * Copy-constructor
   * @param rhs CBignum to copy from.
   */
  CBignum(const CBignum& rhs) : m_bz(BzCopy(rhs.m_bz)) {}

  /**
   * Make a CBignum from a CRational.
   * @param rhs CRational to copy from.
   */
  CBignum(const rational::CRational& rhs);

#if defined(BN_CPP11)
  /**
   * Move-constructor
   * @param rhs CBignum to move from.
   */
  CBignum(CBignum&& rhs) noexcept : m_bz(rhs.m_bz) {
    // steal m_bz from rhs
    rhs.m_bz = 0;
  }

  /**
   * user-defined integer literals.
   * @param [in] init initial litteral CBignum string.
   */
  friend CBignum operator""_bn(const char* init, size_t) {
    return CBignum(init);
  }
#endif

  /**
   * Translates the String representation of a CBignum in the specified base
   * into a CBignum.
   * @param [in] init string representation of a CBignum.
   * @param [in] base integer base.
   */
  CBignum(const char* init, unsigned int base = 10)
    : m_bz(BzFromString(init, base, BZ_UNTIL_END)) {
    if (m_bz == 0) {
      throw std::domain_error("Invalid CBignum string: "
                              + std::string(init));
    }
  }
  /**
   * Translates the String representation of a CBignum in the specified base
   * into a CBignum.
   * @param [in] init string representation of a CBignum.
   * @param [in] base integer base.
   */
  CBignum(const char* init, size_t len, unsigned int base = 10)
    : m_bz(BzFromStringLen(init, len, base, BZ_UNTIL_END)) {
    if (m_bz == 0) {
      throw std::domain_error("Invalid CBignum string: "
                              + std::string(init));
    }
  }
  /**
   * Make a CBignum from boolean value. true is CBignum(1), false is CBignum(0).
   * @param [in] b initial boolean value.
   */
  explicit CBignum(bool b) : m_bz(BzFromInteger(b ? 1 : 0)) {}

  /**
   * destructor. Free memory allocated for this CBignum.
   */
  ~CBignum() {
    if (m_bz) {
      BzFree(m_bz);
    }
  }

  // convertions

  /**
   * Convert this CBignum to BzInt.
   * @return BzInt representation. CBignum may be truncated.
   */
  operator BzInt() const throw() {
    return BzToInteger(m_bz);
  }

  /**
   * Convert this CBignum to BzUInt.
   * @return BzUInt representation. CBignum may be truncated.
   */
  operator BzUInt() const throw() {
    return BzToUnsignedInteger(m_bz);
  }

  /**
   * Convert this CBignum to double (very inefficient).
   * @return BzUInt representation. CBignum may be truncated.
   */
  operator double() const throw() {
    const std::string s(static_cast<std::string>(*this));
    return strtod(s.c_str(), NULL);
  }

  /**
   * Convert this CBignum to bool (true if non 0).
   * @return true if CBignum != 0.
   */
  operator bool() const throw() {
    return BzGetSign(m_bz) != BZ_ZERO;
  }

  /**
   * Convert this CBignum to a C++ string.
   * @return base 10 string representation.
   */
  operator std::string() const throw();

  // unary +, -, ++, --

  /**
   * The result of the unary + operator is the value of its (promoted)
   * operand. The integer promotions are performed on the operand, and
   * the result has the promoted type.
   * @return integer
   */
  int operator+() const throw() {
    return static_cast<int>(static_cast<BzInt>(*this));
  }

  /**
   * Negate (change sign) of this CBignum.
   * @return -(*this).
   */
  CBignum operator-() const throw() {
    return CBignum(BzNegate(m_bz), ASSIGN);
  }

  inline CBignum& operator++() throw();
  CBignum operator++(int);
  inline CBignum& operator--() throw();
  CBignum operator--(int);

  // binary +, - *, /, %

  friend CBignum operator+(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzAdd(bz1.m_bz, bz2.m_bz), ASSIGN);
  }

  friend CBignum operator-(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzSubtract(bz1.m_bz, bz2.m_bz), ASSIGN);
  }

  friend CBignum operator*(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzMultiply(bz1.m_bz, bz2.m_bz), ASSIGN);
  }

  friend CBignum operator/(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzDiv(bz1.m_bz, bz2.m_bz), ASSIGN);
  }

  friend CBignum operator%(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzMod(bz1.m_bz, bz2.m_bz), ASSIGN);
  }

  // comparisons

  friend bool operator==(const CBignum& bz1, const CBignum& bz2) {
    return BzCompare(bz1.m_bz, bz2.m_bz) == BZ_EQ;
  }

  friend bool operator!=(const CBignum& bz1, const CBignum& bz2) {
    return !(bz1 == bz2);
  }

  friend bool operator>(const CBignum& bz1, const CBignum& bz2) {
    return BzCompare(bz1.m_bz, bz2.m_bz) == BZ_GT;
  }

  friend bool operator<=(const CBignum& bz1, const CBignum& bz2) {
    return !(bz1 > bz2);
  }

  friend bool operator<(const CBignum& bz1, const CBignum& bz2) {
    return BzCompare(bz1.m_bz, bz2.m_bz) == BZ_LT;
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

  /**
   * Inverse bits of CBignum.
   * @return a new CBignum as ~(*this).
   */
  CBignum operator~ () {
    return CBignum(BzNot(m_bz), ASSIGN);
  }

  /**
   * logbitp is used to test the value of a particular bit in integer,
   * that is treated as if it were binary. The value of logbitp is
   * true if the bit in integer whose index is index (that is, its
   * weight is 2^index) is a one-bit; otherwise it is false.  Negative
   * integers are treated as if they were in two's-complement
   * notation.
   * @param [in] bitnb bit to be tested.
   * @return true if bit bitnb is set.
   */
  bool logbitp(unsigned int bitnb) const throw() {
    return BzTestBit(bitnb, m_bz) == BN_TRUE;
  }

  /**
   * logbitp is used to test the value of a particular bit in integer,
   * that is treated as if it were binary. The value of logbitp is
   * true if the bit in integer whose index is index (that is, its
   * weight is 2^index) is a one-bit; otherwise it is false.  Negative
   * integers are treated as if they were in two's-complement
   * notation.
   * @param [in] bitnb bit to be tested.
   * @return true if bit bitnb is set.
   */
  bool logbitp(const CBignum& bitnb) const {
    if (BzGetSign(bitnb.m_bz) == BZ_MINUS) {
      throw std::domain_error("logbitp, bitnb is a negative value: "
                              + static_cast<std::string>(bitnb));
    }
    const BzUInt bn(bitnb);
    return BzTestBit(static_cast<unsigned int>(bn), m_bz) == BN_TRUE;
  }

  // shifts

  /**
   * Shift left bz1 by bz2 bytes.
   * @param [in] bz1 CBignum to shift.
   * @param [in] bz2 number of bytes to shift.
   */
  friend CBignum operator<<(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzAsh(bz1.m_bz,
                         static_cast<int>(BzToInteger(bz2.m_bz))),
                   ASSIGN);
  }

  /**
   * Shift right bz1 by bz2 bytes.
   * @param [in] bz1 CBignum to shift.
   * @param [in] bz2 number of bytes to shift.
   */
  friend CBignum operator>>(const CBignum& bz1, const CBignum& bz2) {
    return CBignum(BzAsh(bz1.m_bz,
                         -static_cast<int>(BzToInteger(bz2.m_bz))),
                   ASSIGN);
  }

  /**
   * @return the integer square root (isqrt) of a this CBignum.
   * It is the positive integer m which is the greatest integer less than
   * or equal to the square root of n
   */
  CBignum isqrt() const {
    if (BzGetSign(m_bz) == BZ_MINUS) {
      throw std::domain_error("isqrt of a negative value: "
                              + static_cast<std::string>(*this));
    }
    return CBignum(BzSqrt(m_bz), ASSIGN);
  }

  /**
   * @return true if this CBignum is even (*this % 2 == 0)
   */
  bool evenp() const throw() {
    return (BzIsEven(m_bz) == BN_TRUE);
  }

  /**
   * @return true if this CBignum is odd (*this % 2 == 1)
   */
  bool oddp() const throw() {
    return (BzIsEven(m_bz) == BN_FALSE);
  }

  /**
   * @return the number of bits in the minimal two's-complement
   * representation of this CBignum, excluding a sign bit.
   */
  size_t length() const throw() {
    return BzLength(m_bz);
  }

  /**
   * @return the number of bits in the two's complement representation
   * of this CBignum that differ from its sign bit.
   */
  size_t count() const throw() {
    return static_cast<size_t>(BzBitCount(m_bz));
  }

  /**
   * @return a CBignum whose value is the greatest common divisor of abs(this)
   * and abs(val).
   */
  CBignum abs() const throw() {
    return CBignum(BzAbs(m_bz), ASSIGN);
  }

  /**
   * @return -1, 0 or 1 as the value of this CBignum is negative,
   * zero or positive.
   */
  int signum() const throw() {
    return static_cast<int>(BzGetSign(m_bz));
  }

  /**
   * Divide this by bz and returns floor value.
   * @param [in] bz CBignum to divide by.
   * @return floor of this / bz
   */
  CBignum floor(const CBignum& bz) const throw() {
    return CBignum(BzFloor(m_bz, bz.m_bz), ASSIGN);
  }

  /**
   * Divide this by bz and returns ceiling value.
   * @param [in] bz CBignum to divide by.
   * @return ceiling of this / bz
   */
  CBignum ceiling(const CBignum& bz) const throw() {
    return CBignum(BzCeiling(m_bz, bz.m_bz), ASSIGN);
  }

  /**
   * Divide this by bz and returns rounded value.
   * @param [in] bz CBignum to divide by.
   * @return round of this / bz
   */
  CBignum round(const CBignum& bz) const throw() {
    return CBignum(BzRound(m_bz, bz.m_bz), ASSIGN);
  }

  /**
   * @param [in] bz value with which the GCD is to be computed.
   * @retrun a CBignum whose value is the greatest common divisor
   * of abs(this) and abs(bz).
   */
  CBignum gcd(const CBignum& bz) const throw() {
    return CBignum(BzGcd(m_bz, bz.m_bz), ASSIGN);
  }

  /**
   * @param [in] bz value with which the LCM is to be computed.
   * @retrun a CBignum whose value is the least common multiple
   * of abs(this) and abs(bz).
   */
  CBignum lcm(const CBignum& bz) const throw() {
    return CBignum(BzLcm(m_bz, bz.m_bz), ASSIGN);
  }

  /**
   * @param [in] exp exponent. Must be positive.
   * @eturn a CBignum whose value is (this exp).
   */
  CBignum pow(const CBignum& exp) const {
    BzInt i = BzToInteger(exp.m_bz);
    if (i > 0) {
      return CBignum(BzPow(m_bz, i), ASSIGN);
    } else if (i == 0) {
      return CBignum(1);
    } else {
      throw std::domain_error("pow is passed a negative argument " +
                              static_cast<std::string>(exp));
    }
  }

  /**
   * Compute this**exp
   * @param [in] exp exponent.
   * @eturn a CBignum whose value is (this exponent).
   */
  CBignum pow(unsigned int exp) const throw() {
    return CBignum(BzPow(m_bz, exp), ASSIGN);
  }

  /**
   * Get a random CBignum using seed.
   * @param [in, out] seed value used to compute next random number.
   * @eturn a CBignum random number.
   */
  CBignum random(unsigned int* seed) const throw() {
    return CBignum(BzRandom(m_bz, reinterpret_cast<BzSeed*>(seed)));
  }

  /**
   * Compute more efficiently this**exponent % modulus.
   * @param [in] exponent
   * @param [in] modulus
   * @return Modular exponentiation of this**exponent % modulus.
   */
  CBignum modExp(const CBignum& exponent,
                 const CBignum& modulus) const throw() {
    return CBignum(BzModExp(m_bz, exponent, modulus.m_bz), ASSIGN);
  }

  /**
   * assignment.
   * @param [in] rhs CBignum used for this assignment.
   */
  CBignum& operator=(const CBignum& rhs) {
    if (this != &rhs) {
      BzFree(m_bz);
      m_bz = BzCopy(rhs.m_bz);
    }
    return *this;
  }

#if defined(BN_CPP11)
  /**
   * Move-assignment.
   * @param [in] rhs CBignum to move from.
   */
  CBignum& operator=(CBignum&& rhs) noexcept {
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

  /**
   * Add value from rsh to CBignum. Object is modified.
   * @param [in] rhs.
   * @return modified object.
   */
  CBignum& operator+=(const CBignum& rhs) {
    return replace(BzAdd(m_bz, rhs.m_bz));
  }

  /**
   * Subtract value from rsh to CBignum. Object is modified.
   * @param [in] rhs.
   * @return modified object.
   */
  CBignum& operator-=(const CBignum& rhs) {
    return replace(BzSubtract(m_bz, rhs.m_bz));
  }

  /**
   * Multiply value from rsh to CBignum. Object is modified.
   * @param [in] rhs.
   * @return modified object.
   */
  CBignum& operator*=(const CBignum& rhs) {
    return replace(BzMultiply(m_bz, rhs.m_bz));
  }

  /**
   * Divide value from rsh to CBignum. Object is modified.
   * @param [in] rhs.
   * @return modified object.
   */
  CBignum& operator/=(const CBignum& rhs) {
    return replace(BzDiv(m_bz, rhs.m_bz));
  }

  /**
   * Modulo value from rsh to CBignum. Object is modified.
   * @param [in] rhs.
   * @return modified object.
   */
  CBignum& operator%=(const CBignum& rhs) {
    return replace(BzMod(m_bz, rhs.m_bz));
  }

  /**
   * Left shift by i bits. Object is modified.
   * @param [in] i number of bits to shift.
   * @return modified object.
   */
  CBignum& operator<<=(int i) {
    return replace(BzAsh(m_bz, i));
  }

  /**
   * Right shift by i bits. Object is modified.
   * @param [in] i number of bits to shift.
   * @return modified object.
   */
  CBignum& operator>>=(int i) {
    return replace(BzAsh(m_bz, -i));
  }

  /**
   * Print a CBignum object to an ostream.
   * @param [in] os output stream.
   * @param [in] bn CBignum object to print.
   */
  friend std::ostream& operator<<(std::ostream& os, const CBignum& bn);

  // version
  static const char *version() { return BzVersion(); }

 private:
  template<typename T>
#if defined(BN_CPP11)
  constexpr
#endif
  inline static bool
  signedType(T init) {
    return init < T(0);
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

extern const CBignum zero;
extern const CBignum one;
extern const CBignum two;
extern const CBignum ten;

inline CBignum&
  CBignum::operator++() throw() {
  *this += one;
  return *this;
}

inline CBignum&
  CBignum::operator--() throw() {
  *this -= one;
  return *this;
}

inline CBignum isqrt(const CBignum& bz) {
  return bz.isqrt();
}

template<typename T>
inline CBignum isqrt(T i) {
  return isqrt(CBignum(i));
}

inline bool logbitp(unsigned int bitnb, const CBignum& bz) {
  return bz.logbitp(bitnb);
}

inline bool logbitp(const CBignum& bitnb, const CBignum& bz) {
  return bz.logbitp(bitnb);
}

inline CBignum abs(const CBignum& bz) {
  return bz.abs();
}

inline bool evenp(const CBignum& bz) {
  return bz.evenp();
}

inline bool oddp(const CBignum& bz) {
  return bz.oddp();
}

inline size_t length(const CBignum& bz) {
  return bz.length();
}

inline CBignum floor(const CBignum& bz1, const CBignum& bz2) {
  return bz1.floor(bz2);
}
template<typename T>
inline CBignum floor(T i, const CBignum& bz) {
  const CBignum tmp(i);
  return tmp.floor(bz);
}

inline CBignum ceiling(const CBignum& bz1, const CBignum& bz2) {
  return bz1.ceiling(bz2);
}
template<typename T>
inline CBignum ceiling(T i, const CBignum& bz) {
  const CBignum tmp(i);
  return tmp.ceiling(bz);
}

inline CBignum round(const CBignum& bz1, const CBignum& bz2) {
  return bz1.round(bz2);
}
template<typename T>
inline CBignum round(T i, const CBignum& bz) {
  const CBignum tmp(i);
  return tmp.round(bz);
}

inline CBignum gcd(const CBignum& bz1, const CBignum& bz2) {
  return bz1.gcd(bz2);
}

template<typename T>
inline CBignum gcd(T i, const CBignum& bz) {
  const CBignum tmp(i);
  return tmp.gcd(bz);
}

inline CBignum lcm(const CBignum& bz1, const CBignum& bz2) {
  return bz1.lcm(bz2);
}

template<typename T>
inline CBignum lcm(T i, const CBignum& bz) {
  const CBignum tmp(i);
  return tmp.lcm(bz);
}

inline CBignum pow(const CBignum& bz, const CBignum& exp) {
  return bz.pow(exp);
}

inline CBignum pow(const CBignum& bz, unsigned int exp) {
  return bz.pow(exp);
}

template<typename T>
inline CBignum pow(T base, unsigned int exp) {
  const CBignum tmp(base);
  return tmp.pow(exp);
}

inline CBignum random(const CBignum& bz, unsigned int* seed) {
  return bz.random(seed);
}

#define BN_CMP_OP(op)                                 \
  template<typename T>                                \
  inline bool operator op (const CBignum& bz1, T i) { \
    return (bz1 op CBignum(i));                       \
  }                                                   \
  template<typename T>                                \
  inline bool operator op (T i, const CBignum& bz1) { \
    return (CBignum(i) op bz1);                       \
}

#define BN_BIN_OP(op)                                 \
  template<typename T>                                \
  inline CBignum operator op (const CBignum& bz1, T i) { \
    return (bz1 op CBignum(i));                       \
  }                                                   \
  template<typename T>                                \
  inline CBignum operator op (T i, const CBignum& bz1) { \
    return (CBignum(i) op bz1);                       \
}

BN_CMP_OP(>)
BN_CMP_OP(>=)
BN_CMP_OP(<)
BN_CMP_OP(<=)
BN_CMP_OP(==)
BN_CMP_OP(!=)

BN_BIN_OP(*)
BN_BIN_OP(/)
BN_BIN_OP(+)
BN_BIN_OP(-)
BN_BIN_OP(%)
BN_BIN_OP(>>)
BN_BIN_OP(<<)

} /* namespace bignum */
#endif  /* __CBIGNUM_H */
