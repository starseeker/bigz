/*
 * Simplified BSD License
 *
 * Copyright (c) 1988-1989, Digital Equipment Corporation & INRIA.
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

/*
 *      Agglomeration header of the n/z/q headers from bigz:
 *
 *      bign.h - Types and structures for clients of BigNum
 *      bigz.h - Types and structures for clients of BigZ
 *      bigq.h - Types and structures for clients of BigQ
 */

#if !defined(__BIGNUM_H)
#  define __BIGNUM_H

#if defined(HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <stdlib.h>
#if defined(_WIN64) || (defined(HAVE_STDINT_H) && (SIZEOF_VOID_P >= 8))
#  include <stdint.h>
#endif

/*
 *  Bignum representation
 *
 *  <--------------------------- nl ---------------------------->
 *  |   Least                                           Most    |
 *  |Significant|           |           |           |Significant|
 *  |BigNumDigit|           |           |           |BigNumDigit|
 *  |___________|___________|___________|___________|___________|
 *        ^                                          (sometimes
 *        |                                            is zero)
 *       nn
 */

#if     defined(__cplusplus)
extern  "C"     {
#endif

#if     defined(BN_EXPERIMENTAL_128BIT)
#define BN_NUM_DIGIT_TYPE
typedef __uint128_t             BigNumDigit;
#endif

/*
 *      Internal digit type.
 */

#if     !defined(BN_NUM_DIGIT_TYPE)
#define BN_NUM_DIGIT_TYPE
#if     defined(_WIN64) || (defined(HAVE_STDINT_H) && (SIZEOF_VOID_P >= 8))
typedef uint64_t                BigNumDigit;
#else
typedef unsigned long           BigNumDigit;
#endif
#endif

#if     defined(__GNUC__) && (__GNUC__ >= 3)
#if     !defined(BN_CONST_FUNCTION)
#define BN_CONST_FUNCTION       __attribute__((const))
#endif
#if     !defined(BN_PURE_FUNCTION)
#define BN_PURE_FUNCTION        __attribute__((pure))
#endif
#endif  /* __GNUC__ >= 3 */

#if     !defined(BN_PURE_FUNCTION)
#define BN_PURE_FUNCTION
#endif

#if     !defined(BN_CONST_FUNCTION)
#define BN_CONST_FUNCTION
#endif

/*
 *      bignum types: digits, big numbers, carries ...
 */

typedef BigNumDigit *   BigNum;         /* A big number is a digit pointer */
typedef BigNumDigit     BigNumProduct;  /* The product of two digits       */
#if     defined(BN_EXPERIMENTAL_128BITX)
typedef __uint128_t     BigNumLength;   /* The length of a bignum          */
#else
typedef unsigned int    BigNumLength;   /* The length of a bignum          */
#endif

typedef enum    {
        BN_FALSE   = 0,
        BN_TRUE    = 1
} BigNumBool;

/*
 *      Results of compare functions
 */

typedef enum    {
        BN_LT      = -1,
        BN_EQ      = 0,
        BN_GT      = 1
} BigNumCmp;

/*
 *      Carry enum type.
 */

typedef enum    {
        BN_NOCARRY = 0,
        BN_CARRY   = 1
} BigNumCarry;

/*
 *      sizes
 *
 *      BN_BYTE_SIZE:   number of bits in a byte
 *      BN_DIGIT_SIZE:  number of bits in a digit of a BigNum
 */

#if     !defined(BN_BYTE_SIZE)
#define BN_BYTE_SIZE    ((BigNumLength)8) /* may be 9! on 36bit computers. */
#endif

#define BN_DIGIT_SIZE   (sizeof(BigNumDigit) * BN_BYTE_SIZE)

/*
 *      some constants
 */

#define BN_ZERO         ((BigNumDigit)0)
#define BN_ONE          ((BigNumDigit)1)
#define BN_COMPLEMENT   (~(BigNumDigit)0)

/*
 *      functions of bign.c
 */

extern BigNumCarry  BnnAdd(BigNum mm, BigNumLength ml, const BigNum nn, BigNumLength nl, BigNumCarry carryin);
extern BigNumCarry  BnnAddCarry(BigNum nn, BigNumLength nl, BigNumCarry carryin);
extern void         BnnAndDigits(BigNum n, BigNumDigit d);
extern void         BnnAssign(BigNum mm, const BigNum nn, BigNumLength nl);
extern BigNumCmp    BnnCompare(const BigNum mm, BigNumLength ml, const BigNum nn, BigNumLength nl) BN_PURE_FUNCTION;
extern BigNumCmp    BnnCompareDigits(BigNumDigit d1, BigNumDigit d2) BN_CONST_FUNCTION;
extern void         BnnComplement(BigNum nn, BigNumLength nl);
extern void         BnnComplement2(BigNum nn, BigNumLength nl);
extern void         BnnDivide(BigNum nn, BigNumLength nl, BigNum dd, BigNumLength dl);
extern BigNumDigit  BnnDivideDigit(BigNum qq, BigNum nn, BigNumLength nl, BigNumDigit d);
extern BigNumDigit  BnnGetDigit(const BigNum nn) BN_PURE_FUNCTION;
extern BigNumBool   BnnIsPower2(const BigNum nn, BigNumLength nl) BN_PURE_FUNCTION;
extern BigNumBool   BnnIsDigitEven(BigNumDigit d) BN_CONST_FUNCTION;
extern BigNumBool   BnnIsDigitOdd(BigNumDigit d) BN_CONST_FUNCTION;
extern BigNumBool   BnnIsDigitNormalized(BigNumDigit d) BN_CONST_FUNCTION;
extern BigNumBool   BnnIsDigitZero(BigNumDigit d) BN_CONST_FUNCTION;
extern BigNumBool   BnnIsZero(const BigNum nn, BigNumLength nl) BN_PURE_FUNCTION;
extern BigNumCarry  BnnMultiply(BigNum pp, BigNumLength pl, const BigNum mm, BigNumLength ml, const BigNum nn, BigNumLength nl);
extern BigNumCarry  BnnMultiplyDigit(BigNum pp, BigNumLength pl, const BigNum mm, BigNumLength ml, BigNumDigit d);
extern BigNumLength BnnNumDigits(const BigNum nn, BigNumLength nl) BN_PURE_FUNCTION;
extern BigNumLength BnnNumLength(const BigNum nn, BigNumLength nl) BN_PURE_FUNCTION;
extern BigNumLength BnnNumCount(const BigNum nn, BigNumLength nl) BN_PURE_FUNCTION;
extern BigNumLength BnnNumLeadingZeroBitsInDigit(BigNumDigit d) BN_CONST_FUNCTION;
extern void         BnnOrDigits(BigNum n, BigNumDigit d);
extern void         BnnSetDigit(BigNum nn, BigNumDigit d);
extern void         BnnSetToZero(BigNum nn, BigNumLength nl);
extern BigNumDigit  BnnShiftLeft(BigNum mm, BigNumLength ml, BigNumLength nbits);
extern BigNumDigit  BnnShiftRight(BigNum mm, BigNumLength ml, BigNumLength nbits);
extern BigNumCarry  BnnSubtract(BigNum mm, BigNumLength ml, const BigNum nn, BigNumLength nl, BigNumCarry carryin);
extern BigNumCarry  BnnSubtractBorrow(BigNum nn, BigNumLength nl, BigNumCarry carryin);
extern void         BnnXorDigits(BigNum n, BigNumDigit d);

#if     defined(__cplusplus)
}
#endif

/*
 * BigZ.h: Types and structures for clients of BigZ
 */

#if     defined(__cplusplus)
extern  "C"     {
#endif


#define BZ_PURE_FUNCTION                BN_PURE_FUNCTION
#define BZ_CONST_FUNCTION               BN_CONST_FUNCTION
#define BZ_VERSION                      "1.6.2"

/*
 * BigZ sign
 */

typedef enum {
        BZ_MINUS = -1,
        BZ_ZERO  = 0,
        BZ_PLUS  = 1
} BzSign;

/*
 * BigZ compare result
 */

typedef enum {
        BZ_LT    = BN_LT,
        BZ_EQ    = BN_EQ,
        BZ_GT    = BN_GT
} BzCmp;

typedef enum {
        BZ_UNTIL_END     = 0,
        BZ_UNTIL_INVALID = 1
} BzStrFlag;

/*
 * BigZ number
 */

typedef struct {
        BigNumLength    Size;
        BzSign          Sign;
} BigZHeader;

 /*
  * define a dummy positive value to declare a Digits vector.
  * BigZ is allocated with required size. Choose a rather large value
  * to prevent 'smart' compilers to exchange fields.
  */

#define BZ_DUMMY_SIZE   32

struct BigZStruct {
        BigZHeader Header;
        /*
         * Digit vector should be the last field to allow allocation
         * of the real size (BZ_DUMMY_SIZE is never used).
         */
        BigNumDigit Digits[BZ_DUMMY_SIZE];
};

typedef struct BigZStruct * __BigZ;

#define BZ_FORCE_SIGN                   1

/*
 *      macros of bigz.c
 */

#if     !defined(BZ_BIGNUM_TYPE)
#define BZ_BIGNUM_TYPE
typedef __BigZ                          BigZ;
#endif

#if     !defined(BZ_CHAR_TYPE)
#define BZ_CHAR_TYPE
typedef char                            BzChar;
#endif

#if     defined(BZ_EXPERIMENTAL_128BIT)
/*
 * No really need to use those types. The most important thing to have is
 * bucket size of 128bit as provided by BN_EXPERIMENTAL_128BIT.
 */
#define BZ_INT_TYPE
typedef __int128_t                      BzInt;
#define BZ_UINT_TYPE
typedef __uint128_t                     BzUInt;
#endif


#if     !defined(BZ_INT_TYPE)
#define BZ_INT_TYPE
#if     defined(_WIN64) || (defined(HAVE_STDINT_H) && (SIZEOF_VOID_P >= 8))
typedef int64_t                         BzInt;
#else
typedef int                             BzInt;
#endif
#endif

#if     !defined(BZ_UINT_TYPE)
#define BZ_UINT_TYPE
#if     defined(_WIN64) || (defined(HAVE_STDINT_H) && (SIZEOF_VOID_P >= 8))
typedef uint64_t                        BzUInt;
#else
typedef unsigned int                    BzUInt;
#endif
#endif

/*
 * Random seed type, by contract it must be an unsigned int.
 */
typedef unsigned int                    BzSeed;

#define BZ_OPTIMIZE_PRINT

#if     !defined(BZ_BUCKET_SIZE)
#if     defined(_WIN64) || (defined(SIZEOF_LONG) && (SIZEOF_LONG == 8))
#define BZ_BUCKET_SIZE 64
#else
#define BZ_BUCKET_SIZE 32
#endif
#endif

#if     !defined(__EXTERNAL_BIGZ_MEMORY)
#define __toBzObj(z)                    ((__BigZ)z)
#define BZNULL                          ((BigZ)0)
#define BzAlloc(size)                   malloc(size)
#define BzFree(z)                       free(z) /* free(__toBzObj(z)) */
#define BzStringAlloc(size)             malloc(size * sizeof(BzChar))
#define BzFreeString(s)                 free(s)
#endif

#define BzGetSize(z)                    (__toBzObj(z)->Header.Size)
#define BzGetSign(z)                    (__toBzObj(z)->Header.Sign)
#define BzGetDigit(z,n)                 (__toBzObj(z)->Digits[n])
#define BzToBn(z)                       (__toBzObj(z)->Digits)
#define BzSetSize(z,s)                  (__toBzObj(z)->Header.Size = (s))
#define BzSetSign(z,s)                  (__toBzObj(z)->Header.Sign = (s))
#define BzSetDigit(z,n,v)               (__toBzObj(z)->Digits[n]   = (v))

/*
 *      functions of bigz.c
 */

extern const char * BzVersion(void) BZ_CONST_FUNCTION;
extern BigZ         BzCreate(BigNumLength Size);
extern BigNumLength BzNumDigits(const BigZ z) BZ_PURE_FUNCTION;
extern BigNumLength BzLength(const BigZ z) BZ_PURE_FUNCTION;
extern BigZ         BzCopy(const BigZ z);
extern BigZ         BzNegate(const BigZ z);
extern BigZ         BzAbs(const BigZ z);
extern BzCmp        BzCompare(const BigZ y, const BigZ z) BZ_PURE_FUNCTION;
extern BigZ         BzAdd(const BigZ y, const BigZ z);
extern BigZ         BzSubtract(const BigZ y, const BigZ z);
extern BigZ         BzMultiply(const BigZ y, const BigZ z);
extern BigZ         BzDivide(const BigZ y, const BigZ z, BigZ *r);
extern BigZ         BzDiv(const BigZ y, const BigZ z);
extern BigZ         BzTruncate(const BigZ y, const BigZ z);
extern BigZ         BzFloor(const BigZ y, const BigZ z);
extern BigZ         BzCeiling(const BigZ y, const BigZ z);
extern BigZ         BzRound(const BigZ y, const BigZ z);
extern BigZ         BzMod(const BigZ y, const BigZ z);
extern BigZ         BzRem(const BigZ y, const BigZ z);
extern BigZ         BzPow(const BigZ base, BzUInt exponent);
extern BigNumBool   BzIsEven(const BigZ y) BZ_PURE_FUNCTION;
extern BigNumBool   BzIsOdd(const BigZ y) BZ_PURE_FUNCTION;
extern BzChar *     BzToString(const BigZ z, BigNumDigit base, int sign);
extern size_t       BzStrLen(const BzChar *s) BN_PURE_FUNCTION;
extern BzChar *     BzToStringBuffer(const BigZ z, BigNumDigit base, int sign, /*@null@*/ BzChar * const buf, /*@null@*/ size_t *len);
extern BzChar *     BzToStringBufferExt(const BigZ z, BigNumDigit base, int sign, /*@null@*/ BzChar * const buf, /*@null@*/ size_t *len, /*@null@*/ size_t *slen);
extern BigZ         BzFromStringLen(const BzChar *s, size_t len, BigNumDigit base, BzStrFlag flag);
extern BigZ         BzFromString(const BzChar *s, BigNumDigit base, BzStrFlag flag);
extern BigZ         BzFromInteger(BzInt i);
extern BigZ         BzFromUnsignedInteger(BzUInt i);
extern BzInt        BzToInteger(const BigZ z) BZ_PURE_FUNCTION;
extern int          BzToIntegerPointer(const BigZ z, BzInt *p);
extern BzUInt       BzToUnsignedInteger(const BigZ z) BZ_PURE_FUNCTION;
extern int          BzToUnsignedIntegerPointer(const BigZ z, BzUInt *p);
extern BigZ         BzFromBigNum(const BigNum n, BigNumLength nl);
extern BigNum       BzToBigNum(const BigZ z, BigNumLength *nl);
extern BigNumBool   BzTestBit(BigNumLength bit, const BigZ z);
extern BigNumLength BzBitCount(const BigZ z);
extern BigZ         BzNot(const BigZ z);
extern BigZ         BzAnd(const BigZ y, const BigZ z);
extern BigZ         BzOr(const BigZ y, const BigZ z);
extern BigZ         BzXor(const BigZ y, const BigZ z);
extern BigZ         BzNand(const BigZ x, const BigZ y);
extern BigZ         BzNor(const BigZ x, const BigZ y);
extern BigZ         BzEqv(const BigZ x, const BigZ y);
extern BigZ         BzAndC1(const BigZ x, const BigZ y);
extern BigZ         BzAndC2(const BigZ x, const BigZ y);
extern BigZ         BzOrC1(const BigZ x, const BigZ y);
extern BigZ         BzOrC2(const BigZ x, const BigZ y);
extern BigZ         BzAsh(const BigZ y, int n);
extern BigZ         BzSqrt(const BigZ z);
extern BigZ         BzLcm(const BigZ y, const BigZ z);
extern BigZ         BzGcd(const BigZ y, const BigZ z);
extern BigZ         BzRandom(const BigZ n, BzSeed *seed);
extern BigZ         BzModExp(const BigZ base, const BigZ exponent, const BigZ modulus);

/*
#define BZ_DEBUG
*/

#if     defined(BZ_DEBUG)
extern void         BnDebug(const char *m, const BzChar *bzstr, const BigNum n, BigNumLength nl, BzSign sign);
extern void         BzDebug(const char *m, const BigZ y);
#endif

#if     defined(__cplusplus)
}
#endif

/*
 * BigQ.h: Types and structures for clients of BigQ
 */

#if     defined(__cplusplus)
extern  "C"     {
#endif

#define BQ_PURE_FUNCTION                BN_PURE_FUNCTION
#define BQ_CONST_FUNCTION               BN_CONST_FUNCTION

/*
 * BigQ compare result
 */

typedef enum {
        BQ_LT    = BN_LT,
        BQ_EQ    = BN_EQ,
        BQ_GT    = BN_GT,
        BQ_ERR   = 100
} BqCmp;

/*
 * BigQ number
 */

struct BigQStruct {
        BigZ    N;
        BigZ    D;
};

typedef struct BigQStruct * __BigQ;

#if     !defined(BQ_RATIONAL_TYPE)
#define BQ_RATIONAL_TYPE
typedef __BigQ                          BigQ;
#endif

#if     !defined(__EXTERNAL_BIGQ_MEMORY)
#define __toBqObj(q)                    ((__BigQ)q)
#define BQNULL                          ((BigQ)0)
#define BqAlloc()                       malloc(sizeof(struct BigQStruct))
#define BqFree(q)                     free(q) /* free(__toBqObj(q)) */
#define BqGetNumerator(q)               (__toBqObj(q)->N)
#define BqGetDenominator(q)             (__toBqObj(q)->D)
#define BqSetNumerator(q,n)             (__toBqObj(q)->N = (n))
#define BqSetDenominator(q,d)           (__toBqObj(q)->D = (d))
#endif

/*
 *      functions of bigq.c
 */

extern BigQ         BqAbs(const BigQ a);
extern BigQ         BqAdd(const BigQ a, const BigQ b);
extern BigQ         BqCreate(const BigZ n, const BigZ d);
extern BqCmp        BqCompare(const BigQ a, const BigQ b) BQ_PURE_FUNCTION;
extern void         BqDelete(const BigQ a);
extern BigQ         BqDiv(const BigQ a, const BigQ b);
extern BigQ         BqFromString(const BzChar *s, int base);
extern BigQ         BqInverse(const BigQ a);
extern BigQ         BqMultiply(const BigQ a, const BigQ b);
extern BigQ         BqNegate(const BigQ a);
extern BigQ         BqSubtract(const BigQ a, const BigQ b);
extern BzChar *     BqToString(const BigQ q, int sign);
extern BigQ         BqFromDouble(double num, BzInt maxd);
extern double       BqToDouble(const BigQ a);

#if 0
extern BzChar *     BqToStringBuffer(const BigQ q, int sign, BzChar *buf, size_t *len);
#endif

#if     defined(__cplusplus)
}
#endif

#endif  /* __BIGNUM_H */
