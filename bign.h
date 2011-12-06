/*
 * $Id: bign.h,v 1.22 2011-12-06 09:31:19 jullien Exp $
*/

/*
 *	bign.h - Types and structures for clients of BigNum.
 */

#if	!defined( __BIGN_H )
#define	__BIGN_H

#if	defined( HAVE_CONFIG_H )
#include "config.h"
#endif

/*
 * Copyright:
 *  - Digital Equipment Corporation & INRIA 1988, 1989
 *  - Eligis 1997 - 2012
 */

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

#if	defined( __cplusplus )
extern	"C"	{
#endif

/*
 *	Internal digit type.
 */

#if	!defined( BN_NUM_DIGIT_TYPE )
#define	BN_NUM_DIGIT_TYPE
#if	defined( _WIN64 )
typedef unsigned __int64	BigNumDigit;
#else
typedef unsigned int		BigNumDigit;
#endif
#endif

#if	!defined( BN_PURE_FUNCTION )
#define	BN_PURE_FUNCTION
#endif

#if	!defined( BN_CONST_FUNCTION )
#define	BN_CONST_FUNCTION
#endif

/*
 *	bignum types: digits, big numbers, carries ...
 */

typedef BigNumDigit *	BigNum;		/* A big number is a digit pointer */
typedef BigNumDigit	BigNumProduct;	/* The product of two digits	   */
typedef unsigned int	BigNumLength;	/* The length of a bignum	   */

typedef enum	{
	BN_FALSE   = 0,
	BN_TRUE    = 1
} BigNumBool;

/*
 *	Results of compare functions
 */

typedef	enum	{
	BN_LT      = -1,
	BN_EQ      = 0,
	BN_GT      = 1
} BigNumCmp;

/*
 *	Carry enum type.
 */

typedef enum	{
	BN_NOCARRY = 0,
	BN_CARRY   = 1
} BigNumCarry;

/*
 *	sizes
 *
 *	BN_BYTE_SIZE:	number of bits in a byte
 *	BN_DIGIT_SIZE:	number of bits in a digit of a BigNum
 */

#define BN_BYTE_SIZE	8
#define BN_DIGIT_SIZE	(sizeof(BigNumDigit) * BN_BYTE_SIZE)

/*
 *	some constants
 */

#define	BN_ZERO		((BigNumDigit)0)
#define	BN_ONE		((BigNumDigit)1)
#define	BN_COMPLEMENT	(~(BigNumDigit)0)

/*
 *	functions of bign.c
 */

extern BigNumCarry  BnnAdd(BigNum mm, BigNumLength ml, BigNum nn, BigNumLength nl, BigNumCarry carryin);
extern BigNumCarry  BnnAddCarry(BigNum nn, BigNumLength nl, BigNumCarry carryin);
extern void	    BnnAndDigits(BigNum n, BigNumDigit d);
extern void	    BnnAssign(BigNum mm, BigNum nn, BigNumLength nl);
extern BigNumCmp    BnnCompare(BigNum mm, BigNumLength ml, BigNum nn, BigNumLength nl) BN_PURE_FUNCTION;
extern BigNumCmp    BnnCompareDigits(BigNumDigit d1, BigNumDigit d2) BN_CONST_FUNCTION;
extern void	    BnnComplement(BigNum nn, BigNumLength nl);
extern void	    BnnComplement2(BigNum nn, BigNumLength nl);
extern void	    BnnDivide(BigNum nn, BigNumLength nl, BigNum dd, BigNumLength dl);
extern BigNumDigit  BnnDivideDigit(BigNum qq, BigNum nn, BigNumLength nl, BigNumDigit d);
extern BigNumDigit  BnnGetDigit(BigNum nn) BN_PURE_FUNCTION;
extern BigNumBool   BnnIsPower2(BigNum nn, BigNumLength nl) BN_PURE_FUNCTION;
extern BigNumBool   BnnIsDigitEven(BigNumDigit d) BN_CONST_FUNCTION;
extern BigNumBool   BnnIsDigitOdd(BigNumDigit d) BN_CONST_FUNCTION;
extern BigNumBool   BnnIsDigitNormalized(BigNumDigit d) BN_CONST_FUNCTION;
extern BigNumBool   BnnIsDigitZero(BigNumDigit d) BN_CONST_FUNCTION;
extern BigNumBool   BnnIsZero(BigNum nn, BigNumLength nl) BN_PURE_FUNCTION;
extern BigNumCarry  BnnMultiply(BigNum pp, BigNumLength pl, BigNum mm, BigNumLength ml, BigNum nn, BigNumLength nl);
extern BigNumCarry  BnnMultiplyDigit(BigNum pp, BigNumLength pl, BigNum mm, BigNumLength ml, BigNumDigit d);
extern BigNumLength BnnNumDigits(BigNum nn, BigNumLength nl) BN_PURE_FUNCTION;
extern BigNumLength BnnNumLength(BigNum nn, BigNumLength nl) BN_PURE_FUNCTION;
extern BigNumLength BnnNumLeadingZeroBitsInDigit(BigNumDigit d) BN_CONST_FUNCTION;
extern void	    BnnOrDigits(BigNum n, BigNumDigit d);
extern void	    BnnSetDigit(BigNum nn, BigNumDigit d);
extern void	    BnnSetToZero(BigNum nn, BigNumLength nl);
extern BigNumDigit  BnnShiftLeft(BigNum mm, BigNumLength ml, BigNumLength nbits);
extern BigNumDigit  BnnShiftRight(BigNum mm, BigNumLength ml, BigNumLength nbits);
extern BigNumCarry  BnnSubtract(BigNum mm, BigNumLength ml, BigNum nn, BigNumLength nl, BigNumCarry carryin);
extern BigNumCarry  BnnSubtractBorrow(BigNum nn, BigNumLength nl, BigNumCarry carryin);
extern void	    BnnXorDigits(BigNum n, BigNumDigit d);

#if	defined( __cplusplus )
}
#endif

#endif	/* __BIGN_H */
