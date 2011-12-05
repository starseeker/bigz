/*
 * $Id: bign.h,v 1.21 2011-12-05 10:25:39 jullien Exp $
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
typedef int		BigNumCmp;	/* Result of comparison		   */
typedef unsigned int	BigNumCarry;	/* Either BN_NOCARRY or BN_CARRY   */
typedef int		Boolean;	/* Boolean type			   */

/*
 *	sizes
 *
 *	BN_BYTE_SIZE:	number of bits in a byte
 *	BN_WORD_SIZE:	number of bits in an "int" in the target language
 *	BN_DIGIT_SIZE:	number of bits in a digit of a BigNum
 */

#define BN_BYTE_SIZE	8
#define BN_WORD_SIZE	(sizeof(int*) * BN_BYTE_SIZE)
#define BN_DIGIT_SIZE	(sizeof(BigNumDigit) * BN_BYTE_SIZE)

/*
 *	Results of compare functions
 *
 *	Note: we don't use "enum" to interface with Modula2+, Lisp, ...
 */

#define BN_LT		((BigNumCmp)-1)
#define BN_EQ		((BigNumCmp)0)
#define BN_GT		((BigNumCmp)1)

/*
 *	some constants
 */

#define	BN_ZERO		((BigNumDigit)0)
#define	BN_ONE		((BigNumDigit)1)
#define	BN_COMPLEMENT	(~(BigNumDigit)0)

#define	BN_CARRY	((BigNumCarry)1)
#define	BN_NOCARRY	((BigNumCarry)0)

#define BN_TRUE		((Boolean)1)
#define BN_FALSE	((Boolean)0)

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
extern Boolean	    BnnIsPower2(BigNum nn, BigNumLength nl) BN_PURE_FUNCTION;
extern Boolean	    BnnIsDigitEven(BigNumDigit d) BN_CONST_FUNCTION;
extern Boolean	    BnnIsDigitOdd(BigNumDigit d) BN_CONST_FUNCTION;
extern Boolean	    BnnIsDigitNormalized(BigNumDigit d) BN_CONST_FUNCTION;
extern Boolean	    BnnIsDigitZero(BigNumDigit d) BN_CONST_FUNCTION;
extern Boolean	    BnnIsZero(BigNum nn, BigNumLength nl) BN_PURE_FUNCTION;
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
