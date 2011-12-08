/*
 * $Id: bigz.h,v 1.43 2011-12-07 08:47:37 jullien Exp $
 */

/* Copyright Digital Equipment Corporation & INRIA 1988, 1989 */

#if	!defined( __BIGZ_H )
#define	__BIGZ_H

#if	!defined( __BIGN_H )
#include "bign.h"
#endif

#if	defined( __cplusplus )
extern	"C"	{
#endif

#define	BZ_PURE_FUNCTION		BN_PURE_FUNCTION
#define	BZ_CONST_FUNCTION		BN_CONST_FUNCTION

/*
 * BigZ.h: Types and structures for clients of BigZ
 */

#define	BZ_VERSION			"1.4"

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

/*
 * BigZ number
 */

typedef struct {
	BigNumLength	Size;
	BzSign		Sign;
} BigZHeader;

 /*
  * define a dummy positive value to declare a Digits vector.
  * BigZ is allocated with required size. Choose a rather large value
  * to prevent 'smart' compilers to exchange fields.
  */

#define	BZ_DUMMY_SIZE	32

struct BigZStruct {
	BigZHeader Header;
	/*
	 * Digit vector should be the last field to allow allocation
	 * of the real size (BZ_DUMMY_SIZE is never used).
	 */
	BigNumDigit 	  Digits[ BZ_DUMMY_SIZE ];
};

typedef struct BigZStruct * __BigZ;

#define	BZ_FORCE_SIGN			1

/*
 *	macros of bigz.c
 */

#if	!defined( BZ_BIGNUM_TYPE )
#define	BZ_BIGNUM_TYPE
typedef __BigZ				BigZ;
#endif

#if	!defined( BZ_CHAR_TYPE )
#define	BZ_CHAR_TYPE
typedef char				BzChar;
#endif

#if	!defined( BZ_INT_TYPE )
#define	BZ_INT_TYPE
#if	defined( _WIN64 )
typedef	__int64				BzInt;
#else
typedef	int				BzInt;
#endif
#endif

#if	!defined( BZ_UINT_TYPE )
#define	BZ_UINT_TYPE
#if	defined( _WIN64 )
typedef	unsigned __int64		BzUInt;
#else
typedef	unsigned int			BzUInt;
#endif
#endif

#define	BZ_OPTIMIZE_FOR_BASE10

#if	defined( BZ_OPTIMIZE_FOR_BASE10 )
/*
 *	Values should be portable for BigNumDigit size >= 32bit
 *	64bit ports may increase the two values to optimize even more.
 */
#if	!defined( BZ_MAX_BASE10 )
/*
 *	Max power of 10 to fix in a BigNumDigit (generally machine word).
 */
#define	BZ_MAX_BASE10		((BigNumDigit)1000000000)
#endif
#if	!defined( BZ_MAX_BASE10_DIGITS )
/*
 *	Max number of digits in base 10 that fit in a BigNumDigit.
 */
#define	BZ_MAX_BASE10_DIGITS	9
#endif
#endif

#if	!defined( __EXTERNAL_BIGZ_MEMORY )
#define	__toBzObj(z)			((__BigZ)z)
#define	BZNULL				((BigZ)0)
#define	BzAlloc( size )			malloc( size )
#define	BzStringAlloc( size )		malloc( size * sizeof( BzChar ) )
#define	BzFree( z )			free( __toBzObj(z) )
#define	BzFreeString( s )		free( s )
#endif

#define BzGetSize(z)			(__toBzObj(z)->Header.Size)
#define BzGetSign(z)			(__toBzObj(z)->Header.Sign)
#define BzGetDigit(z,n)			(__toBzObj(z)->Digits[n])
#define BzToBn(z)			(__toBzObj(z)->Digits)
#define BzSetSize(z,s)			(__toBzObj(z)->Header.Size = (s))
#define BzSetSign(z,s)			(__toBzObj(z)->Header.Sign = (s))
#define BzSetDigit(z,n,v)		(__toBzObj(z)->Digits[n]   = (v))

/*
 *	functions of bigz.c
 */

extern const char * BzVersion(void) BZ_CONST_FUNCTION;
extern BigZ	    BzCreate(BigNumLength Size);
extern BigNumLength BzNumDigits(const BigZ z) BZ_PURE_FUNCTION;
extern BigNumLength BzLength(const BigZ z) BZ_PURE_FUNCTION;
extern BigZ	    BzCopy(const BigZ z);
extern BigZ	    BzNegate(const BigZ z);
extern BigZ	    BzAbs(const BigZ z);
extern BzCmp	    BzCompare(const BigZ y, const BigZ z) BZ_PURE_FUNCTION;
extern BigZ	    BzAdd(const BigZ y, const BigZ z);
extern BigZ	    BzSubtract(const BigZ y, const BigZ z);
extern BigZ	    BzMultiply(const BigZ y, const BigZ z);
extern BigZ	    BzDivide(const BigZ y, const BigZ z, BigZ *r);
extern BigZ	    BzDiv(const BigZ y, const BigZ z);
extern BigZ	    BzTruncate(const BigZ y, const BigZ z);
extern BigZ	    BzFloor(const BigZ y, const BigZ z);
extern BigZ	    BzCeiling(const BigZ y, const BigZ z);
extern BigZ	    BzRound(const BigZ y, const BigZ z);
extern BigZ	    BzMod(const BigZ y, const BigZ z);
extern BigZ	    BzRem(const BigZ y, const BigZ z);
extern BigNumBool   BzIsEven(const BigZ y) BZ_PURE_FUNCTION;
extern BigNumBool   BzIsOdd(const BigZ y) BZ_PURE_FUNCTION;
extern BzChar *	    BzToString(const BigZ z, BigNumDigit base, int sign);
extern BzChar *	    BzToStringBuffer(const BigZ z, BigNumDigit base, int sign, BzChar *buf, size_t *len);
extern BigZ	    BzFromString(const BzChar *s, BigNumDigit base);
extern BigZ	    BzFromInteger(BzInt i);
extern BigZ	    BzFromUnsignedInteger(BzUInt i);
extern BzInt	    BzToInteger(const BigZ z) BZ_PURE_FUNCTION;
extern int	    BzToIntegerPointer(const BigZ z, BzInt *p);
extern BzUInt	    BzToUnsignedInteger(const BigZ z) BZ_PURE_FUNCTION;
extern int	    BzToUnsignedIntegerPointer(const BigZ z, BzUInt *p);
extern BigZ	    BzFromBigNum(BigNum n, BigNumLength nl);
extern BigNum	    BzToBigNum(const BigZ z, BigNumLength *nl);
extern BigNumBool   BzTestBit(unsigned int bit, const BigZ z);
extern BigZ	    BzNot(const BigZ z);
extern BigZ	    BzAnd(const BigZ y, const BigZ z);
extern BigZ	    BzOr(const BigZ y, const BigZ z);
extern BigZ	    BzXor(const BigZ y, const BigZ z);
extern BigZ	    BzNand(const BigZ x, const BigZ y);
extern BigZ	    BzNor(const BigZ x, const BigZ y);
extern BigZ	    BzEqv(const BigZ x, const BigZ y);
extern BigZ	    BzAndC1(const BigZ x, const BigZ y);
extern BigZ	    BzAndC2(const BigZ x, const BigZ y);
extern BigZ	    BzOrC1(const BigZ x, const BigZ y);
extern BigZ	    BzOrC2(const BigZ x, const BigZ y);
extern BigZ	    BzAsh(const BigZ y, int n);
extern BigZ	    BzSqrt(const BigZ z);
extern BigZ	    BzLcm(const BigZ y, const BigZ z);
extern BigZ	    BzGcd(const BigZ y, const BigZ z);
extern BigZ	    BzRandom(const BigZ n);
extern void	    BzSetRandom(const BigZ n);

#if	defined( BZ_DEBUG )
extern void	    BzShowBits(BigNumDigit n);
extern void	    BnDebug(const BzChar *m, const BzChar *bzstr, BigNum n, BigNumLength nl, BzSign sign);
extern void	    BzDebug(const BzChar *m, const BigZ y);
#endif

#if	defined( __cplusplus )
}
#endif

#endif	/* __BIGZ_H */
