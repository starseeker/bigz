/*
 * $Id: bigq.h,v 1.11 2012-01-28 07:53:27 jullien Exp $
 */

/*
 * Simplified BSD License
 *
 * Copyright (c) 1992-2012, Eligis
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

#if	!defined( __BIGQ_H )
#define	__BIGQ_H

#if	!defined( __BIGZ_H )
#include "bigz.h"
#endif

#if	defined( __cplusplus )
extern	"C"	{
#endif

#define	BQ_PURE_FUNCTION		BN_PURE_FUNCTION
#define	BQ_CONST_FUNCTION		BN_CONST_FUNCTION

/*
 * BigQ.h: Types and structures for clients of BigQ
 */

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
	BigZ	N;
	BigZ	D;
};

typedef struct BigQStruct * __BigQ;

#if	!defined( BQ_RATIONAL_TYPE )
#define	BQ_RATIONAL_TYPE
typedef	__BigQ				BigQ;
#endif

#if	!defined( __EXTERNAL_BIGQ_MEMORY )
#define	__toBqObj(q)			((__BigQ)q)
#define	BQNULL				((BigQ)0)
#define	BqAlloc()			malloc( sizeof( struct BigQStruct ) )
#define	BqFree( q )			free( q ) /* free(__toBqObj(q)) */
#define BqGetNumerator(q)		(__toBqObj(q)->N)
#define BqGetDenominator(q)		(__toBqObj(q)->D)
#define BqSetNumerator(q,n)		(__toBqObj(q)->N = (n))
#define BqSetDenominator(q,d)		(__toBqObj(q)->D = (d))
#endif

/*
 *	functions of bigq.c
 */

extern BigQ	    BqAbs(const BigQ a);
extern BigQ	    BqAdd(const BigQ a, const BigQ b);
extern BigQ	    BqCreate(const BigZ n, const BigZ d);
extern BqCmp	    BqCompare(const BigQ a, const BigQ b) BQ_PURE_FUNCTION;
extern void	    BqDelete(const BigQ a);
extern BigQ	    BqDiv(const BigQ a, const BigQ b);
extern BigQ	    BqFromString(const BzChar *s, int base);
extern BigQ	    BqInverse(const BigQ a);
extern BigQ	    BqMultiply(const BigQ a, const BigQ b);
extern BigQ	    BqNegate(const BigQ a);
extern BigQ	    BqSubtract(const BigQ a, const BigQ b);
extern BzChar *	    BqToString(const BigQ q, int sign);
extern	BigQ	    BqFromDouble( double num, BzInt maxd );

#if 0
extern BzChar *	    BqToStringBuffer(const BigQ q, int sign, BzChar *buf, size_t *len);
#endif

#if	defined( __cplusplus )
}
#endif

#endif	/* __BIGQ_H */
