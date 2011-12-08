/* Copyright     Digital Equipment Corporation & INRIA     1988 */

/*
 * Copyright (c) 1988-1989, Digital Equipment Corporation & INRIA.
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

/* BntoBnn.h: allowing to use the new interfaces of KerN */

#include <stdlib.h>
#include "bign.h"


		/* old types of Bn */

typedef unsigned int 	BigNumType;	/* A BigNum's type */

struct BigNumHeader  			/* The header of a BigNum */
{
   BigNumType 	type;
   int		length;
};


		/* macros of old types of Bn */

#define BN_TYPE(n)	(((struct BigNumHeader *) n) - 1)->type
#define BN_LENGTH(n)	(((struct BigNumHeader *) n) - 1)->length


		/* macros of functions of Bn to functions Bnn */

#define BnIsZero(n, nd, nl)				BnnIsZero ((n+nd), nl)
#define BnMultiply(p, pd, pl, m, md, ml, n, nd, nl)	BnnMultiply ((p+pd), pl, (m+md), ml, (n+nd), nl)
#define BnDivide(n, nd, nl, d, dd, dl)			BnnDivide ((n+nd), nl, (d+dd), dl)
#define BnCompare(m, md, ml, n, nd, nl)			BnnCompare ((m+md), ml, (n+nd), nl)
#define BnSetToZero(n, nd, nl) 				BnnSetToZero ((n+nd), nl) 
#define BnAssign(m, md, n, nd, nl)			BnnAssign ((m+md), (n+nd), nl)
#define BnSetDigit(n, nd, d) 				BnnSetDigit ((n+nd), d) 
#define BnGetDigit(n, nd)				BnnGetDigit ((n+nd))
#define BnNumDigits(n, nd, nl)				BnnNumDigits ((n+nd), nl)
#define BnNumLeadingZeroBitsInDigit(n, nd)		BnnNumLeadingZeroBitsInDigit (*(n+nd))
#define BnDoesDigitFitInWord(n, nd) 			BnnDoesDigitFitInWord (*(n+nd))
#define BnIsDigitZero(n, nd) 				BnnIsDigitZero (*(n+nd))
#define BnIsDigitNormalized(n, nd) 			BnnIsDigitNormalized (*(n+nd))
#define BnIsDigitOdd(n, nd) 				BnnIsDigitOdd (*(n+nd))
#define BnCompareDigits(m, md, n, nd) 			BnnCompareDigits (*(m+md), *(n+nd))
#define BnComplement(n, nd, nl)				BnnComplement ((n+nd), nl)
#define BnAndDigits(m, md, n, nd) 			BnnAndDigits ((m+md), *(n+nd))
#define BnOrDigits(m, md, n, nd) 			BnnOrDigits ((m+md), *(n+nd))
#define BnXorDigits(m, md, n, nd) 			BnnXorDigits ((m+md), *(n+nd))
#define BnShiftLeft(m, md, ml, n, nd, nbits)		*(n+nd) = BnnShiftLeft ((m+md), ml, nbits)
#define BnShiftRight(m, md, ml, n, nd, nbits)		*(n+nd) = BnnShiftRight ((m+md), ml, nbits)
#define BnAddCarry(n, nd, nl, carryin)			BnnAddCarry ((n+nd), nl, carryin)
#define BnAdd(m, md, ml, n, nd, nl, carryin)		BnnAdd ((m+md), ml, (n+nd), nl, carryin)
#define BnSubtractBorrow(n, nd, nl, carryin)		BnnSubtractBorrow ((n+nd), nl, carryin)
#define BnSubtract(m, md, ml, n, nd, nl, carryin)	BnnSubtract ((m+md), ml, (n+nd), nl, carryin)
#define BnMultiplyDigit(p, pd, pl, m, md, ml, n, nd)	BnnMultiplyDigit ((p+pd), pl, (m+md), ml, *(n+nd))
#define BnDivideDigit(q, qd, r, rd, n, nd, nl, d, dd)	*(r+rd) = BnnDivideDigit ((q+qd), (n+nd), nl, *(d+dd))


		/* old functions of Bn */

/*
 *	Creation and access to type and length fields.
 */

/* Allocates a BigNum structure and returns a pointer to it */
BigNum BnAlloc(int size) {
	register BigNum n;
	int len;

	len = sizeof(struct BigNumHeader) + size * sizeof(BigNumDigit);

	n = (BigNum)((char *)malloc(len) + sizeof(struct BigNumHeader));

	BN_LENGTH(n) = size;
	return(n);
}
 
/* Allocates a BigNum, inserts its Type, and returns a pointer to it */
BigNum
BnCreate(BigNumType type, int size) {
	register BigNum n;
 
	n = BnAlloc(size);
	BN_TYPE(n) = type;
	BnSetToZero(n, 0, size);
	return(n);
}
 
/* Frees a BigNum structure */
int
BnFree(BigNum n) {
	free(((struct BigNumHeader *) n) - 1);
	return(1);
}
 
/* Returns the BigNum's Type */
BigNumType
BnGetType(BigNum n) {
        return(BN_TYPE(n));
}
 
/* Sets the BigNum's Type */
void
BnSetType(BigNum n, BigNumType type) {
        BN_TYPE(n) = type;
}
 
/* Returns the number of digits allocated for the BigNum */
int
BnGetSize(BigNum n) {
	return(BN_LENGTH(n));
}
 
