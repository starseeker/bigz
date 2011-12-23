//
// $Id: CRational.h,v 1.13 2011-12-09 07:52:23 jullien Exp $
//

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

//
//	CRational.h :	
//

#if	!defined( __CRATIONAL_H )
#define	__CRATIONAL_H_H

#include <iostream>
#include <stdlib.h>
#include <bigq.h>
#include <CBignum.h>

class	CRational {
private:
	enum	Flags { ASSIGN };
public:
	CRational( const CBignum &n, const CBignum &d )
		: m_q( BqCreate( n, d) ) {
	}
	CRational( const CBignum &n )
		: m_q( BqCreate( n, BzFromInteger((BzInt)1) ) ) {
	}
	~CRational() { BzFree( m_q ); }

	// binary +

	friend CRational operator+(const CRational& q1, const CRational& q2) {
		return CRational(BqAdd(q1.m_q, q2.m_q), ASSIGN);
	}
	friend CRational operator+(const CRational& q, const CBignum& bn) {
		return (q + CRational(bn));
	}
	friend CRational operator+(const CBignum& bn, const CRational& bz1) {
		return (CRational(bn) + bz1);
	}

	// binary -

	friend CRational operator-(const CRational& q1, const CRational& q2) {
		return CRational(BqSubtract(q1.m_q, q2.m_q), ASSIGN);
	}
	friend CRational operator-(const CRational& q, const CBignum& bn) {
		return (q - CRational(bn));
	}
	friend CRational operator-(const CBignum& bn, const CRational& bz1) {
		return (CRational(bn) - bz1);
	}

	// binary *

	friend CRational operator*(const CRational& q1, const CRational& q2) {
		return CRational(BqMultiply(q1.m_q, q2.m_q), ASSIGN);
	}
	friend CRational operator*(const CRational& q, const CBignum& bn) {
		return (q * CRational(bn));
	}
	friend CRational operator*(const CBignum& bn, const CRational& q) {
		return (CRational(bn) * q);
	}

	// binary /

	friend CRational operator/(const CRational& q1, const CRational& q2) {
		return CRational(BqDiv(q1.m_q, q2.m_q), ASSIGN);
	}
	friend CRational operator/(const CRational& q, const CBignum& bn) {
		return (q / CRational(bn));
	}
	friend CRational operator/(const CBignum& bn, const CRational& q) {
		return (CRational(bn) / q);
	}

	// comparisons

	friend bool operator==(const CRational& q1, const CRational& q2) {
		return BqCompare(q1.m_q, q2.m_q) == BZ_EQ;
	}
	friend bool operator==(const CRational& q, const CBignum& bn) {
		return (q == CRational(bn));
	}
	friend bool operator==(const CBignum& bn, const CRational& q) {
		return (CRational(bn) == q);
	}
	friend bool operator!=(const CRational& q1, const CBignum& q2) {
		return !(q1 != q2);
	}

	friend bool operator>(const CRational& q1, const CRational& q2) {
		return BqCompare(q1.m_q, q2.m_q) == BZ_GT;
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
		return BqCompare(q1.m_q, q2.m_q) == BZ_LT;
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

	// output
	friend std::ostream& operator<<(std::ostream& os, const CRational& q) {
		BzSign sign = BzGetSign(q.m_q);
		const char* n = BzToString( BqGetNumerator(q.m_q), 10, 0 );
		const char* d = BzToString( BqGetDenominator(q.m_q), 10, 0 );
		if( sign == BZ_MINUS ) {
			os << "-";
		}
		os << n << "/" << d;
		BzFreeString( (void *)d );
		BzFreeString( (void *)n );
		return( os );
	}
private:
	BigQ	m_q;
	CRational( const BigQ init, Flags ) : m_q( init ) {}
};

#endif	/* __CRATIONAL_H */
