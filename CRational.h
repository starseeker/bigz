//
// $Id: CRational.h,v 1.13 2013-03-05 06:21:39 jullien Exp $
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

namespace rational {

using namespace bignum;

class	CRational {
private:
	enum	Flags { ASSIGN };
public:
	CRational( const CBignum &n = 0, const CBignum &d = 1)
		: m_q( BqCreate( n, d) ) {
	}
	CRational( const CRational &q )
		: m_q( BqCreate( BqGetNumerator(q.m_q),
				 BqGetDenominator(q.m_q)) ) {
	}
        CRational( const char *s, int base=10 )
		: m_q( BqFromString((BzChar *)s, base) ) {
	}
#if	defined( HAVE_BQ_FROM_DOUBLE )
	CRational( double n )
		: m_q( BqFromDouble(n) ) {
	}
#endif
	~CRational() { BqDelete(m_q); }

	// convertions

	operator const char* () const { return BqToString( m_q, 0 ); }
	operator BigQ        () const { return m_q; }

	// unary -

	friend CRational operator-(const CRational& q ) {
		return( CRational(BqNegate(q.m_q), ASSIGN ) );
	}

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
	friend bool operator!=(const CRational& q1, const CBignum& q2) {
		return !(q1 != q2);
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

	// output
	friend std::ostream& operator<<(std::ostream& os, const CRational& q) {
		BzChar *s = BqToString(q, 0);
		os << s;
		BzFreeString( (void *)s );
		return os;
	}
private:
	BigQ	m_q;
	CRational( const BigQ init, Flags ) : m_q( init ) {}
};

} // namespace rational

#endif	/* __CRATIONAL_H */
