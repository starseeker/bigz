/*
 * $Id: bzf.c,v 1.9 2015/12/19 08:15:23 jullien Exp $
 */

/*
 * Copyright (c) 1988-1989, Digital Equipment Corporation & INRIA.
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

/*
 *	bzf.c :	 Miscellaneous functions built on top of BigZ.
 */

extern	BigZ	BzFactorial(BigZ z);

BigZ
BzFactorial(BigZ z)
{
	/*
	 * Returns Z!
	 * Assumes Z < Base.
	 */

	BigZ		f;
	BigNumDigit	zval;
	int		fl = 1;

	zval = BnnGetDigit(BzToBn(z));
	f = BzCreate(zval+1);
	BnnSetDigit(BzToBn(f), 1);
	BzSetSign(f, BzGetSign(z));

	while (zval-- > 1) {
		BnnMultiplyDigit(BzToBn(f), fl+1, BzToBn(f), fl, zval);
		fl = BnnNumDigits(BzToBn(f), fl+1);
	}
    
	return (f);
}
