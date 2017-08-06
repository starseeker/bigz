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
 *      This file is an agglomeration of the 3 C files from the parent
 *      bigz distribution at https://sourceforge.net/projects/bigz/ :
 *
 *      bign.c : the kernel written in pure C (it uses no C library)
 *      bigz.c : provides an implementation of "unlimited-precision"
 *               arithmetic for signed integers.
 *
 */

#if !defined(_CRT_SECURE_NO_DEPRECATE)
#  define _CRT_SECURE_NO_DEPRECATE        1
#endif
#if !defined(_CRT_NONSTDC_NO_DEPRECATE)
#  define _CRT_NONSTDC_NO_DEPRECATE       1
#endif

#include <stdlib.h>
#if defined(_WIN64) || defined(HAVE_STDINT_H)
#  include <stdint.h>
#endif
#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#include "bignum.h"

#define MaxInt(a, b)            (((a) < (b)) ? (b) : (a))
#define AbsInt(x)               (((x) >= 0) ? (x) : -(x))

/*                             bign.c                                */

/*
 *      Description of types and constants.
 *
 * Several conventions are used in the commentary:
 *    A "BigNum" is the name for an infinite-precision number.
 *    Capital letters (e.g., "N") are used to refer to the value of BigNums.
 *    The word "digit" refers to a single BigNum digit.
 *    The notation "Size(N)" refers to the number of digits in N,
 *       which is typically passed to the subroutine as "nl".
 *    The notation "Length(N)" refers to the number of digits in N,
 *       not including any leading zeros.
 *    The word "Base" is used for the number 2 ** BN_DIGIT_SIZE, where
 *       BN_DIGIT_SIZE is the number of bits in a single BigNum digit.
 *    The expression "BBase(N)" is used for Base ** NumDigits(N).
 *    The term "leading zeros" refers to any zeros before the most
 *       significant digit of a number.
 *
 * In the code, we have:
 *
 *    "nn" is a pointer to a big number,
 *    "nl" is the number of digits from nn,
 *    "d" is a digit.
 *
 */

static void
BnnDivideHelper(BigNum nn, BigNumLength nl, BigNum dd, BigNumLength dl);

void
BnnSetToZero(BigNum nn, BigNumLength nl) {
        /*
         * Sets all the specified digits of the BigNum to BN_ZERO (0).
         */

        BigNumLength d;

        for (d = 0; d < nl; ++d) {
                nn[d] = BN_ZERO;
        }
}

void
BnnAssign(BigNum mm, const BigNum nn, BigNumLength nl) {
        /*
         * Copies N => M
         */

        int     d;

        if ((mm < nn) || (mm > (nn + nl))) {
                /*
                 * no memory overlap using classic loop 
                 */
                for (d = 0; d < (int)nl; ++d) {
                        mm[d] = nn[d];
                }
        } else  if (mm > nn) {
                /*
                 * memory overlap, loop starting from most significant digit
                 */
                for (d = (int)(nl - 1); d >= 0; --d) {
                        mm[d] = nn[d];
                }
        }
}

void
BnnSetDigit(BigNum nn, BigNumDigit d) {
        /*
         * Sets a single digit of N to the passed value
         */

        *nn = d;
}

BigNumDigit
BnnGetDigit(const BigNum nn) {
        /*
         * Returns the single digit pointed by N
         */

        return (*nn);
}

BigNumLength
BnnNumDigits(const BigNum nn, BigNumLength nl) {
        /*
         * Returns the number of digits of N, not counting leading zeros
         */

        int     d;

        /*
         * loop starting from most significant digit
         */

        for (d = (int)(nl - 1); d >= 0; --d) {
                if (nn[d] != BN_ZERO) {
                        /*
                         * length = d+1
                         */
                        return ((BigNumLength)(d + 1));
                }
        }

        return ((BigNumLength)1);
}

BigNumLength
BnnNumLength(const BigNum nn, BigNumLength nl) {
        /*
         * Returns the number of bits of N, not counting leading zeros
         */

        const BigNumDigit d = nn[nl - 1];
        int     i;

        for (i = (int)(BN_DIGIT_SIZE - 1); i >= 0; --i) {
                if ((d & (BN_ONE << (BigNumLength)i)) != 0) {
                        return ((BigNumLength)(((nl-1) * BN_DIGIT_SIZE) + i+1));
                }
        }

        return (0);
}

BigNumLength
BnnNumCount(const BigNum nn, BigNumLength nl) {
        /*
         * Returns the count of bits set of N.
         */

        BigNumLength    count = 0;
        int     j;

        for (j = 0; j < (int)nl; ++j) {
                const BigNumDigit d = nn[j];
                int     i;

                for (i = (int)(BN_DIGIT_SIZE - 1); i >= 0; --i) {
                        if ((d & (BN_ONE << (BigNumLength)i)) != 0) {
                                ++count;
                        }
                }
        }

        return (count);
}

BigNumLength
BnnNumLeadingZeroBitsInDigit(BigNumDigit d) {
        /*
         * Returns the number of leading zero bits in a digit
         */

        BigNumDigit     mask = (BigNumDigit)(BN_ONE << (BN_DIGIT_SIZE - 1));
        BigNumLength    p;

        if (d == BN_ZERO) {
                return ((BigNumLength)BN_DIGIT_SIZE);
        }

        for (p = 0; (d & mask) == 0; ++p) {
                mask >>= 1;
        }

        return (p);
}

BigNumBool
BnnIsPower2(const BigNum nn, BigNumLength nl) {
        /*
         * Returns BN_TRUE iff nn is a power of 2.
         */

        BigNumLength i;
        BigNumLength nbits;
        BigNumDigit  d;

        /*
         *      The n-1 digits must be 0
         */

        for (i = 0; i < (nl - 1); ++i) {
                if (nn[i] != BN_ZERO) {
                        return (BN_FALSE);
                }
        }

        /*
         *      There must be only 1 bit set on the last Digit.
         */

        d     = nn[i];
        nbits = 0;

        for (i = 0; i < (BigNumLength)BN_DIGIT_SIZE; ++i) {
                if ((d & (BN_ONE << i)) != 0) {
                        if (nbits++ > 0) {
                                /*
                                 * More than two digits.
                                 */
                                return (BN_FALSE);
                        }
                }
        }

        return (BN_TRUE);
}

BigNumBool
BnnIsDigitZero(BigNumDigit d) {
        /*
         * Returns BN_TRUE iff digit = 0
         */

        return ((BigNumBool)(d == 0));
}

BigNumBool
BnnIsDigitNormalized(BigNumDigit d) {
        /*
         * Returns BN_TRUE iff Base/2 <= digit < Base
         * i.e. if digit's leading bit is 1
         */

        if ((d & (BN_ONE << (BN_DIGIT_SIZE - 1))) != 0) {
                return (BN_TRUE);
        } else  {
                return (BN_FALSE);
        }
}

BigNumBool
BnnIsDigitOdd(BigNumDigit d) {
        /*
         * Returns BN_TRUE iff digit is odd
         */

        if ((d & 1) != 0) {
                return (BN_TRUE);
        } else  {
                return (BN_FALSE);
        }
}

BigNumBool
BnnIsDigitEven(BigNumDigit d) {
        /*
         * Returns BN_TRUE iff digit is even
         */

        if ((d & 1) == 0) {
                return (BN_TRUE);
        } else  {
                return (BN_FALSE);
        }
}

BigNumCmp
BnnCompareDigits(BigNumDigit d1, BigNumDigit d2) {
        /*
         * Returns      BN_GT      if digit1 > digit2
         *              BN_EQ      if digit1 = digit2
         *              BN_LT      if digit1 < digit2
         */

        return ((BigNumCmp)((d1 > d2) ? BN_GT : (d1 == d2 ? BN_EQ : BN_LT)));
}

void
BnnComplement(BigNum nn, BigNumLength nl) {
        /*
         * Performs the computation BBase(N) - N - 1 => N
         */

        BigNumLength d;

        for (d = 0; d < nl; ++d) {
                nn[d] ^= BN_COMPLEMENT;
        }
}

void
BnnComplement2(BigNum nn, BigNumLength nl) {
        /*
         * Performs the computation neg(N) => N
         */

        BigNumDigit one = BN_ONE;

        /*
         * Initialize constants
         */

        BnnComplement(nn, nl);
        (void)BnnAdd(nn, nl, &one, (BigNumLength)1, BN_NOCARRY);
}

void
BnnAndDigits(BigNum n, BigNumDigit d) {
        /*
         * Returns the logical computation n[0] AND d in n[0]
         */

        *n &= d;
}

void
BnnOrDigits(BigNum n, BigNumDigit d) {
        /*
         * Returns the logical computation n[0] OR d2 in n[0].
         */
        *n |= d;
}

void
BnnXorDigits(BigNum n, BigNumDigit d) {
        /*
         * Returns the logical computation n[0] XOR d in n[0].
         */

        *n ^= d;
}

/*
 *      Shift operations
 */

BigNumDigit
BnnShiftLeft(BigNum mm, BigNumLength ml, BigNumLength nbits) {
        /*
         * Shifts  M  left by "nbits",  filling with 0s.  Returns the
         * leftmost  "nbits"  of  M in a digit.  Assumes 0 <= nbits <
         * BN_DIGIT_SIZE.
         */

        BigNumDigit res = BN_ZERO;

        if (nbits != 0) {
                BigNumLength rnbits = (BigNumLength)(BN_DIGIT_SIZE - nbits);
                BigNumLength evenlen = (ml & ~(BigNumLength)1);
                BigNumLength d;

                /*
                 * Loop is now unrooled two BigNumDigit at a time.
                 */

                for (d = 0; d < evenlen; ++d) {
                        BigNumDigit save0;
                        BigNumDigit save1;
                        save0 = mm[d];
                        mm[d] = (save0 << nbits) | res;
                        save1 = mm[++d];
                        mm[d] = (save1 << nbits) | (save0 >> rnbits);
                        res   = save1 >> rnbits;
                }

                if (ml != evenlen) {
                        BigNumDigit save = mm[d];
                        mm[d] = (save << nbits) | res;
                        res   = save >> rnbits;
                }
        }

        return (res);
}

BigNumDigit
BnnShiftRight(BigNum mm, BigNumLength ml, BigNumLength nbits) {
        /*
         * Shifts  M right by "nbits",  filling with 0s.  Returns the
         * rightmost  "nbits"  of M in a digit.  Assumes 0 <= nbits <
         * BN_DIGIT_SIZE.
         */

        BigNumDigit res = BN_ZERO;

        if (nbits != 0) {
                const BigNumLength lnbits = (BigNumLength)BN_DIGIT_SIZE - nbits;

                /*
                 * loop starting from most significant digit
                 */

                if ((ml & (BigNumLength)1) != (BigNumLength)0) {
                        /*
                         * Odd number of digits, start with most significant
                         * digit, then loop on even number of digits.
                         */
                        BigNumDigit save = mm[--ml];
                        mm[ml] = (save >> nbits); /* res==0, no need to | res */
                        res    = save << lnbits;
                }

                /*
                 * Loop is now unrooled two digits at a time.
                 */

                while (ml != (BigNumLength)0) {
                        BigNumDigit save0;
                        BigNumDigit save1;
                        save0  = mm[--ml];
                        mm[ml] = (save0 >> nbits) | res;
                        save1  = mm[--ml];
                        mm[ml] = (save1 >> nbits) | (save0 << lnbits);
                        res    = save1 << lnbits;
                }
        }

        return (res);
}

/*
 *      Additions
 */

BigNumCarry
BnnAddCarry(BigNum nn, BigNumLength nl, BigNumCarry carryin) {
        /*
         * Performs the sum N + CarryIn => N. Returns the CarryOut.
         */

        if (carryin == BN_NOCARRY) {
                return (BN_NOCARRY);
        } else  if (nl == 0) {
                return (BN_CARRY);
        } else  {
                BigNumLength d;

                for (d = 0; d < nl; ++d) {
                        if (++nn[d] != 0) {
                                return (BN_NOCARRY);
                        }
                }

                return (BN_CARRY);
        }
}

BigNumCarry
BnnAdd(BigNum mm,
       BigNumLength ml,
       const BigNum nn,
       BigNumLength nl,
       BigNumCarry carryin) {
        /*
         * Performs the sum M + N + CarryIn => M. Returns the CarryOut.
         * Assumes Size(M) >= Size(N).
         */

        BigNumProduct c = (BigNumProduct)carryin;
        BigNumLength i;

        for (i = 0; i < nl; ++i) {
                BigNumProduct save = (BigNumProduct)*mm;

                c   += save;
                if (c < save) {
                        *(mm++) = nn[i];
                        c       = (BigNumProduct)1;
                } else  {
                        save    = (BigNumProduct)nn[i];
                        c      += save;
                        *(mm++) = (BigNumDigit)c;
                        c       = (BigNumProduct)((c < save) ? 1 : 0);
                }
        }

        return (BnnAddCarry(mm, ml-nl, ((c == 0) ? BN_NOCARRY : BN_CARRY)));
}

/*
 *      Subtraction
 */

BigNumCarry
BnnSubtractBorrow(BigNum nn, BigNumLength nl, BigNumCarry carryin) {
        /*
         * Performs the difference N + CarryIn - 1 => N.
         * Returns the CarryOut.
         */

        if (carryin == BN_CARRY) {
                return (BN_CARRY);
        } else  if (nl == 0) {
                return (BN_NOCARRY);
        } else  {
                BigNumLength d;

                for (d = 0; d < nl; ++d) {
                        if (nn[d]-- != 0) {
                                return (BN_CARRY);
                        }
                }

                return (BN_NOCARRY);
        }
}

BigNumCarry
BnnSubtract(BigNum mm,
            BigNumLength ml,
            const BigNum nn,
            BigNumLength nl,
            BigNumCarry carryin) {
        /*
         * Performs the difference M - N + CarryIn - 1 => M.
         * Returns the CarryOut. Assumes Size(M) >= Size(N).
         */

        BigNumProduct   c = (BigNumProduct)((carryin == BN_CARRY) ? 1 : 0);
        BigNumDigit     invn;
        BigNumProduct   save;
        BigNumLength    i;

        for (i = 0; i < nl; ++i) {
                save = (BigNumProduct)*mm;
                invn = nn[i] ^ BN_COMPLEMENT;
                c += save;

                if (c < save) {
                        *(mm++) = invn;
                        c       = (BigNumProduct)1;
                } else  {
                        c      += invn;
                        *(mm++) = (BigNumDigit)c;
                        c       = (BigNumProduct)((c < invn) ? 1 : 0);
                }
        }

        if (c == 0) {
                return (BnnSubtractBorrow(mm, ml-nl, BN_NOCARRY));
        } else  {
                return (BnnSubtractBorrow(mm, ml-nl, BN_CARRY));
        }
}

/*
 *      Multiplication
 */

#define LOW(x)   (BigNumDigit)(x & ((BN_ONE << (BN_DIGIT_SIZE / 2)) - 1))
#define HIGH(x)  (BigNumDigit)(x >> (BN_DIGIT_SIZE / 2))
#define L2H(x)   (BigNumDigit)(x << (BN_DIGIT_SIZE / 2))

#define UPDATE_S(c, V, X3) c += V; X3 += (int)(c < V);

BigNumCarry
BnnMultiplyDigit(BigNum pp,
                 BigNumLength pl,
                 const BigNum mm,
                 BigNumLength ml,
                 BigNumDigit d) {
        /*
         * Performs the product:
         * Q = P + M * d
         * BB = BBase(P)
         * Q mod BB => P
         * Q div BB => CarryOut
         * Returns the CarryOut.
         * Assumes Size(P) >= Size(M) + 1.
         */

        BigNumLength    i;
        BigNumProduct   c = 0;
        BigNumDigit     save;

        if (d == BN_ZERO) {
                return (BN_NOCARRY);
        }

        if (d == BN_ONE) {
                return (BnnAdd(pp, pl, mm, ml, BN_NOCARRY));
        }

        for (i = 0; i < ml; ++i) {
                BigNumDigit     Lm;
                BigNumDigit     Hm;
                BigNumDigit     Ld;
                BigNumDigit     Hd;
                BigNumDigit     X0;
                BigNumDigit     X1;
                BigNumDigit     X2;
                BigNumDigit     X3;

                Ld = LOW(d);
                Hd = HIGH(d);
                Lm = LOW(mm[i]);
                Hm = HIGH(mm[i]);
                X0 = Ld * Lm;
                X1 = Ld * Hm;
                X2 = Hd * Lm;
                X3 = Hd * Hm;

                UPDATE_S(c, X0,      X3);
                UPDATE_S(c, L2H(X1), X3);
                UPDATE_S(c, L2H(X2), X3);
                UPDATE_S(c, *pp,     X3);

                --pl;
                *(pp++) = (BigNumDigit)c;
                c = X3 + HIGH(X1) + HIGH(X2);
        }

        if (pl == 0) {
                return (BN_NOCARRY);
        }

        save    = *pp;
        c      += save;
        *pp     = (BigNumDigit)c;

        if (c >= save) {
                return (BN_NOCARRY);
        }

        ++pp;
        --pl;

        while (pl != 0 && (++(*pp++)) == 0) {
                pl--;
        }

        return ((pl != 0) ? BN_NOCARRY : BN_CARRY);
}

/*
 * Division
 */

/* xh:xl -= yh:yl */

#define SUB(xh, xl, yh, yl)                                     \
        xh -= yh + (int)(yl > xl);                              \
        xl -= yl;

BigNumDigit
BnnDivideDigit(BigNum qq, BigNum nn, BigNumLength nl, BigNumDigit d) {
        /*
         * Performs the quotient: N div d => Q
         * Returns R = N mod d
         * Assumes leading digit of N < d, and d > 0.
         */

        BigNumLength    k;
        BigNumLength    orig_nl;
        BigNumDigit     rh;     /* Two halves of current remainder */
        BigNumDigit     rl;     /* Correspond to quad above        */
        BigNumDigit     ph;
        BigNumDigit     pl;     /* product of c and qa             */
        BigNumDigit     ch;
        BigNumDigit     cl;
        BigNumDigit     prev_qq;

        /*
         * Normalize divisor
         */

        k = BnnNumLeadingZeroBitsInDigit(d);

        if (k != 0) {
                prev_qq = qq[-1];
                orig_nl = nl;
                d <<= k;
                (void)BnnShiftLeft(nn, nl, k);
        } else  {
                prev_qq = 0;
                orig_nl = 0;
        }

        nn += nl;
        nl--;
        qq += nl;

        ch = HIGH(d);
        cl = LOW(d);

        /*
         * At this point ch can't be == 0; d has been shifted by k
         * (the number of leading 0).
         */

        rl = *(--nn);

        while (nl-- != 0) {
                BigNumDigit qa; /* Current appr. to quotient */

                rh = rl;
                rl = *(--nn);
                qa = rh / ch;   /* appr. quotient */

                /*
                 * Compute ph, pl
                 */

                pl = cl * qa;
                ph = ch * qa;
                ph += HIGH(pl);
                pl = L2H(pl);

                /*
                 * While ph:pl > rh:rl, decrement qa, adjust qh:ql
                 */

                while ((ph > rh) || ((ph == rh) && (pl > rl))) {
                        qa--;
                        SUB(ph, pl, ch, L2H(cl));
                }

                SUB(rh, rl, ph, pl);

                /*
                 * Top half of quotient is correct; save it
                 */

                *(--qq) = L2H(qa);
                qa = (L2H(rh) | HIGH(rl)) / ch;

                /*
                 * Approx low half of q. Compute ph, pl, again
                 */

                pl = cl * qa;
                ph = ch * qa;
                ph += HIGH(pl);
                pl = LOW(pl) | L2H(LOW(ph));
                ph = HIGH(ph);

                /*
                 * While ph:pl > rh:rl, decrement qa, adjust qh:ql
                 */

                while ((ph > rh) || ((ph == rh) && (pl > rl))) {
                        qa--;
                        SUB(ph, pl, 0, d);
                }

                /*
                 * Subtract ph:pl from rh:rl; we know rh will be 0
                 */

                rl -= pl;
                *qq |= qa;
        }

        /*
         * Denormalize dividend
         */

        if (k != 0) {
                if ((qq > nn) && (qq < &nn[orig_nl])) {
                        /*
                         * Overlap between qq and nn. Care of *qq!
                         */
                        orig_nl = (BigNumLength)(qq - nn);
                        (void)BnnShiftRight(nn, orig_nl, k);
                        nn[orig_nl - 1] = prev_qq;
                } else  if (qq == nn) {
                        (void)BnnShiftRight(&nn[orig_nl-1], (BigNumLength)1, k);
                } else  {
                        (void)BnnShiftRight(nn, orig_nl, k);
                }
        }
        return (rl >> k);
}

BigNumBool
BnnIsZero(const BigNum nn, BigNumLength nl) {
        /*
         * Returns BN_TRUE iff N = 0
         */

        if ((BnnNumDigits(nn, nl) == (BigNumLength)1)
            && (nl == 0 || BnnIsDigitZero(*nn) != BN_FALSE)) {
                return (BN_TRUE);
        } else  {
                return (BN_FALSE);
        }
}

BigNumCarry
BnnMultiply(BigNum pp,
            BigNumLength pl,
            const BigNum mm,
            BigNumLength ml,
            const BigNum nn,
            BigNumLength nl) {
        /*
         * Performs the product:
         *    Q = P + M * N
         *    BB = BBase(P)
         *    Q mod BB => P
         *    Q div BB => CarryOut
         *
         * Returns the CarryOut.
         *
         * Assumes:
         *    Size(P) >= Size(M) + Size(N), 
         *    Size(M) >= Size(N).
         */

        BigNumLength i;
        BigNumCarry  c = BN_NOCARRY;

        /*
         * Multiply one digit at a time
         */

        for (i = 0; i < nl; ++i) {
                if (BnnMultiplyDigit(&pp[i], pl--, mm, ml, nn[i]) == BN_CARRY) {
                        c = BN_CARRY;
                }
        }

        return (c);
}

#define BNN_COMPARE_DIGITS(d1, d2) (d1 == d2)

static void
BnnDivideHelper(BigNum nn, BigNumLength nl, BigNum dd, BigNumLength dl) {
        /*
         * In-place division.
         *
         * Input (N has been EXTENDED by 1 PLACE; D is normalized):
         *      +-----------------------------------------------+----+
         *      |                       N                         EXT|
         *      +-----------------------------------------------+----+
         *
         *      +-------------------------------+
         *      |               D              1|
         *      +-------------------------------+
         *
         * Output (in place of N):
         *      +-------------------------------+---------------+----+
         *      |               R               |          Q         |
         *      +-------------------------------+---------------+----+
         *
         * Assumes:
         *    N > D
         *    Size(N) > Size(D)
         *    last digit of N < last digit of D
         *    D is normalized (Base/2 <= last digit of D < Base)
         */

        BigNumDigit     DDigit;
        BigNumDigit     BaseMinus1;
        BigNumDigit     QApp;
        BigNumLength    ni;

        /*
         * Initialize constants
         */

        /*
         * BaseMinus1 = BN_COMPLEMENT;
         * ->
         *      BnnSetDigit(&BaseMinus1, BN_ZERO);
         *      BnnComplement(&BaseMinus1, (BigNumLength)1);
         */

        BaseMinus1 = BN_COMPLEMENT;

        /*
         * Save the most significant digit of D
         */

        DDigit = BN_ZERO;
        BnnAssign(&DDigit, dd + dl - 1, (BigNumLength)1);

        /*
         * Replace D by Base - D
         */

        BnnComplement(dd, dl);
        (void)BnnAddCarry(dd, dl, BN_CARRY);

        /*
         * For each digit of the divisor, from most significant to least:
         */

        QApp = BN_ZERO;
        nl += 1;
        ni = nl - dl;

        while (ni != 0) {
                /*
                 * Compute the approximate quotient
                 */

                ni--;
                nl--;

                /*
                 * If first digits of numerator and denominator are the same,
                 */

                if (BNN_COMPARE_DIGITS(*(nn + nl), DDigit)) {
                        /*
                         * Use "Base - 1" for the approximate quotient
                         */
                        BnnAssign(&QApp, &BaseMinus1, (BigNumLength)1);
                } else  {
                        /*
                         * Divide  the  first  2  digits  of N by the
                         * first digit of D
                         */
                        (void)BnnDivideDigit(&QApp,
                                             nn + nl - 1,
                                             (BigNumLength)2,
                                             DDigit);
                }

                /*
                 * Compute the remainder
                 */

                (void)BnnMultiplyDigit(nn + ni, dl + 1, dd, dl, QApp);

                /*
                 * Correct the approximate quotient, in case it was too large
                 */

                while (!BNN_COMPARE_DIGITS(*(nn + nl), QApp)) {
                        /*
                         * Subtract D from N
                         */

                        (void)BnnSubtract(nn+ni, dl + 1, dd, dl, BN_CARRY);

                        /*
                         * Q -= 1
                         */

                        (void)BnnSubtractBorrow(&QApp,
                                                (BigNumLength)1,
                                                BN_NOCARRY);
                }
        }

        /*
         * Restore original D
         */

        BnnComplement(dd, dl);
        (void)BnnAddCarry(dd, dl, BN_CARRY);
}

void
BnnDivide(BigNum nn, BigNumLength nl, BigNum dd, BigNumLength dl) {
        /*
         * Performs the quotient:
         *    N div D => high-order bits of N, starting at N[dl]
         *    N mod D => low-order dl bits of N
         *
         * Assumes 
         *    Size(N) > Size(D),
         *    last digit of N < last digit of D (if N > D).
         */

        BigNumLength    nshift;

        /*
         * Take care of easy cases first
         */

        switch (BnnCompare(nn, nl, dd, dl)) {
        case BN_LT:     /* n < d */
                /* nop */                              /* N => R */
                BnnSetToZero(nn + dl, nl - dl);        /* 0 => Q */
                return;
        case BN_EQ:     /* n == d */
                BnnSetToZero(nn, nl);                  /* 0 => R */
                BnnSetDigit(nn + nl - 1, BN_ONE);      /* 1 => Q */
                return;
        case BN_GT:     /* n > d */
                /*
                 * If divisor is just 1 digit, use a special divide
                 */

                if (dl == (BigNumLength)1) {
                        /*
                         * note: nn+1 = nn+dl
                         */

                        *nn = BnnDivideDigit(nn + 1, nn, nl, *dd);

                        /*
                         * Otherwise, divide one digit at a time
                         */
                } else  {
                        /*
                         * Normalize
                         */

                        nshift = BnnNumLeadingZeroBitsInDigit(*(dd + dl - 1));
                        (void)BnnShiftLeft(dd, dl, nshift);
                        (void)BnnShiftLeft(nn, nl, nshift);

                        /*
                         * Divide
                         */

                        BnnDivideHelper(nn, nl - 1, dd, dl);

                        /*
                         * Unnormalize
                         */

                        (void)BnnShiftRight(dd, dl, nshift);
                        (void)BnnShiftRight(nn, dl, nshift);

                        /*
                         * note: unnormalize N <=> unnormalize R (with R < D)
                         */
                }
        }
}

BigNumCmp
BnnCompare(const BigNum mm,
           BigNumLength ml,
           const BigNum nn,
           BigNumLength nl) {
        /*
         * return
         *              BN_GT   iff M > N
         *              BN_EQ   iff N = N
         *              BN_LT   iff N < N
         */

        ml = BnnNumDigits(mm, ml);
        nl = BnnNumDigits(nn, nl);

        if (ml != nl) {
                return ((ml > nl) ? BN_GT : BN_LT);
        } else  {
                int     d;

                /*
                 * loop starting from most significant digit
                 */

                for (d = (int)(nl - 1); d >= 0; --d) {
                        if (mm[d] != nn[d]) {
                                return ((mm[d] > nn[d]) ? BN_GT : BN_LT);
                        }
                }

                return (BN_EQ);
        }
}

/*                             bigz.c                                */

/*
 * Several conventions are used in the commentary:
 *    A "BigZ" is the name for an arbitrary-precision signed integer.
 *    Capital letters (e.g., "Z") are used to refer to the value of BigZs.
 */

#define BZMAXINT                ((BzInt)((~(BzUInt)0) >> 1))
#define BZMAXUINT               (~(BzUInt)0)

#define BzFreeIf(cond, ptr)     if (cond) BzFree(ptr)

/*
 *      See ./etc/hextable.c if you need to change BigHexToDigit tables.
 */

#if     !defined(OLEBCDIC)
/*
 * ASCII character class table
 */
static const int BigHexToDigit[] = {
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
         0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1,
        -1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
        25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, -1, -1, -1, -1, -1,
        -1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
        25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, -1, -1, -1, -1, -1
};

#define CTOI(c) ((((unsigned int)c) < (unsigned int)127) \
                 ? BigHexToDigit[(unsigned int)c]      \
                 : -1)

#else
/*
 * EBCDIC character class table
 */
static const int BigHexToDigit[] = {
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, 10, 11, 12, 13, 14, 15, 16, 17, 18, -1, -1, -1, -1, -1, -1,
        -1, 19, 20, 21, 22, 23, 24, 25, 26, 27, -1, -1, -1, -1, -1, -1,
        -1, -1, 28, 29, 30, 31, 32, 33, 34, 35, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, 10, 11, 12, 13, 14, 15, 16, 17, 18, -1, -1, -1, -1, -1, -1,
        -1, 19, 20, 21, 22, 23, 24, 25, 26, 27, -1, -1, -1, -1, -1, -1,
        -1, -1, 28, 29, 30, 31, 32, 33, 34, 35, -1, -1, -1, -1, -1, -1,
         0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1
};

#define CTOI(c) ((((unsigned int)c) < (unsigned int)255) \
                 ? BigHexToDigit[(unsigned int)c]        \
                 : -1)
#endif

static BzSign   BzGetOppositeSign(const BigZ z);

#if     defined(BZ_DEBUG)
static void     BzShowBits(BigNumDigit n);
static void     BzShowUnsingnedInt(unsigned int n);

static void
BzShowBits(BigNumDigit n) {
        int     i;

        for (i = (int)(BN_DIGIT_SIZE - 1); i >= 0; i--) {
                if ((n & (BN_ONE << (unsigned int)i)) != 0) {
                        (void)printf("1");
                } else  {
                        (void)printf("0");
                }
        }
}

static void
BzShowUnsingnedInt(unsigned int n) {
        /*
         * Deal with different integer sizes. This avoid to build format
         * string dynamically which results to potential buffer overflow.
         * This also makes splint happy.
         */

        size_t size = sizeof(n);

        switch (size) {
        case  2: (void)printf("%04x",  n); break;
        case  4: (void)printf("%08x",  n); break;
        case  8: (void)printf("%016x", n); break;
        case 16: (void)printf("%032x", n); break;
        default: (void)printf("%016x", n); break;
        }
}

#define BZ_INT_CHUNKS   ((unsigned int)8) /* Assume BigNumDigit < (8 * int) */

void
BnDebug(const char *m,
        const BzChar *bzstr,
        const BigNum n,
        BigNumLength nl,
        BzSign       sign) {
        BigNumLength l;
        BzChar       c;

        if (m != NULL) {
                (void)printf("%-20s\n", m);
        }

        (void)printf("\t: BigZ = %s at 0x%p, ", (char *)bzstr, n);
        (void)printf("digit = %d, word = %d bytes\n",
                      (int)BN_DIGIT_SIZE,
                      (int)sizeof(void *));
        (void)printf("\t: <-- high %02d digit(s) low -->\n", (int)nl);

        if (sign == BZ_ZERO) {
                c = (BzChar)'0';
        } else  if (sign == BZ_MINUS) {
                c = (BzChar)'-';
        } else  {
                c = (BzChar)'+';
        }

        (void)printf("      %c : ", c);

        for (l = nl; l-- != 0;) {
                BigNumDigit d = n[l];
                unsigned int dsize = (unsigned int)sizeof(d);
                unsigned int isize = (unsigned int)sizeof(dsize); /* int */

                (void)printf("|");

                if ((dsize > isize) && ((dsize / isize) <= BZ_INT_CHUNKS)) {
                        /*
                         * sizeof(BigNumDigit) > sizeof(int).
                         * (can be 64bit BigNumDigit and 32bit int).
                         * NOTE: this code also works if isize == dsize.
                         */
                        unsigned int chunk[BZ_INT_CHUNKS];
                        unsigned int mask;
                        unsigned int shift;
                        unsigned int i;

                        mask  = ~((unsigned int)0); /* 0xff..ff */
                        shift = (unsigned int)(isize * BN_BYTE_SIZE);

                        /*
                         *      Split BigNumDigit in int chunks.
                         */

                        for (i = 0; i < (dsize / isize); ++i) {
                                chunk[i] = (unsigned int)(d & mask);
                                d >>= shift;
                        }

                        while (i-- != 0) {
                                BzShowUnsingnedInt(chunk[i]);
                        }
                } else  {
                        /*
                         * sizeof(BigNumDigit) <= sizeof(int).
                         */
                        BzShowUnsingnedInt((unsigned int)d);
                }
        }
        (void)printf("|\n");

        (void)printf("      %c : ", c);

        for (l = nl; l-- != 0;) {
                (void)printf("|");
                BzShowBits(n[l]);
        }

        (void)printf("|\n");
}

void
BzDebug(const char *m, const BigZ y) {
        BzChar * s = BzToString(y, (BigNumDigit)10, 0);

        BnDebug(m, s, BzToBn(y), BzNumDigits(y), BzGetSign(y));
        BzFreeString(s);
}
#endif

const char *
BzVersion(void) {
        return (BZ_VERSION);
}

/*
 *      constants used by BzToString() and BzFromString()
 */

#define BZ_MIN_BASE     2
#define BZ_MAX_BASE     36

/*
 *      following table is computed using:
 *
 *      for (i = 1; i <= BZ_MAX_BASE; ++i) {
 *              printf("\t%16.16f, // log(%2d)\n", log((double)i), i);
 *      }
 */

static const double BzLog[] = {
        0.0000000000000000,
        0.0000000000000000, /* log(1) */
        0.6931471805599453, /* log(2) */
        1.0986122886681098, /* log(3) */
        1.3862943611198906, /* log(4) */
        1.6094379124341003, /* log(5) */
        1.7917594692280550, /* log(6) */
        1.9459101490553132, /* log(7) */
        2.0794415416798357, /* log(8) */
        2.1972245773362196, /* log(9) */
        2.3025850929940459, /* log(10) */
        2.3978952727983707, /* log(11) */
        2.4849066497880004, /* log(12) */
        2.5649493574615367, /* log(13) */
        2.6390573296152584, /* log(14) */
        2.7080502011022101, /* log(15) */
        2.7725887222397811, /* log(16) */
        2.8332133440562162, /* log(17) */
        2.8903717578961645, /* log(18) */
        2.9444389791664403, /* log(19) */
        2.9957322735539909, /* log(20) */
        3.0445224377234230, /* log(21) */
        3.0910424533583161, /* log(22) */
        3.1354942159291497, /* log(23) */
        3.1780538303479458, /* log(24) */
        3.2188758248682006, /* log(25) */
        3.2580965380214821, /* log(26) */
        3.2958368660043291, /* log(27) */
        3.3322045101752038, /* log(28) */
        3.3672958299864741, /* log(29) */
        3.4011973816621555, /* log(30) */
        3.4339872044851463, /* log(31) */
        3.4657359027997265, /* log(32) */
        3.4965075614664802, /* log(33) */
        3.5263605246161616, /* log(34) */
        3.5553480614894135, /* log(35) */
        3.5835189384561099, /* log(36) */
};

BigZ
BzCreate(BigNumLength Size) {
        /*
         * Allocates a zeroed BigZ of the desired size.
         */

        BigZ    z;
        size_t  chunk;

        /*
         * Compute BigZ allocation size taking care of aligment.
         */
        chunk = sizeof(struct BigZStruct)
              - (BZ_DUMMY_SIZE * sizeof(BigNumDigit))
              + (Size * sizeof(BigNumDigit));

        if ((z = (BigZ)(BzAlloc(chunk))) != BZNULL) {
                /*
                 * reset digits
                 */

                BnnSetToZero(BzToBn(z), Size);

                /*
                 * init header
                 */

                BzSetSize(z, Size);
                BzSetSign(z, BZ_ZERO);
        }

        return (z);
}

BigNumLength
BzNumDigits(const BigZ z) {
        /*
         * Returns the number of digits used by z.
         */

        return (BnnNumDigits(BzToBn(z), BzGetSize(z)));
}

BigNumLength
BzLength(const BigZ z) {
        /*
         * Returns the number of bits used by z.
         */

        BigNumLength nl;

        switch (BzGetSign(z)) {
        case BZ_MINUS :
                nl = BnnNumLength(BzToBn(z), BzNumDigits(z));
                if (BnnIsPower2(BzToBn(z), BzNumDigits(z)) == BN_TRUE) {
                        return (nl - 1);
                } else  {
                        return (nl);
                }
        case BZ_PLUS :
                return (BnnNumLength(BzToBn(z), BzNumDigits(z)));
        default :
                return ((BigNumLength)0);
        }
}

BigZ
BzCopy(const BigZ z) {
        /*
         * Creates a copy of the passed BigZ.
         */

        BigZ            y;
        BigNumLength    zl;

        zl = BzNumDigits(z);

        if ((y = BzCreate(zl)) != BZNULL) {
                /*
                 * copy the digits
                 */
                BnnAssign(BzToBn(y), BzToBn(z), zl);

                /*
                 * copy the header WITHOUT the size !!
                 */
                BzSetSign(y, BzGetSign(z));
        }

        return (y);
}

static BzSign
BzGetOppositeSign(const BigZ z) {
        switch (BzGetSign(z)) {
        case BZ_MINUS:
                return (BZ_PLUS);
        case BZ_ZERO:
                return (BZ_ZERO);
        default:
                return (BZ_MINUS);
        }
}

BigZ
BzNegate(const BigZ z) {
        /*
         * Negates the passed BigZ.
         */

        BigZ    y;

        if ((y = BzCopy(z)) != BZNULL) {
                switch (BzGetSign(z)) {
                case BZ_MINUS:
                        BzSetSign(y, BZ_PLUS);
                        break;
                case BZ_ZERO:
                        BzSetSign(y, BZ_ZERO);
                        break;
                case BZ_PLUS:
                        BzSetSign(y, BZ_MINUS);
                        break;
                }
        }

        return (y);
}

BigZ
BzAbs(const BigZ z) {
        /*
         * Takes the absolute value of the passed BigZ.
         */

        BigZ    y;

        if ((y = BzCopy(z)) != BZNULL) {
                if (BzGetSign(z) == BZ_MINUS) {
                        BzSetSign(y, BZ_PLUS);
                }
        }

        return (y);
}

BzCmp
BzCompare(const BigZ y, const BigZ z) {
        /*
         * Returns BZ_GT        if Y > Z,
         *         BZ_LT        if Y < Z,
         *         BZ_EQ        otherwise.
         */

        if (BzGetSign(y) > BzGetSign(z)) {
                return (BZ_GT);
        } else  if (BzGetSign(y) < BzGetSign(z)) {
                return (BZ_LT);
        } else  if (BzGetSign(y) == BZ_PLUS) {
                return ((BzCmp)BnnCompare(BzToBn(y), BzGetSize(y),
                                           BzToBn(z), BzGetSize(z)));
        } else  if (BzGetSign(y) == BZ_MINUS) {
                return ((BzCmp)BnnCompare(BzToBn(z), BzGetSize(z),
                                           BzToBn(y), BzGetSize(y)));
        } else  {
                return (BZ_EQ);
        }
}

BigZ
BzAdd(const BigZ y, const BigZ z) {
        /*
         * Returns Y + Z.
         */

        BigZ            n;
        BigNumLength    yl;
        BigNumLength    zl;

        yl = BzNumDigits(y);
        zl = BzNumDigits(z);

        if (BzGetSign(y) == BzGetSign(z)) {
                /*
                 * Add magnitudes if signs are the same
                 */
                switch (BnnCompare(BzToBn(y), yl, BzToBn(z), zl)) {
                case BN_EQ:
                case BN_GT:     /* |Y| >= |Z| */
                        if ((n = BzCreate(yl + 1)) != BZNULL) {
                                BnnAssign(BzToBn(n), BzToBn(y), yl);
                                (void)BnnAdd(BzToBn(n),
                                             yl + 1,
                                             BzToBn(z),
                                             zl,
                                             BN_NOCARRY);
                                BzSetSign(n, BzGetSign(y));
                        }
                        break;
                default:        /* BN_LT: |Y| < |Z| */
                        if ((n = BzCreate(zl+1)) != BZNULL) {
                                BnnAssign(BzToBn(n), BzToBn(z), zl);
                                (void)BnnAdd(BzToBn(n),
                                             zl + 1,
                                             BzToBn(y),
                                             yl,
                                             BN_NOCARRY);
                                BzSetSign(n, BzGetSign(z));
                        }
                        break;
                }
        } else  {
                /*
                 * Subtract magnitudes if signs are different
                 */
                switch (BnnCompare(BzToBn(y), yl, BzToBn(z), zl)) {
                case BN_EQ:     /* Y = -Z */
                        n = BzCreate((BigNumLength)1);
                        break;
                case BN_GT:     /* |Y| > |Z| */
                        if ((n = BzCreate(yl)) != BZNULL) {
                            BnnAssign(BzToBn(n), BzToBn(y), yl);
                            (void)BnnSubtract(BzToBn(n),
                                               yl,
                                               BzToBn(z),
                                               zl,
                                               BN_CARRY);
                            BzSetSign(n, BzGetSign(y));
                        }
                        break;

                default:        /* BN_LT: |Y| < |Z| */
                        if ((n = BzCreate(zl)) != BZNULL) {
                            BnnAssign(BzToBn(n), BzToBn(z), zl);
                            (void)BnnSubtract(BzToBn(n),
                                               zl,
                                               BzToBn(y),
                                               yl,
                                               BN_CARRY);
                            BzSetSign(n, BzGetSign(z));
                        }
                        break;
                }
        }

        return (n);
}

BigZ
BzSubtract(const BigZ y, const BigZ z) {
        /*
         * Returns Y - Z.
         */

        BigZ    diff;

        if (y != z) {
                BzSetSign(z, BzGetOppositeSign(z));
                diff = BzAdd(y, z);
                BzSetSign(z, BzGetOppositeSign(z));
        } else  {
                diff = BzFromInteger(0);
        }

        return (diff);
}

BigZ
BzMultiply(const BigZ y, const BigZ z) {
        /*
         * Returns Y * Z.
         */

        BigZ            n;
        BigNumLength    yl;
        BigNumLength    zl;

        if (BzGetSign(y) == BZ_ZERO || BzGetSign(z) == BZ_ZERO) {
                return (BzFromInteger(0));
        }

        yl = BzNumDigits(y);
        zl = BzNumDigits(z);

        if ((n = BzCreate(yl + zl)) == BZNULL) {
                return (BZNULL);
        }

        (void)BnnMultiply(BzToBn(n), yl + zl, BzToBn(y), yl, BzToBn(z), zl);

        if (BzGetSign(y) == BzGetSign(z)) {
                BzSetSign(n, BZ_PLUS);
        } else  {
                BzSetSign(n, BZ_MINUS);
        }

        return (n);
}

BigZ
BzDivide(const BigZ y, const BigZ z, BigZ *r) {
        /*
         * Returns Y div Z => Q
         * Sets    Y mod Z => R,
         *
         * such that Y = ZQ + R
         */

        BigZ            q;
        BigNumLength    yl;
        BigNumLength    zl;
        BigNumLength    ql;
        BigNumLength    rl;

        if (BzGetSign(z) == BZ_ZERO) {
                return (BZNULL);
        }

        yl = BzNumDigits(y);
        zl = BzNumDigits(z);
        ql = (BigNumLength)MaxInt((int)yl - (int)zl + 1, 1) + 1;
        rl = (BigNumLength)MaxInt(zl, yl) + 1;

        /*
         * Set up quotient, remainder
         */

        if ((q = BzCreate(ql)) == BZNULL) {
                return (BZNULL);
        }

        if ((*r = BzCreate(rl)) == BZNULL) {
                BzFree(q);
                return (BZNULL);
        }

        BnnAssign(BzToBn(*r), BzToBn(y), yl);

        /*
         * Do the division
         */

        BnnDivide(BzToBn(*r), rl, BzToBn(z), zl);
        BnnAssign(BzToBn(q), BzToBn(*r) + zl, rl - zl);
        BnnSetToZero(BzToBn(*r) + zl, rl - zl);
        rl = zl;

        /*
         * Correct the signs, adjusting the quotient and remainder
         */

        if (BnnIsZero(BzToBn(*r), rl) == BN_FALSE) {
                /*
                 * R<>0
                 */
                if (BzGetSign(y) != BzGetSign(z)) {
                        /*
                         * (Z-R) => R
                         */
                        BnnComplement(BzToBn(*r), rl);
                        (void)BnnAdd(BzToBn(*r), rl, BzToBn(z), zl, BN_CARRY);
                        /*
                         * (Q+1) => Q
                         */
                        (void)BnnAddCarry(BzToBn(q), ql, BN_CARRY);
                }
                /*
                 * The sign of the result (mod) is the sign of z.
                 */
                BzSetSign(*r, BzGetSign(z));
        } else  {
                /*
                 * Set sign to BZ_ZERO
                 * (already made by BzCreate but makes it clear)
                 */
                BzSetSign(*r, BZ_ZERO);
        }

        /*
         * Set the sign of the quotient.
         */

        if (BnnIsZero(BzToBn(q), ql) == BN_TRUE) {
                BzSetSign(q, BZ_ZERO);
        } else  if (BzGetSign(y) == BzGetSign(z)) {
                BzSetSign(q, BZ_PLUS);
        } else  {
                BzSetSign(q, BZ_MINUS);
        }

        return (q);
}

BigZ
BzDiv(const BigZ y, const BigZ z) {
        /*
         * Returns div(Y, Z).
         */

        BigZ    q;
        BigZ    r = BZNULL;

        q = BzDivide(y, z, &r);

        if (r != BZNULL) {
                BzFree(r);
        }

        return (q);
}

BigZ
BzTruncate(const BigZ y, const BigZ z) {
        /*
         * Returns truncate(Y, Z).
         */

        BigZ            q;
        BigZ            r = BZNULL;
        BigNumLength    ql;

        q  = BzDivide(y, z, &r);

        if (r == BZNULL) {
                /*
                 * This should never happend.
                 */
                BzFree(q);
                return (BZNULL);
        }

        ql = BzNumDigits(q);

        if (BzGetSign(q) == BZ_MINUS && BzGetSign(r) != BZ_ZERO) {
                /*
                 *      Q < 0, R <> 0, 2*R>= Z : Q-1 => Q
                 */
                (void)BnnSubtractBorrow(BzToBn(q), ql, BN_NOCARRY);

                if (BnnIsZero(BzToBn(q), ql) == BN_TRUE) {
                        BzSetSign(q, BZ_ZERO);
                }
        } else  if (BnnIsZero(BzToBn(q), ql) == BN_TRUE &&
                    BzGetSign(y) == BzGetSign(z)) {
                /*
                 *      Q == 0, sign(Y) == sign(Z) : 0 => Q
                 */
                BzFree(q);
                q = BzFromInteger(0);
        }

        BzFree(r);

        return (q);
}

BigZ
BzFloor(const BigZ y, const BigZ z) {
        /*
         * Returns floor(Y, Z).
         */

        BigZ    q;
        BigZ    r = BZNULL;

        q = BzDivide(y, z, &r);

        if (r != BZNULL) {
                BzFree(r);
        }

        return (q);
}

BigZ
BzCeiling(const BigZ y, const BigZ z) {
        /*
         * Returns ceiling(Y, Z).
         */

        BigZ            q;
        BigZ            r = BZNULL;
        BigNumLength    ql;

        if ((q = BzDivide(y, z, &r)) == BZNULL) {
                return (BZNULL);
        }

        if (r == BZNULL) {
                /*
                 * This should never happend.
                 */
                BzFree(q);
                return (BZNULL);
        }

        ql = BzNumDigits(q);

        if (BzGetSign(q) == BZ_PLUS && BzGetSign(r) != BZ_ZERO) {
                /*
                 *      Q > 0, R <> 0 : Q+1 => Q
                 */
                BigNumDigit one = BN_ONE;
                (void)BnnAdd(BzToBn(q), ql, &one, (BigNumLength)1, BN_NOCARRY);
        } else  if (BzGetSign(q) == BZ_MINUS && BzGetSign(r) != BZ_ZERO) {
                /*
                 *      Q < 0, R <> 0 : Q-1 => Q
                 */
                (void)BnnSubtractBorrow(BzToBn(q), ql, BN_NOCARRY);
                if (BnnIsZero(BzToBn(q), ql) == BN_TRUE) {
                        BzSetSign(q, BZ_ZERO);
                }
        } else  if (BzGetSign(q) == BZ_ZERO
                    && BzGetSign(y) == BzGetSign(z)) {
                /*
                 *      Q == 0, sign(Y) == sign(Z) : 1 => Q
                 */
                BzFree(q);
                q = BzFromInteger((BzInt)1);
        }

        BzFree(r);

        return (q);
}

BigZ
BzRound(const BigZ y, const BigZ z) {
        /*
         * Returns round(Y, Z).
         */

        BigZ            q;
        BigZ            r = BZNULL;
        BigNumLength    ql;

        if ((q = BzDivide(y, z, &r)) == BZNULL) {
                return (BZNULL);
        }

        if (r == BZNULL) {
                /*
                 * This should never happend.
                 */
                BzFree(q);
                return (BZNULL);
        }

        ql = BzNumDigits(q);

        if (BzGetSign(q) == BZ_PLUS && BzGetSign(r) != BZ_ZERO) {
                BigNumDigit one = BN_ONE;
                BigZ        roundz;
                BzSign      sign = BzGetSign(z);

                BzSetSign(r, BZ_PLUS);
                BzSetSign(z, BZ_PLUS);

                roundz = BzAsh(r, 1);

                switch (BzCompare(roundz, z)) {
                case BZ_LT :
                        break;
                case BZ_EQ :
                        /*
                         *      Q > 0, R <> 0, 2*R= Z :
                         *
                         * roundz is exactly halfway between two integers,
                         * choose even number.
                         */
                        if (BzIsOdd(q) == BN_TRUE) {
                                (void)BnnAdd(BzToBn(q),
                                              ql,
                                              &one,
                                              (BigNumLength)1,
                                              BN_NOCARRY);
                        }
                        break;
                case BZ_GT :
                        /*
                         *      Q > 0, R <> 0, 2*R>= Z : Q+1 => Q
                         */
                        (void)BnnAdd(BzToBn(q),
                                      ql,
                                      &one,
                                      (BigNumLength)1,
                                      BN_NOCARRY);
                        break;
                }

                BzSetSign(z, sign);
                BzFree(roundz);
        } else  if (BzGetSign(q) == BZ_MINUS && BzGetSign(r) != BZ_ZERO) {
                /*
                 *      Q < 0, R <> 0, 2*R>= Z : Q-1 => Q
                 */
                BigZ    roundz;
                BzSign  sign = BzGetSign(z);

                BzSetSign(r, BZ_PLUS);
                BzSetSign(z, BZ_PLUS);

                roundz = BzAsh(r, 1);

                switch (BzCompare(roundz, z)) {
                case BZ_LT :
                        break;
                case BZ_EQ :
                        /*
                         * roundz is exactly halfway between two integers,
                         * choose even number.
                         */
                        if (BzIsOdd(q) == BN_TRUE) {
                                (void)BnnSubtractBorrow(BzToBn(q),
                                                         ql,
                                                         BN_NOCARRY);
                        }
                        break;
                case BZ_GT :
                        (void)BnnSubtractBorrow(BzToBn(q), ql, BN_NOCARRY);
                        break;
                }

                BzSetSign(z, sign);
                BzFree(roundz);

                if (BnnIsZero(BzToBn(q), ql) == BN_TRUE) {
                        BzSetSign(q, BZ_ZERO);
                }
        } else  if ((BzGetSign(q) == BZ_ZERO)
                    && (BzGetSign(y) == BzGetSign(z))) {
                /*
                 *      Q == 0, sign(Y) == sign(Z):
                 */
                BigZ    roundz;
                BzSign  sign = BzGetSign(z);

                BzFree(q);

                BzSetSign(r, BZ_PLUS);
                BzSetSign(z, BZ_PLUS);

                roundz = BzAsh(r, 1);

                if (BzCompare(roundz, z) == BZ_LT) {
                        /*
                         *      2*R< Z : 0 => Q
                         */
                        q = BzFromInteger(0);
                } else  {
                        /*
                         *      2*R>= Z : 1 => Q
                         */
                        q = BzFromInteger((BzInt)1);
                }

                BzSetSign(z, sign);
                BzFree(roundz);
        }

        BzFree(r);

        return (q);
}

BigZ
BzMod(const BigZ y, const BigZ z) {
        /*
         * Returns Y mod Z.
         */

        BigZ    r = BZNULL;

        BzFree(BzDivide(y, z, &r));

        return (r);
}

BigZ
BzRem(const BigZ y, const BigZ z) {
        /*
         * Returns Y rem Z.
         */

        BigZ q;
        BigZ r = BZNULL;
        BigZ rem;

        if ((q = BzDivide(y, z, &r)) != BZNULL) {
                BzFree(q);

                if (r == BZNULL) {
                        /*
                         * This should never happend.
                         */
                        return (BZNULL);
                }

                if (BzGetSign(r) == BZ_ZERO) {
                        return (r);
                } else  if (BzGetSign(y) == BzGetSign(z)) {
                        return (r);
                } else  if (BzGetSign(y) == BZ_MINUS) {
                        if ((rem = BzSubtract(z, r)) != BZNULL) {
                                BzSetSign(rem, BZ_MINUS);
                        }
                        BzFree(r);
                        return (rem);
                } else  {
                        if ((rem = BzSubtract(r, z)) != BZNULL) {
                                BzSetSign(rem, BZ_PLUS);
                        }
                        BzFree(r);
                        return (rem);
                }
        } else  {
                return (BZNULL);
        }
}

BigNumBool
BzIsEven(const BigZ y) {
        /*
         * Returns BN_TRUE iff y is even
         */

        return (BnnIsDigitEven(BzGetDigit(y, 0)));
}

BigNumBool
BzIsOdd(const BigZ y) {
        /*
         * Returns BN_TRUE iff y is odd
         */

        return (BnnIsDigitOdd(BzGetDigit(y, 0)));
}

#if     defined(BZ_OPTIMIZE_PRINT)
typedef struct {
        int MaxDigits;
        BigNumDigit  MaxValue;
} BzPrintTable;

#if (BZ_BUCKET_SIZE == 32)
static const BzPrintTable BzPrintBase[] = {
  {  0, (BigNumDigit)0U                     }, /*  0 */
  {  0, (BigNumDigit)0U                     }, /*  1 */
  { 31, (BigNumDigit)2147483648U            }, /*  2 */
  { 20, (BigNumDigit)3486784401U            }, /*  3 */
  { 15, (BigNumDigit)1073741824U            }, /*  4 */
  { 13, (BigNumDigit)1220703125U            }, /*  5 */
  { 12, (BigNumDigit)2176782336U            }, /*  6 */
  { 11, (BigNumDigit)1977326743U            }, /*  7 */
  { 10, (BigNumDigit)1073741824U            }, /*  8 */
  { 10, (BigNumDigit)3486784401U            }, /*  9 */
  {  9, (BigNumDigit)1000000000U            }, /* 10 */
  {  9, (BigNumDigit)2357947691U            }, /* 11 */
  {  8, (BigNumDigit)429981696U             }, /* 12 */
  {  8, (BigNumDigit)815730721U             }, /* 13 */
  {  8, (BigNumDigit)1475789056U            }, /* 14 */
  {  8, (BigNumDigit)2562890625U            }, /* 15 */
  {  7, (BigNumDigit)268435456U             }, /* 16 */
  {  7, (BigNumDigit)410338673U             }, /* 17 */
  {  7, (BigNumDigit)612220032U             }, /* 18 */
  {  7, (BigNumDigit)893871739U             }, /* 19 */
  {  7, (BigNumDigit)1280000000U            }, /* 20 */
  {  7, (BigNumDigit)1801088541U            }, /* 21 */
  {  7, (BigNumDigit)2494357888U            }, /* 22 */
  {  7, (BigNumDigit)3404825447U            }, /* 23 */
  {  6, (BigNumDigit)191102976U             }, /* 24 */
  {  6, (BigNumDigit)244140625U             }, /* 25 */
  {  6, (BigNumDigit)308915776U             }, /* 26 */
  {  6, (BigNumDigit)387420489U             }, /* 27 */
  {  6, (BigNumDigit)481890304U             }, /* 28 */
  {  6, (BigNumDigit)594823321U             }, /* 29 */
  {  6, (BigNumDigit)729000000U             }, /* 30 */
  {  6, (BigNumDigit)887503681U             }, /* 31 */
  {  6, (BigNumDigit)1073741824U            }, /* 32 */
  {  6, (BigNumDigit)1291467969U            }, /* 33 */
  {  6, (BigNumDigit)1544804416U            }, /* 34 */
  {  6, (BigNumDigit)1838265625U            }, /* 35 */
  {  6, (BigNumDigit)2176782336U            }  /* 36 */
};
#endif /* BZ_BUCKET_SIZE == 32 */

#if (BZ_BUCKET_SIZE == 64)
static const BzPrintTable BzPrintBase[] = {
  {  0, (BigNumDigit)0UL                    }, /*  0 */
  {  0, (BigNumDigit)0UL                    }, /*  1 */
  { 63, (BigNumDigit)9223372036854775808UL  }, /*  2 */
  { 40, (BigNumDigit)12157665459056928801UL }, /*  3 */
  { 31, (BigNumDigit)4611686018427387904UL  }, /*  4 */
  { 27, (BigNumDigit)7450580596923828125UL  }, /*  5 */
  { 24, (BigNumDigit)4738381338321616896UL  }, /*  6 */
  { 22, (BigNumDigit)3909821048582988049UL  }, /*  7 */
  { 21, (BigNumDigit)9223372036854775808UL  }, /*  8 */
  { 20, (BigNumDigit)12157665459056928801UL }, /*  9 */
  { 19, (BigNumDigit)10000000000000000000UL }, /* 10 */
  { 18, (BigNumDigit)5559917313492231481UL  }, /* 11 */
  { 17, (BigNumDigit)2218611106740436992UL  }, /* 12 */
  { 17, (BigNumDigit)8650415919381337933UL  }, /* 13 */
  { 16, (BigNumDigit)2177953337809371136UL  }, /* 14 */
  { 16, (BigNumDigit)6568408355712890625UL  }, /* 15 */
  { 15, (BigNumDigit)1152921504606846976UL  }, /* 16 */
  { 15, (BigNumDigit)2862423051509815793UL  }, /* 17 */
  { 15, (BigNumDigit)6746640616477458432UL  }, /* 18 */
  { 15, (BigNumDigit)15181127029874798299UL }, /* 19 */
  { 14, (BigNumDigit)1638400000000000000UL  }, /* 20 */
  { 14, (BigNumDigit)3243919932521508681UL  }, /* 21 */
  { 14, (BigNumDigit)6221821273427820544UL  }, /* 22 */
  { 14, (BigNumDigit)11592836324538749809UL }, /* 23 */
  { 13, (BigNumDigit)876488338465357824UL   }, /* 24 */
  { 13, (BigNumDigit)1490116119384765625UL  }, /* 25 */
  { 13, (BigNumDigit)2481152873203736576UL  }, /* 26 */
  { 13, (BigNumDigit)4052555153018976267UL  }, /* 27 */
  { 13, (BigNumDigit)6502111422497947648UL  }, /* 28 */
  { 13, (BigNumDigit)10260628712958602189UL }, /* 29 */
  { 13, (BigNumDigit)15943230000000000000UL }, /* 30 */
  { 12, (BigNumDigit)787662783788549761UL   }, /* 31 */
  { 12, (BigNumDigit)1152921504606846976UL  }, /* 32 */
  { 12, (BigNumDigit)1667889514952984961UL  }, /* 33 */
  { 12, (BigNumDigit)2386420683693101056UL  }, /* 34 */
  { 12, (BigNumDigit)3379220508056640625UL  }, /* 35 */
  { 12, (BigNumDigit)4738381338321616896UL  }  /* 36 */
};
#endif /* BZ_BUCKET_SIZE == 64 */
#endif /* BZ_OPTIMIZE_PRINT */

BzChar *
BzToString(const BigZ z, BigNumDigit base, int sign) {
        /*
         * wrapper to BzToStringBuffer that always allocate buffer.
         */
        return (BzToStringBuffer(z, base, sign, (BzChar *)0, (size_t *)0));
}

BzChar *
BzToStringBuffer(const BigZ z,
                 BigNumDigit base,
                 int sign,
                 BzChar * const buf,
                 size_t *len) {
        return (BzToStringBufferExt(z,
                                    base,
                                    sign,
                                    buf,
                                    len,
                                    (size_t *)0));
}

BzChar *
BzToStringBufferExt(const BigZ z,
                    BigNumDigit base,
                    int sign,
                    BzChar * const buf,
                    size_t *len,
                    size_t *slen) {
        /*
         * Returns  a  pointer  to  a string that represents Z in the
         * specified   base.   Assumes   BZ_MIN_BASE   <=   base   <=
         * BZ_MAX_BASE.  If  optional  buffer  is supplied,  len is a
         * pointer  of  this buffer size.  If there is enough room to
         * print  the  number  buf is used otherwise function returns
         * NULL and len contains the required size.  If buf is passed
         * as NULL,  this string is allocated on the heap, so it must
         * be desallocated by the user.
         */

        static const BzChar Digit[] = {
                (BzChar)'0', (BzChar)'1', (BzChar)'2', (BzChar)'3',
                (BzChar)'4', (BzChar)'5', (BzChar)'6', (BzChar)'7',
                (BzChar)'8', (BzChar)'9', (BzChar)'a', (BzChar)'b',
                (BzChar)'c', (BzChar)'d', (BzChar)'e', (BzChar)'f',
                (BzChar)'g', (BzChar)'h', (BzChar)'i', (BzChar)'j',
                (BzChar)'k', (BzChar)'l', (BzChar)'m', (BzChar)'n',
                (BzChar)'o', (BzChar)'p', (BzChar)'q', (BzChar)'r',
                (BzChar)'s', (BzChar)'t', (BzChar)'u', (BzChar)'v',
                (BzChar)'w', (BzChar)'x', (BzChar)'y', (BzChar)'z'
        };

        BigZ            y;
        BigZ            q;
        BigNumLength    zl;
        BigNumLength    sl;
        BzChar *        s;
        BzChar *        strg;

        if (base < (BigNumDigit)BZ_MIN_BASE
            || base > (BigNumDigit)BZ_MAX_BASE) {
                if (len != 0) {
                        *len = 0;
                }
                return ((BzChar *)NULL);
        }

        /*
         * Allocate BigNums and set up string
         */

        zl = BzNumDigits(z) + 1;
        sl = (BigNumLength)((BzLog[2] * BN_DIGIT_SIZE * zl) / BzLog[base] + 3);

        if (buf != (BzChar *)NULL
            && len != (size_t *)NULL
            && (sl > (BigNumLength)*len)) {
                /*
                 * a buffer is passed but there is not enough room,
                 * return NULL and set required size in len.
                 */
                *len = (size_t)sl;
                return ((BzChar *)NULL);
        }

        if (len != (size_t *)NULL) {
                /*
                 * set len to 0 in case BzToStringBuffer returns NULL
                 * because of allocation failure. It should be checked by
                 * caller that may otherwise allocate a bigger buffer.
                 */
                *len = 0;
        }

        if ((y = BzCreate(zl)) == BZNULL) {
                return ((BzChar *)NULL);
        }

        if ((q = BzCreate(zl)) == BZNULL) {
                BzFree(y);
                return ((BzChar *)NULL);
        }

        if (buf != (BzChar *)NULL) {
                strg = buf;
        } else  {
                strg = (BzChar *)BzStringAlloc((size_t)sl);
                if (strg == (BzChar *)NULL) {
                        BzFree(y);
                        BzFree(q);
                        return ((BzChar *)NULL);
                }
                if (len != (size_t *)NULL) {
                        /*
                         * a buffer is allocated and caller wants to know
                         * allocated size.
                         */
                        *len = (size_t)sl;
                }
        }

        BnnAssign(BzToBn(y), BzToBn(z), zl - 1);
        s = strg + sl;

        /*
         * Divide Z by base repeatedly; successive digits given by remainders
         */

        *--s = (BzChar)'\0';

        if (BzGetSign(z) == BZ_ZERO) {
                *--s = (BzChar)'0';
#if     defined(BZ_OPTIMIZE_PRINT)
        } else  {
                /*
                 * Compute maxval and digits that can be used with
                 * this base.
                 */

                BigNumDigit  maxval = (BigNumDigit)BzPrintBase[base].MaxValue;
                BigNumLength digits = (BigNumLength)BzPrintBase[base].MaxDigits;

                /*
                 * This optimization makes BigZ output 10 to 20x faster.
                 */
                do {
                        BigZ            v;
                        BigNumDigit     r;
                        /*
                         * compute: y div maxval => q,
                         * returns r = y mod maxval
                         *
                         * maxval is the greatest integer in base 'base'
                         * that fits in a BigNumDigit.
                         */

                        r = BnnDivideDigit(BzToBn(q),
                                           BzToBn(y),
                                           zl,
                                           maxval);

                        if (BnnIsZero(BzToBn(q), zl) == BN_FALSE) {
                                /*
                                 * More digits to come on left, add exactly
                                 * the number of digits with possible
                                 * leading 0 (when r becomes 0).
                                 */
                                int     i;
                                for (i = 0; i < (int)digits; ++i) {
                                        if (r == 0) {
                                                /*
                                                 * No need to divide, fill
                                                 * the rest with '0'.
                                                 */
                                                *--s = (BzChar)'0';
                                        } else {
                                                *--s = Digit[r % base];
                                                r = r / base;
                                        }
                                }
                        } else  {
                                /*
                                 * Last serie (top left). Print only available
                                 * digits (stop when r becomes 0).
                                 */
                                while (r != 0) {
                                        *--s = Digit[r % base];
                                        r = r / base;
                                }
                        }

                        /*
                         * exchange y and q (to avoid BzMove(y, q))
                         */

                        v = q;
                        q = y;
                        y = v;
                } while (BnnIsZero(BzToBn(y), zl) == BN_FALSE);
        }
#else   /* BZ_OPTIMIZE_PRINT */
        } else  do {
                BigZ            v;
                BigNumDigit     r;
                /* compute: y div base => q, returns r = y mod base */

                r = BnnDivideDigit(BzToBn(q), BzToBn(y), zl, base);
                *--s = Digit[r];

                /*
                 * exchange y and q (to avoid BzMove(y, q))
                 */

                v = q;
                q = y;
                y = v;
        } while (BnnIsZero(BzToBn(y), zl) == BN_FALSE);
#endif  /* BZ_OPTIMIZE_PRINT */

        /*
         * Add sign if needed.
         */

        switch (BzGetSign(z)) {
        case BZ_MINUS:
                /*
                 * z < 0, always add '-' sign.
                 */
                *--s = (BzChar)'-';
                break;
        case BZ_ZERO:
                /*
                 * z = 0 -> no sign even if BZ_FORCE_SIGN is passed.
                 */
                break;
        case BZ_PLUS:
                /*
                 * z > 0, add '+' only if sign is required.
                 */
                if (sign == BZ_FORCE_SIGN) {
                        *--s = (BzChar)'+';
                }
        }

#if 0
        /*
         * and move string into position
         */

        if ((s - strg) > 0) {
                BigNumLength i;

                for (i = 0; s[i] != (BzChar)'\000'; ++i) {
                        strg[i] = s[i];
                }

                strg[i] = (BzChar)'\000';
        }
#else
        if (buf != (BzChar *)NULL) {
                strg = &s[0];
        } else if ((s - strg) > 0) {
               /*
                * and move string into position as no buffer
                * has been supplied.
                */
                BigNumLength i;

                for (i = 0; s[i] != (BzChar)'\000'; ++i) {
                        strg[i] = s[i];
                }

                strg[i] = (BzChar)'\000';
        }
#endif

        /*
         * Free temporary BigNums and return the string
         */

        BzFree(y);
        BzFree(q);

        if (slen != (size_t *)NULL) {
                /*
                 * A non null pointer was passed to get string length
                 * (which is not the same a buffer length used to build string).
                 */
                *slen = (size_t)(strg + sl - s - 1);
        }

        return (strg);
}

size_t
BzStrLen(const BzChar *s) {
        size_t  len;

        for (len = (size_t)0; *s++ != (BzChar)'\000'; ++len) {
                continue;
        }

        return (len);
}

BigZ
BzFromStringLen(const BzChar *s, size_t len, BigNumDigit base, BzStrFlag flag) {
        /*
         * Creates  a  BigZ whose value is represented by "string" in
         * the  specified  base.  The  "string"  may  contain leading
         * spaces,  followed  by  an  optional  sign,  followed  by a
         * series of digits. Assumes BZ_MIN_BASE<=base <=BZ_MAX_BASE.
         * When called from C, only the first 2 arguments are passed.
         */

        BigZ            z;
        BigZ            p;
        BzSign          sign;
        BigNumLength    zl;
        size_t          i;

        /*
         * Throw away any initial space
         */

        while ((*s == (BzChar)' ')
               || (*s == (BzChar)'\t')
               || (*s == (BzChar)'\n')
               || (*s == (BzChar)'\r')) {
                ++s;
                --len;
        }

        /*
         * Set up sign, base, initialize result
         */

        switch (*s) {
        case ((BzChar)'-'):
                sign = BZ_MINUS;
                ++s;
                --len;
                break;
        case ((BzChar)'+'):
                sign = BZ_PLUS;
                ++s;
                --len;
                break;
        default:
                sign = BZ_PLUS;
        }

        /*
         * Allocate BigNums
         */

        zl = (BigNumLength)(len*BzLog[base] / (BzLog[2]*BN_DIGIT_SIZE)+1);

        if ((z = BzCreate(zl)) == BZNULL) {
                return (BZNULL);
        }

        if ((p = BzCreate(zl)) == BZNULL) {
                BzFree(z);
                return (BZNULL);
        }

        /*
         * Multiply in the digits of the string, one at a time
         */

        for (i = 0; i < len; ++i) {
                BigZ        v;
                int         val  = CTOI(s[i]);
                BigNumDigit next = (BigNumDigit)val;

                if (val == -1 || next >= base) {
                        if (flag == BZ_UNTIL_INVALID) {
                                break;
                        }
                        /*
                         * Invalid syntax for base.
                         */
                        BzFree(p);
                        BzFree(z);
                        return (BZNULL);
                }

                BnnSetToZero(BzToBn(p), zl);
                BnnSetDigit(BzToBn(p), next);
                (void)BnnMultiplyDigit(BzToBn(p), zl, BzToBn(z), zl, base);

                /*
                 * exchange z and p (to avoid BzMove (z, p)
                 */

                v = p;
                p = z;
                z = v;
        }

        /*
         * Set sign of result
         */

        BzSetSign(z, (BnnIsZero(BzToBn(z), zl) == BN_TRUE) ? BZ_ZERO : sign);

        /*
         * Free temporary BigNums
         */

        BzFree(p);

        return (z);
}

BigZ
BzFromString(const BzChar *s, BigNumDigit base, BzStrFlag flag) {
        return (BzFromStringLen(s, BzStrLen(s), base, flag));
}

BigZ
BzFromInteger(BzInt i) {
        BigZ    z;

        z = BzCreate((BigNumLength)1);

        if (z != BZNULL) {
                BzSetDigit(z, 0, (BigNumDigit)AbsInt(i));

                if (i > 0) {
                        BzSetSign(z, BZ_PLUS);
                } else  if (i < 0) {
                        BzSetSign(z, BZ_MINUS);
                } else  {
                        BzSetSign(z, BZ_ZERO);
                }
        }

        return (z);
}

BigZ
BzFromUnsignedInteger(BzUInt i) {
        BigZ    z;

        z = BzCreate((BigNumLength)1);

        if (z != BZNULL) {
                BzSetDigit(z, 0, (BigNumDigit)i);
                BzSetSign(z, ((i > (BzInt)0) ? BZ_PLUS : BZ_ZERO));
        }

        return (z);
}

BzInt
BzToInteger(const BigZ z) {
        if (BzNumDigits(z) > (BigNumLength)1) {
                return (BZMAXINT);
        } else  if (BzGetSign(z) == BZ_MINUS) {
                return (- (BzInt)BzGetDigit(z, 0));
        } else  {
                return ((BzInt)BzGetDigit(z, 0));
        }
}

int
BzToIntegerPointer(const BigZ z, BzInt *p) {
        BzInt value;

        if (BzNumDigits(z) > (BigNumLength)1) {
                *p = BZMAXINT;
                return (0);
        }

        value = (BzInt)BzGetDigit(z, 0);

        if (value < 0) {
                *p = BZMAXINT;
                return (0);
        } else  if (BzGetSign(z) == BZ_MINUS) {
                *p = -value;
        } else  {
                *p =  value;
        }

        return (1);
}

BzUInt
BzToUnsignedInteger(const BigZ z) {
        if (BzNumDigits(z) > (BigNumLength)1) {
                return (BZMAXUINT);
        } else  {
                return ((BzUInt)BzGetDigit(z, 0));
        }
}

int
BzToUnsignedIntegerPointer(const BigZ z, BzUInt *p) {
        if (BzNumDigits(z) > (BigNumLength)1) {
                *p = BZMAXUINT;
                return (0);
        } else  {
                *p = (BzUInt)BzGetDigit(z, 0);

                return (1);
        }
}

BigZ
BzFromBigNum(const BigNum n, BigNumLength nl) {
        BigZ            z;
        BigNumLength    i;

        z = BzCreate(nl);

        if (z != BZNULL) {
                /*
                 * set the sign of z such that the pointer n is unchanged yet
                 */

                if (BnnIsZero(n, nl) == BN_TRUE) {
                        BzSetSign(z, BZ_ZERO);
                } else  {
                        BzSetSign(z, BZ_PLUS);
                }

                for (i = 0; i < nl; ++i) {
                        BzSetDigit(z, i, n[i]);
                }
        }

        return (z);
}

BigNum
BzToBigNum(const BigZ z, BigNumLength *nl) {
        BigNum          n;
        BigNum          m;
        BigNumLength    i;
        size_t          size;

        if (BzGetSign(z) == BZ_MINUS) {
                return ((BigNum)NULL);
        }

        *nl  = BzNumDigits(z);
        size = ((size_t)(*nl + 1)) * sizeof(BigNumDigit);

        if ((n = (BigNum)(BzAlloc(size))) != NULL) {
                *n = (BigNumDigit)*nl; /* set size */

                for (i = 0, m = ++n; i < *nl; i++, m++) {
                        *m = BzGetDigit(z, i);
                }
        }

        return (n);
}

/*
 * The  logical  operations  provide a convenient way to represent an
 * infinite  vector of bits.  Let such a conceptual vector be indexed
 * by the non-negative integers.  Then bit j is assigned a ``weight''
 * 2**j.  Assume  that only a finite number of bits are 1's or only a
 * finite number of bits are 0's.  A vector with only a finite number
 * of  one-bits  is  represented  as  the  sum  of the weights of the
 * one-bits,  a positive integer.  A vector with only a finite number
 * of  zero-bits is represented as -1 minus the sum of the weights of
 * the zero-bits, a negative integer.
 *
 * This  method  of  using  integers  to represent bit-vectors can in
 * turn  be  used  to  represent  sets.  Suppose  that some (possibly
 * countably  infinite) universe of discourse for sets is mapped into
 * the non-negative integers.  Then a set can be represented as a bit
 * vector;  an  element  is  in  the  set  if  the  bit  whose  index
 * corresponds  to that element is a one-bit.  In this way all finite
 * sets  can  be  represented (by positive integers),  as well as all
 * sets whose complements are finite (by negative integers).
 */

#define BZ_SIGN1        0x1     /* 01 */
#define BZ_SIGN2        0x2     /* 10 */

BigZ
BzNot(const BigZ z) {
        /*
         * Negates the passed BigZ.
         */

        BigZ    y;

        switch (BzGetSign(z)) {
        case BZ_MINUS:
             if ((y = BzCopy(z)) != BZNULL) {
               BnnComplement2(BzToBn(y), BzNumDigits(y));
               BnnComplement(BzToBn(y), BzNumDigits(y));
               if (BnnIsZero(BzToBn(y), BzNumDigits(y)) == BN_TRUE) {
                 /*
                  * ~(-1) -> 0
                  */
                 BzSetSign(y, BZ_ZERO);
               } else {
                 BzSetSign(y, BZ_PLUS);
               }
             }
             break;
        case BZ_ZERO:
             y = BzFromInteger((BzInt)-1);
             break;
        default: /* case BZ_PLUS: */
             if ((y = BzCopy(z)) != BZNULL) {
                     BnnComplement(BzToBn(y), BzNumDigits(y));
                     BnnComplement2(BzToBn(y), BzNumDigits(y));
                     BzSetSign(y, BZ_MINUS);
             }
             break;
        }

        return (y);
}

BigZ
BzAnd(const BigZ y, const BigZ z) {
        /*
         * Returns Y & Z.
         */

        BigZ            n;
        BigZ            yy;
        BigZ            zz;
        BigNumLength    yl;
        BigNumLength    zl;
        BigNumLength    l;
        unsigned int    sign = 0;

        yl = BzNumDigits(y);
        zl = BzNumDigits(z);

        if (BzGetSign(y) == BZ_MINUS) {
                if ((yy = BzCopy(y)) == BZNULL) {
                        return (BZNULL);
                }
                BnnComplement2(BzToBn(yy), yl);
                sign |= BZ_SIGN1;
        } else  {
                yy = y;
        }

        if (BzGetSign(z) == BZ_MINUS) {
                if ((zz = BzCopy(z)) == BZNULL) {
                        if ((sign & BZ_SIGN1) != 0) {
                                BzFree(yy);
                        }
                        return (BZNULL);
                }
                BnnComplement2(BzToBn(zz), zl);
                sign |= BZ_SIGN2;
        } else  {
                zz = z;
        }

        if (yl < zl) {
                if ((n = BzCopy(zz)) != BZNULL) {
                        BzSetSign(n, BZ_PLUS);
                        if ((sign & BZ_SIGN1) == 0) {
                                for (l = (zl - 1); l >= yl; --l) {
                                  BnnAndDigits(BzToBn(n) + l, BN_ZERO);
                                }
                        }
                        for (l = 0; l < yl; ++l) {
                          BnnAndDigits(BzToBn(n) + l, *(BzToBn(yy) + l));
                        }
                }
        } else  {
                if ((n = BzCopy(yy)) != BZNULL) {
                        BzSetSign(n, BZ_PLUS);
                        if ((sign & BZ_SIGN2) == 0) {
                                for (l = (yl - 1); l >= zl; --l) {
                                  BnnAndDigits(BzToBn(n) + l, BN_ZERO);
                                }
                        }
                        for (l = 0; l < zl; ++l) {
                          BnnAndDigits(BzToBn(n) + l, *(BzToBn(zz) + l));
                        }
                }
        }

        if (n != BZNULL) {
                if (BnnIsZero(BzToBn(n), BzNumDigits(n)) == BN_TRUE) {
                        BzSetSign(n, BZ_ZERO);
                } else  if (sign == (unsigned int)(BZ_SIGN1 | BZ_SIGN2)) {
                        BnnComplement2(BzToBn(n), BzNumDigits(n));
                        BzSetSign(n, BZ_MINUS);
                }
        }

        /*
         *  Free copies.
         */

        if ((sign & BZ_SIGN1) != 0) {
                BzFree(yy);
        }

        if ((sign & BZ_SIGN2) != 0) {
                BzFree(zz);
        }

        return (n);
}

BigZ
BzOr(const BigZ y, const BigZ z) {
        /*
         * Returns Y | Z.
         */

        BigZ            n;
        BigZ            yy;
        BigZ            zz;
        BigNumLength    yl;
        BigNumLength    zl;
        BigNumLength    l;
        unsigned int    sign = 0;

        yl = BzNumDigits(y);
        zl = BzNumDigits(z);

        if (BzGetSign(y) == BZ_MINUS) {
                if ((yy = BzCopy(y)) == BZNULL) {
                        return (BZNULL);
                }
                BnnComplement2(BzToBn(yy), yl);
                sign |= BZ_SIGN1;
        } else  {
                yy = y;
        }

        if (BzGetSign(z) == BZ_MINUS) {
                if ((zz = BzCopy(z)) == BZNULL) {
                        if ((sign & BZ_SIGN1) != 0) {
                                BzFree(yy);
                        }
                        return (BZNULL);
                }
                BnnComplement2(BzToBn(zz), zl);
                sign |= BZ_SIGN2;
        } else  {
                zz = z;
        }

        if (yl < zl) {
                if ((n = BzCopy(zz)) != BZNULL) {
                        BzSetSign(n, BZ_PLUS);
                        if ((sign & BZ_SIGN1) != 0) {
                                for (l = (zl - 1); l >= yl; --l) {
                                  BnnAndDigits(BzToBn(n) + l, BN_ZERO);
                                }
                        }
                        for (l = 0; l < yl; ++l) {
                          BnnOrDigits(BzToBn(n) + l, *(BzToBn(yy) + l));
                        }
                }
        } else  {
                if ((n = BzCopy(yy)) != BZNULL) {
                        BzSetSign(n, BZ_PLUS);
                        if ((sign & BZ_SIGN2) != 0) {
                                for (l = (yl - 1); l >= zl; --l) {
                                  BnnAndDigits(BzToBn(n) + l, BN_ZERO);
                                }
                        }
                        for (l = 0; l < zl; ++l) {
                          BnnOrDigits(BzToBn(n) + l, *(BzToBn(zz) + l));
                        }
                }
        }

        if (n != BZNULL) {
                if (BnnIsZero(BzToBn(n), BzNumDigits(n)) == BN_TRUE) {
                        BzSetSign(n, BZ_ZERO);
                } else  if (sign != 0) {
                        BnnComplement2(BzToBn(n), BzNumDigits(n));
                        BzSetSign(n, BZ_MINUS);
                }
        }

        /*
         *  Free copies.
         */

        if ((sign & BZ_SIGN1) != 0) {
                BzFree(yy);
        }

        if ((sign & BZ_SIGN2) != 0) {
                BzFree(zz);
        }

        return (n);
}

BigZ
BzXor(const BigZ y, const BigZ z) {
        /*
         * Returns Y ^ Z.
         */

        BigZ            n;
        BigZ            yy;
        BigZ            zz;
        BigNumLength    yl;
        BigNumLength    zl;
        BigNumLength    l;
        int             sign = 0;

        yl = BzNumDigits(y);
        zl = BzNumDigits(z);

        if (BzGetSign(y) == BZ_MINUS) {
                if ((yy = BzCopy(y)) == BZNULL) {
                        return (BZNULL);
                }
                BnnComplement2(BzToBn(yy), yl);
                sign |= BZ_SIGN1;
        } else  {
                yy = y;
        }

        if (BzGetSign(z) == BZ_MINUS) {
                if ((zz = BzCopy(z)) == BZNULL) {
                        if ((sign & BZ_SIGN1) != 0) {
                                BzFree(yy);
                        }
                        return (BZNULL);
                }
                BnnComplement2(BzToBn(zz), zl);
                sign |= BZ_SIGN2;
        } else  {
                zz = z;
        }

        if (yl < zl) {
                if ((n = BzCopy(zz)) != BZNULL) {
                        BzSetSign(n, BZ_PLUS);
                        if ((sign & BZ_SIGN1) != 0) {
                                for (l = (zl - 1); l >= yl; --l) {
                                  BnnXorDigits(BzToBn(n) + l, BN_COMPLEMENT);
                                }
                        }
                        for (l = 0; l < yl; ++l) {
                          BnnXorDigits(BzToBn(n) + l, *(BzToBn(yy) + l));
                        }
                }
        } else  {
                if ((n = BzCopy(yy)) != BZNULL) {
                        BzSetSign(n, BZ_PLUS);
                        if ((sign & BZ_SIGN2) != 0) {
                                for (l = (yl - 1); l >= zl; --l) {
                                  BnnXorDigits(BzToBn(n) + l, BN_COMPLEMENT);
                                }
                        }
                        for (l = 0; l < zl; ++l) {
                          BnnXorDigits(BzToBn(n) + l, *(BzToBn(zz) + l));
                        }
                }
        }

        if (n != BZNULL) {
                if (BnnIsZero(BzToBn(n), BzNumDigits(n)) == BN_TRUE) {
                        BzSetSign(n, BZ_ZERO);
                } else  if (sign == BZ_SIGN1 || sign == BZ_SIGN2) {
                        BnnComplement2(BzToBn(n), BzNumDigits(n));
                        BzSetSign(n, BZ_MINUS);
                }
        }

        /*
         *  Free copies.
         */

        if ((sign & BZ_SIGN1) != 0) {
                BzFree(yy);
        }

        if ((sign & BZ_SIGN2) != 0) {
                BzFree(zz);
        }

        return (n);
}

BigNumBool
BzTestBit(BigNumLength bit, const BigZ z) {
        BigNumLength    zl;
        BigNumBool      r;

        /*
         * Returns  BN_TRUE  iff bit is on (i.e.  2**bit is one).  It
         * assumes that bit is a non-negative integer.
         */

        zl = (BigNumLength)(bit / BN_DIGIT_SIZE);

        if (zl >= BzNumDigits(z)) {
                return ((BzGetSign(z) == BZ_MINUS) ? BN_TRUE : BN_FALSE);
        }

        bit = (bit % BN_DIGIT_SIZE);

        if (BzGetSign(z) == BZ_MINUS) {
                BigZ y;
                BigNumLength yl;

                if ((y = BzCopy(z)) == BZNULL) {
                        return (BN_FALSE);
                }

                yl = BzNumDigits(y);

                BnnComplement2(BzToBn(y), yl);
                BzSetSign(y, BZ_PLUS);
                r = (BigNumBool)(((*(BzToBn(y) + zl)) & (BN_ONE << bit)) != 0);
                BzFree(y);
        } else  {
                r = (BigNumBool)(((*(BzToBn(z) + zl)) & (BN_ONE << bit)) != 0);
        }

        return (r);
}

BigNumLength
BzBitCount(const BigZ z) {
        /*
         * Returns the number of bits set in z.
         */

        BigNumLength nl = (BigNumLength)0;
        BigZ    y;

        switch (BzGetSign(z)) {
        case BZ_MINUS:
                if ((y = BzCopy(z)) == BZNULL) {
                        return ((BigNumLength)0);
                }
                BnnComplement2(BzToBn(y), BzNumDigits(y));
                BnnComplement(BzToBn(y), BzNumDigits(y));
                nl = BnnNumCount(BzToBn(y), BzNumDigits(y));
                BzFree(y);
                break;
        case BZ_ZERO:
                nl = (BigNumLength)0;
                break;
        case BZ_PLUS:
                nl = BnnNumCount(BzToBn(z), BzNumDigits(z));
                break;
        }

        return (nl);
}

/*
 *      Simple logical equivalence rules.
 */

BigZ
BzNand(const BigZ x, const BigZ y) {
        BigZ tmp = BzAnd(x, y);
        BigZ res = BzNot(tmp);
        BzFree(tmp);
        return (res);
}

BigZ
BzNor(const BigZ x, const BigZ y) {
        BigZ tmp = BzOr(x, y);
        BigZ res = BzNot(tmp);
        BzFree(tmp);
        return (res);
}

BigZ
BzEqv(const BigZ x, const BigZ y) {
        BigZ tmp = BzXor(x, y);
        BigZ res = BzNot(tmp);
        BzFree(tmp);
        return (res);
}

BigZ
BzAndC1(const BigZ x, const BigZ y) {
        BigZ tmp = BzNot(x);
        BigZ res = BzAnd(tmp, y);
        BzFree(tmp);
        return (res);
}

BigZ
BzAndC2(const BigZ x, const BigZ y) {
        BigZ tmp = BzNot(y);
        BigZ res = BzAnd(x, tmp);
        BzFree(tmp);
        return (res);
}

BigZ
BzOrC1(const BigZ x, const BigZ y) {
        BigZ tmp = BzNot(x);
        BigZ res = BzOr(tmp, y);
        BzFree(tmp);
        return (res);
}

BigZ
BzOrC2(const BigZ x, const BigZ y) {
        BigZ tmp = BzNot(y);
        BigZ res = BzOr(x, y);
        BzFree(tmp);
        return (res);
}

BigZ
BzAsh(const BigZ y, int n) {
        BigZ    z;

        if (n > 0) {
                /*
                 *      Create a copy + space for the shift.
                 */

                BigNumLength zl;
                BigNumLength len = (BigNumLength)(BN_DIGIT_SIZE - 1);
                int          ll;

                if ((BzNumDigits(y) == (BigNumLength)1)
                    && (BzGetDigit(y, 0) == (BigNumDigit)1)) {
                        /*
                         * Optimize case where y == 1 (power of two).
                         */
                        int bitnb = n + 1;
                        BigNumDigit digit;

                        /*
                         * Compute the number of buckets.
                         */

                        zl = (BigNumLength)(bitnb / BN_DIGIT_SIZE);

                        if ((bitnb % BN_DIGIT_SIZE) != 0) {
                                zl++;
                        }

                        /*
                         * Create a zeroed BigZ of zl buckets.
                         */

                        if ((z = BzCreate(zl)) == BZNULL) {
                                return (z);
                        }

                        /*
                         * Set the highest bit.
                         */

                        digit = ((BigNumDigit)1 << ((BzUInt)n % BN_DIGIT_SIZE));
                        BzSetDigit(z, zl - 1, digit);

                        /*
                         * Sign is the sign of y.
                         */
                        BzSetSign(z, BzGetSign(y));
                        return (z);
                } else  {
                        zl = (BigNumLength)(n / BN_DIGIT_SIZE);

                        if ((n % BN_DIGIT_SIZE) != 0) {
                                zl++;
                        }

                        zl += BzNumDigits(y);

                        if ((z = BzCreate(zl)) == BZNULL) {
                                return (z);
                        }

                        BnnAssign(BzToBn(z), BzToBn(y), BzNumDigits(y));
                        BzSetSign(z, BzGetSign(y));

                        /*
                         *      Now do the shift by BN_DIGIT_SIZE increment.
                         */

                        for (ll = n; ll >= (int)BN_DIGIT_SIZE; ll -= len) {
                                (void)BnnShiftLeft(BzToBn(z), zl, len);
                        }

                        (void)BnnShiftLeft(BzToBn(z), zl, (BigNumLength)ll);
                }
        } else  {
                BigZ    one;
                BigZ    d;

                if (y == BZNULL) {
                        return (BZNULL);
                } else  if (BzGetSign(y) == BZ_ZERO) {
                        return (BzCopy(y));
                }

                if ((one = BzFromInteger((BzInt)1)) == BZNULL) {
                        return (BZNULL);
                }

                if ((d = BzAsh(one, -n)) == BZNULL) {
                        BzFree(one);
                        return (BZNULL);
                }

                z = BzFloor(y, d);
                BzFree(d);
                BzFree(one);
        }

        return (z);
}

BigZ
BzSqrt(const BigZ z) {
        BigNumLength    n;
        BigZ            x;
        BigZ            two;

        if (BzGetSign(z) == BZ_ZERO) {
                return (BzFromInteger((BzInt)0));
        }

        n   = BzLength(z);

        if ((n & 1) != 0) {
                /*
                 * n is odd.
                 */
                n = (n >> 1) + 1;
        } else  {
                /*
                 * n is even.
                 */
                n = (n >> 1);
        }

        {
                BigZ one = BzFromInteger((BzInt)1);
                
                x = BzAsh(one, (int)n);
                BzFree(one);
        }

        two = BzFromInteger((BzInt)2);

        for (;;) {
                BigZ v;
                BigZ y = BzFloor(z, x);

                if (BzCompare(x, y) != BZ_GT) {
                        BzFree(y);
                        break;
                }

                v = BzAdd(x, y);

                BzFree(x);
                x = BzFloor(v, two);
                BzFree(v);
                BzFree(y);
        }

        /*
         * Free temporary two
         */

        BzFree(two);

        return (x);
}

BigZ
BzLcm(const BigZ y, const BigZ z) {
        BigZ    a;
        BigZ    b;
        BigZ    r;

        /*
         * Returns lcm(Y, Z).
         */

        r = BZNULL;

        if ((a = BzMultiply(y, z)) != BZNULL) {
                if (BzGetSign(a) == BZ_MINUS) {
                        BzSetSign(a, BZ_PLUS);
                }

                if ((b = BzGcd(y, z)) != BZNULL) {
                        r = BzTruncate(a, b);
                        BzFree(b);
                }
                BzFree(a);
        }

        return (r);
}

BigZ
BzGcd(const BigZ y, const BigZ z) {
        /*
         * Returns gcd(Y, Z).
         */

        if (BzGetSign(y) == BZ_ZERO) {
                return (BzAbs(z));
        } else  if (BzGetSign(z) == BZ_ZERO) {
                return (BzAbs(y));
        } else  {
                BigZ yc;
                BigZ zc;

                if ((yc = BzAbs(y)) == BZNULL) {
                        /* a fresh copy failed */
                        return (BZNULL);
                }

                if ((zc = BzAbs(z)) == BZNULL) {
                        /* a fresh copy failed */
                        BzFree(yc);
                        return (BZNULL);
                }

                while (BzGetSign(zc) != BZ_ZERO) {
                        BigZ tmp = BzMod(yc, zc);
                        BzFree(yc);

                        if (tmp == BZNULL) {
                                yc = BZNULL;
                                break;
                        } else {
                                yc = zc;
                                zc = tmp;
                        }
                }

                BzFree(zc);

                return (yc);
        }
}

static BigNumDigit BzInternalRandom(BzSeed *seed);

#if     defined(_WIN64) || defined(HAVE_STDINT_H)
typedef uint32_t BzUInt32;
#else
typedef unsigned int BzUInt32;
#endif

static BigNumDigit
BzInternalRandom(BzSeed *seed) {
        BzUInt32 s;


        if (seed == (BzSeed *)0) {
                /*
                 * Should not happen. Returns 0 instead of hanging.
                 */
                return 0;
        }

        /*
         * http://en.wikipedia.org/wiki/Linear_congruential_generator
         * a - 1 is divisible by all prime factors of m.
         * (In our case m = 2^32, size of the int, so m has only one prime
         * factor = 2)
         * a - 1 is a multiple of 4 if m is a multiple of 4.
         * (32768 is multiple of 4, and 1103515244 too)
         */

        s = *seed;

        *seed = (BzSeed)(s * 1103515245 + 12345);

        return (BigNumDigit)(((BzUInt32)*seed / 65536) % 32768);
}

BigZ
BzRandom(const BigZ n, BzSeed *seed) {
        BigZ res;
        BigZ r;
        BigNumLength len;
        BigNumLength i;
        BigNumLength bytes;

        /*
         * Algo: make a copy of n and call BzInternalRandom() to replace all
         * its bits (all its BigNumDigit).
         * Assume any bit has an equiprobable [0-1] value.
         */

        if ((r = BzCopy(n)) == BZNULL) {
                return (BZNULL);
        }

        len = BzLength(n);
        bytes = len / BN_BYTE_SIZE;

        if (len % BN_BYTE_SIZE) {
                ++bytes;
        }

        for (i = 0; bytes > 0; ++i) {
                BigNumLength j;
                BigNumDigit  *d = BzToBn(r) + i;

                for (j = 0;
                     bytes > 0 && j < (BigNumLength)sizeof(BigNumDigit);
                     ++j) {
                        BigNumDigit chunk = (BzInternalRandom(seed) & 0xff);
                        *d += (chunk << (j * BN_BYTE_SIZE));
                        --bytes;
                }
        }

        /*
         * Call BzMod to insure result is less than n.
         */
        res = BzMod(r, n);

        BzFree(r);

        return (res);
}

BigZ
BzPow(const BigZ base, BzUInt exponent) {
        if (exponent == 0) {
                /*
                 * Any nonzero number raised by the exponent 0 is 1
                 */
                return (BzFromInteger(1));
        } else  {
                BigZ x;
                BigZ y;

                if ((x = BzMultiply(base, base)) == BZNULL) {
                        return BZNULL;
                }

                y = BzPow(x, exponent / 2);

                BzFree(x);

                if (y == BZNULL) {
                        return (BZNULL);
                } else  if ((exponent % (BzUInt)2) != 0) {
                        x = BzMultiply(y, base);
                        BzFree(y);
                        return (x);
                } else {
                        return (y);
                }
        }
}

/*
 *  Right-to-left binary method
 *  https://en.wikipedia.org/wiki/Modular_exponentiation
 *  function modular_pow(base, exponent, modulus)
 *    if (modulus == 1)
 *      return 0
 *    result := 1
 *    base := base mod modulus
 *    while exponent > 0
 *        if (exponent mod 2 == 1)
 *           result := (result * base) mod modulus
 *        exponent := exponent >> 1
 *        base := (base * base) mod modulus
 *    return result
 */
BigZ
BzModExp(const BigZ base, const BigZ exponent, const BigZ modulus) {
        BigZ result;
        BigZ mod;
        BigZ expnt;
        BigZ b;
        int  neg;

        if ((result = BzFromInteger(1)) == BZNULL) {
                return (BZNULL);
        }

        switch (BzGetSign(exponent)) {
        case BZ_ZERO:
                /*
                 * exponent == 0, (base ** 0) == 1, two cases to consider:
                 */
                if (BzCompare(modulus, result) == BZ_EQ) {
                        /*
                         * modulus == 1 => 0
                         */
                        BnnSetToZero(BzToBn(result), (BigNumLength)1);
                        BzSetSign(result, BZ_ZERO);
                        return (result);
                } else  if (BzGetSign(modulus) == BZ_MINUS) {
                        /*
                         * modulus < 0 => modulus + 1
                         */
                        BigZ tmp = BzAdd(modulus, result);
                        BzFree(result);
                        return (tmp);
                } else  {
                        /*
                         * modulus > 1 => 1
                         */
                        return (result);
                }
        case BZ_MINUS:
                /*
                 * Negative exponent is not supported.
                 */
                BzFree(result);
                return (BZNULL);
        default:
                if (BzGetSign(modulus) == BZ_PLUS) {
                        /*
                         * modulus is positive, don't need to copy it.
                         */
                        neg = 0;
                        mod = modulus;
                } else  {
                        /*
                         * Make a copy of modulus as positive value.
                         */

                        if ((mod = BzNegate(modulus)) == BZNULL) {
                                BzFree(result);
                                return (BZNULL);
                        }

                        neg = 1;
                }

                /*
                 * Copy base as it will be modified.
                 */

                if ((b = BzCopy(base)) == BZNULL) {
                        BzFreeIf(neg, mod);
                        BzFree(result);
                        return (BZNULL);
                }

                /*
                 * Copy exponent as it will be modified.
                 */

                if ((expnt = BzCopy(exponent)) == BZNULL) {
                        BzFreeIf(neg, mod);
                        BzFree(b);
                        BzFree(result);
                        return (BZNULL);
                }

                while (BzGetSign(expnt) == BZ_PLUS) {
                        BigZ tmp;
                        if (BzIsOdd(expnt)) {
                                tmp = BzMultiply(result, b);
                                BzFree(result);
                                if (tmp == BZNULL) {
                                        BzFreeIf(neg, mod);
                                        BzFree(expnt);
                                        BzFree(b);
                                        return (BZNULL);
                                }
                                result = BzMod(tmp, mod);
                                BzFree(tmp);
                                if (result == BZNULL) {
                                        BzFreeIf(neg, mod);
                                        BzFree(expnt);
                                        BzFree(b);
                                        return (BZNULL);
                                }
                        }
                        /*
                         * expnt = expnt >> 1;
                         */
                        tmp = BzAsh(expnt, -1);
                        BzFree(expnt);
                        expnt = tmp;
                        tmp = BzMultiply(b, b);
                        BzFree(b);
                        if (tmp == BZNULL) {
                                BzFreeIf(neg, mod);
                                BzFree(expnt);
                                BzFree(result);
                                return (BZNULL);
                        }
                        b = BzMod(tmp, mod);
                        BzFree(tmp);
                        if (b == BZNULL) {
                                BzFreeIf(neg, mod);
                                BzFree(expnt);
                                BzFree(result);
                                return (BZNULL);
                        }
                }

                BzFree(expnt);
                BzFree(b);

                if (neg) {
                        /*
                         * Modulus is negative. Adjust value.
                         */
                        BigZ tmp;
                        BzFree(mod);
                        tmp = BzMod(result, modulus);
                        BzFree(result);
                        return (tmp);
                } else  {
                        return (result);
                }
        }
}


/*                                   bigq.c                               */

/*
 * Implementation note:
 * If any BigQ parameter is passed as BZNULL, function returns BZNULL which
 * should be considered as an error.
 */

typedef enum {
        BQ_COPY,
        BQ_SET
} BqCreateMode;

static  void BqNormalize(BigQ q);
static  BigQ BqCreateInternal(const BigZ n, const BigZ d, BqCreateMode mode);

static BigQ
BqCreateInternal(const BigZ n, const BigZ d, BqCreateMode mode) {
        /*
         * Constraints:
         * - n in Z
         * - d in (N \ {0})
         */

        BigQ    q;
        BigZ    cn;
        BigZ    cd;

        if (n == BZNULL || d == BZNULL) {
                return (BQNULL);
        }

#if     defined(BQ_POSITIVE_DENOMINATOR)
        if (BzGetSign(d) != BZ_PLUS) {
                return (BQNULL);
        }
#else
        if (BzGetSign(d) == BZ_ZERO) {
                return (BQNULL);
        }
#endif

        if ((q = (BigQ)BqAlloc()) == 0) {
                return (BQNULL);
        }

        if (BzGetSign(n) == BZ_ZERO) {
                if (mode == BQ_SET) {
                        BzFree(d);
                        BzFree(n);
                }

                BqSetNumerator(q, BzFromInteger((BzInt)0));
                BqSetDenominator(q, BzFromInteger((BzInt)1));
                return (q);
        }

        if (mode == BQ_COPY) {
                cn = BzCopy(n);
                cd = BzAbs(d);

                if (BzGetSign(n) != BzGetSign(d)) {
                        BzSetSign(cn, BZ_MINUS);
                }
        } else  {
                cn = n;
                cd = d;

                if (BzGetSign(n) != BzGetSign(d)) {
                        BzSetSign(cn, BZ_MINUS);
                }
                BzSetSign(cd, BZ_PLUS);
        }

        if (BzGetSign(n) != BzGetSign(d)) {
                BzSetSign(cn, BZ_MINUS);
        } else  {
                BzSetSign(cn, BZ_PLUS);
        }

        BqSetNumerator(q, cn);
        BqSetDenominator(q, cd);

        if (BzLength(cd) != (BigNumLength)1) {
                BqNormalize(q);
        }

        return (q);
}

static  void
BqNormalize(BigQ q) {
        BigZ n   = BqGetNumerator(q);
        BigZ d   = BqGetDenominator(q);
        BigZ gcd = BzGcd(n, d);
        BigZ one = BzFromInteger((BzInt)1);

        if (BzCompare(gcd, one) != BZ_EQ) {
                BigZ nn = BzDiv(n, gcd);
                BigZ nd = BzDiv(d, gcd);

                BzFree(d);
                BzFree(n);

                BqSetNumerator(q, nn);
                BqSetDenominator(q, nd);
        }

        BzFree(one);
        BzFree(gcd);
}

/*
 *      Public interface
 */

BigQ
BqCreate(const BigZ n, const BigZ d) {
        return (BqCreateInternal(n, d, BQ_COPY));
}

void
BqDelete(const BigQ a) {
        if (a == BQNULL) {
                return;
        } else  {
                const BigZ n = BqGetNumerator(a);
                const BigZ d = BqGetDenominator(a);
                BzFree(n);
                BzFree(d);
                BqFree(a);
        }
}

BigQ
BqAdd(const BigQ a, const BigQ b) {
        if (a == BQNULL || b == BQNULL) {
                return (BQNULL);
        } else  {
                const BigZ an = BqGetNumerator(a);
                const BigZ ad = BqGetDenominator(a);
                const BigZ bn = BqGetNumerator(b);
                const BigZ bd = BqGetDenominator(b);
                BigZ n;
                BigZ d;
                BigZ tmp1;
                BigZ tmp2;

                /*
                 * Compute numerator
                 */

                tmp1 = BzMultiply(an, bd);
                tmp2 = BzMultiply(ad, bn);
                n = BzAdd(tmp1, tmp2);
                BzFree(tmp2);
                BzFree(tmp1);

                /*
                 * Compute denominator
                 */

                d = BzMultiply(ad, bd);

                return (BqCreateInternal(n, d, BQ_SET));
        }
}

BigQ
BqSubtract(const BigQ a, const BigQ b) {
        if (a == BQNULL || b == BQNULL) {
                return (BQNULL);
        } else  {
                const BigZ an = BqGetNumerator(a);
                const BigZ ad = BqGetDenominator(a);
                const BigZ bn = BqGetNumerator(b);
                const BigZ bd = BqGetDenominator(b);
                BigZ n;
                BigZ d;
                BigZ tmp1;
                BigZ tmp2;

                tmp1 = BzMultiply(an, bd);
                tmp2 = BzMultiply(ad, bn);
                n    = BzSubtract(tmp1, tmp2);
                BzFree(tmp2);
                BzFree(tmp1);

                d    = BzMultiply(ad, bd);

                return (BqCreateInternal(n, d, BQ_SET));
        }
}

BigQ
BqMultiply(const BigQ a, const BigQ b) {
        if (a == BQNULL || b == BQNULL) {
                return (BQNULL);
        } else  {
                const BigZ an = BqGetNumerator(a);
                const BigZ ad = BqGetDenominator(a);
                const BigZ bn = BqGetNumerator(b);
                const BigZ bd = BqGetDenominator(b);
                const BigZ n  = BzMultiply(an, bn);
                const BigZ d  = BzMultiply(ad, bd);

                return (BqCreateInternal(n, d, BQ_SET));
        }
}

BigQ
BqDiv(const BigQ a, const BigQ b) {
        if (a == BQNULL || b == BQNULL) {
                return (BQNULL);
        } else  {
                const BigZ an = BqGetNumerator(a);
                const BigZ ad = BqGetDenominator(a);
                const BigZ bn = BqGetNumerator(b);
                const BigZ bd = BqGetDenominator(b);
                const BigZ n  = BzMultiply(an, bd);
                const BigZ d  = BzMultiply(ad, bn);

                if (BzGetSign(n) != BzGetSign(d)) {
                        BzSetSign(n, BZ_MINUS);
                } else  {
                        BzSetSign(n, BZ_PLUS);
                }
                BzSetSign(d, BZ_PLUS);

                return (BqCreateInternal(n, d, BQ_SET));
        }
}

BqCmp
BqCompare(const BigQ a, const BigQ b) {
        if (a == BQNULL || b == BQNULL) {
                return (BQ_ERR);
        } else  {
                const BigZ an = BqGetNumerator(a);
                const BigZ ad = BqGetDenominator(a);
                const BigZ bn = BqGetNumerator(b);
                const BigZ bd = BqGetDenominator(b);
                BigZ tmp1;
                BigZ tmp2;
                BzCmp cmp;

                if (BzGetSign(an) != BzGetSign(bn)) {
                        /*
                         *      Sign differs, easy case!
                         */
                        if (BzGetSign(an) == BZ_MINUS) {
                                return (BQ_LT);
                        } else  if (BzGetSign(an) == BZ_ZERO
                                    && BzGetSign(bn) == BZ_PLUS) {
                                return (BQ_LT);
                        } else  {
                                return (BQ_GT);
                        }
                }

                if (BzCompare(an, bn) == BZ_EQ
                    && BzCompare(ad, bd) == BZ_EQ) {
                        /*
                         *      Numerators and denominators are equal.
                         */
                        return (BQ_EQ);
                }

                tmp1 = BzMultiply(an, bd);
                tmp2 = BzMultiply(ad, bn);
                cmp  = BzCompare(tmp1, tmp2);
                BzFree(tmp2);
                BzFree(tmp1);

                switch (cmp) {
                case BZ_LT: return (BQ_LT);
                case BZ_GT: return (BQ_GT);
                default:    return (BQ_EQ);
                }
        }
}

BigQ
BqNegate(const BigQ a) {
        if (a == BQNULL) {
                return (BQNULL);
        } else  {
                const BigZ an  = BqGetNumerator(a);
                const BigZ ad  = BqGetDenominator(a);
                BigQ       res = BqCreateInternal(an, ad, BQ_COPY);

                switch (BzGetSign(an)) {
                case BZ_MINUS:
                        BzSetSign(BqGetNumerator(res), BZ_PLUS);
                        return (res);
                case BZ_PLUS:
                        BzSetSign(BqGetNumerator(res), BZ_MINUS);
                        return (res);
                default:
                        return (res);
                }
        }
}

BigQ
BqAbs(const BigQ a) {
        if (a == BQNULL) {
                return (BQNULL);
        } else  {
                const BigZ an  = BqGetNumerator(a);
                const BigZ ad  = BqGetDenominator(a);
                BigQ       res = BqCreateInternal(an, ad, BQ_COPY);

                if (BzGetSign(BqGetNumerator(res)) == BZ_MINUS) {
                        BzSetSign(BqGetNumerator(res), BZ_PLUS);
                }

                return (res);
        }
}

BigQ
BqInverse(const BigQ a) {
        if (a == BQNULL) {
                return (BQNULL);
        } else  {
                const BigZ an  = BqGetNumerator(a);
                const BigZ ad  = BqGetDenominator(a);

                return (BqCreateInternal(ad, an, BQ_COPY));
        }
}

/*
 *      Define QNaN as an array (not a pointer!!!) to let sizeof returns
 *      the null terminating string length (including '\000').
 */

static  const char BqNaN[] = "#.QNaN";

BzChar *
BqToString(const BigQ q, int sign) {
        BzChar * n;
        BzChar * d;
        BzChar * res;
        size_t  len;
        int     i;

        if (q == BQNULL) {
                /*
                 * BqToString contract is to allocate a new string, even
                 * for #.QNaN error.
                 */
                len = sizeof(BqNaN); /* works because BqNaN is an array */
                res = (BzChar *)BzStringAlloc(len * sizeof(BzChar));
                if (res != (BzChar *)NULL) {
                        for (i = 0; BqNaN[i] != '\000'; ++i) {
                                res[i] = (BzChar)BqNaN[i];
                        }
                        res[i] = (BzChar)'\000';
                }
                return (res);
        } else  if (BzLength(BqGetDenominator(q)) == (BigNumLength)1) {
                return (BzToString(BqGetNumerator(q), (BigNumDigit)10, sign));
        } else  {
                /*
                 * Get numerator string.
                 */
                n = BzToString(BqGetNumerator(q), (BigNumDigit)10, sign);

                if (n == (BzChar *)NULL) {
                        return (n);
                }

                /*
                 * Get denominator string.
                 */
                d = BzToString(BqGetDenominator(q), (BigNumDigit)10, 0);

                if (d == (BzChar *)NULL) {
                        BzFreeString(n);
                        return (d);
                }

                /*
                 * Compute total length
                 */

                len = BzStrLen(n) + BzStrLen(d) + 2; /* +2 for '/' and '\000' */

                /*
                 * Alloc result string and catenate n/d.
                 */

                if ((res = (BzChar *)BzStringAlloc(len)) != (BzChar *)NULL) {
                        int pos = 0;
                        for (i = 0; n[i] != (BzChar)'\000'; ++i) {
                                res[pos++] = n[i];
                        }

                        res[pos++] = (BzChar)'/';

                        for (i = 0; d[i] != (BzChar)'\000'; ++i) {
                                res[pos++] = d[i];
                        }

                        res[pos] = (BzChar)'\000';
                }

                BzFreeString(d);
                BzFreeString(n);

                return (res);
        }
}

BigQ
BqFromString(const BzChar *s, int base) {
        BigZ n;
        BigZ d;
        BigQ q;
        const BzChar *p;

        if (s == (BzChar *)NULL) {
                return (BQNULL);
        }

        /*
         * Throw away any initial space
         */

        while ((*s == (BzChar)' ')
               || (*s == (BzChar)'\t')
               || (*s == (BzChar)'\n')
               || (*s == (BzChar)'\r')) {
                s++;
        }

        /*
         * search for '/'
         */

        p = s;

        if (*p == (BzChar)'+' || *p == (BzChar)'-') {
                ++p;
        }

        while (*p != (BzChar)'\000') {
                if (*p == '/') {
                        break;
                } else  {
                        ++p;
                }
        }

        if (*p == (BzChar)'\000') {
                /*
                 * simply an integer in Z (no denominator).
                 */
                n = BzFromString(s, (BigNumDigit)base, BZ_UNTIL_END);
                if (n == BZNULL) {
                        return (BQNULL);
                }
                d = BzFromInteger((BzInt)1);
                q = BqCreateInternal(n, d, BQ_SET);
                return (q);
        } else  {
                n = BzFromString(s, (BigNumDigit)base, BZ_UNTIL_INVALID);
                if (n == BZNULL) {
                        return (BQNULL);
                }

                ++p; /* skip slash */

                d = BzFromString(p, (BigNumDigit)base, BZ_UNTIL_END);
                if (d == BZNULL) {
                        BzFree(n);
                        return (BQNULL);
                }

                q = BqCreateInternal(n, d, BQ_COPY);
                BzFree(d);
                BzFree(n);
                return (q);
        }
}

BigQ
BqFromDouble(double num, BzInt maxd) {
        /*
         * find rational approximation to given real number (Farey's method)
         */

        BzInt   ln = (BzInt)0;  /* lower value = 0/1 */
        BzInt   ld = (BzInt)1;
        BzInt   un = (BzInt)1;  /* upper value = 1/0 = oo */
        BzInt   ud = (BzInt)0;
        BzInt   rn = (BzInt)1;
        BzInt   rd = (BzInt)0;
        BigZ    n;
        BigZ    d;
        BigQ    q;
        int     sign;

        /*
         * See: http://en.wikipedia.org/wiki/Farey_series
         *      http://wiki.cs.princeton.edu/index.php/Rational.ck
         */

        if (num < (double)0.0) {
                sign = -1;
                num *= (double)-1.0;
        } else  {
                sign = 1;
        }

        for (;;) {
                const BzInt     mn = ln + un;
                const BzInt     md = ld + ud;

                if ((num * md) > (double)mn) {
                        if (maxd < md) {
                                /*
                                 * return upper.
                                 */
                                rn = un;
                                rd = ud;
                                break;
                        } else  {
                                /*
                                 * set lower to median and continue
                                 */
                                ln = mn;
                                ld = md;
                                continue;
                        }
                } else  if ((num * md) == (double)mn) {
                        if (maxd >= md) {
                                /*
                                 * return median.
                                 */
                                rn = mn;
                                rd = md;
                                break;
                        } else  if (ld < ud) {
                                /*
                                 * return lower.
                                 */
                                rn = ln;
                                rd = ld;
                                break;
                        } else  {
                                /*
                                 * return upper.
                                 */
                                rn = un;
                                rd = ud;
                                break;
                        }
                } else  {
                        if (maxd < md) {
                                /*
                                 * return lower.
                                 */
                                rn = ln;
                                rd = ld;
                                break;
                        } else  {
                                /*
                                 * set lower to median and continue
                                 */
                                un = mn;
                                ud = md;
                                continue;
                        }
                }
        }

        n = BzFromInteger(sign * rn);
        d = BzFromInteger(rd);
        q = BqCreate(n, d);
        BzFree(d);
        BzFree(n);

        return (q);
}

double
BqToDouble(const BigQ a) {
        BzInt  in = (BzInt)0;
        BzUInt id = (BzUInt)1;

        if ((BzToIntegerPointer(BqGetNumerator(a), &in) != 0)
            && (BzToUnsignedIntegerPointer(BqGetDenominator(a), &id) != 0)) {
                return (double)in / (double)id;
        } else {
#if defined(NAN)
                return NAN;
#else
                return 0.0;
#endif
        }
}
