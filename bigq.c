/*
 * $Id: bigq.c,v 1.40 2015/12/19 08:15:23 jullien Exp $
 */

/*
 * Simplified BSD License
 *
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

#if     !defined(__BIGQ_H)
#include "bigq.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

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
