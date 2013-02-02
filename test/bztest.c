/*
 static	const char rcsid[] = "$Id: bztest.c,v 1.13 2013-02-02 18:17:33 jullien Exp $";
*/

/*
 * Simplified BSD License
 *
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

/*
 *	bztest.c :	
 */

#include <stdio.h>
#include <string.h>
#include "bigz.h"

#define S(A,B)		strcmp(A,B)
#define NEWLINE		fprintf(stderr,"\n")
#define P(A)		fprintf(stderr,"%03d..",A)
#define E(A,B,C)	fprintf(stderr,"\nError in test #%d:\nComputed: %s\nCorrect:  %s\n",A,C,B)
#define T(A,B,C)	S(B,C)?E(A,B,C):P(A)
#define To(A)		BzToString(A,10,0)

#define From(A)		BzFromString(A,10,BZ_UNTIL_END)
#define Abs(A)		BzAbs(A)
#define Neg(A)		BzNegate(A)
#define Add(A,B)	BzAdd(A,B)
#define Sub(A,B)	BzSubtract(A,B)
#define Mul(A,B)	BzMultiply(A,B)
#define Div(A,B)	BzDiv(A,B)
#define Mod(A,B)	BzMod(A,B)
#define Fac(A)		BzFactorial(A)
#define And(A,B)	BzAnd(A,B)
#define Or(A,B)		BzOr(A,B)
#define Xor(A,B)	BzXor(A,B)
#define Ash(A,B)	BzAsh(A,B)

/*
 * bzf.c: Miscellaneous functions built on top of BigZ.
 */

extern BigZ BzFactorial( BigZ z );

BigZ
BzFactorial( BigZ z )
{
	/*
	 * Returns Z!
	 * Assumes Z < Base.
	 */

	BigZ		f;
	BigNumDigit	zval;
	int		fl = 1;

	zval = BnnGetDigit( BzToBn( z ) );
	f = BzCreate( (BigNumLength)zval+1 );
	BnnSetDigit( BzToBn( f ), 1);
	BzSetSign( f, BzGetSign( z ) );

	while( zval-- > 1 ) {
		BnnMultiplyDigit( BzToBn( f ), fl+1, BzToBn( f ), fl, zval );
		fl = BnnNumDigits( BzToBn( f ), fl+1);
	}
	return( f );
}

int
main()
{
   BigZ	a;
   BigZ b;

   printf("BzTest v%s test suite\n", BzVersion());

   printf("sizeof(BigNumDigit)  == %d\n", (int)sizeof(BigNumDigit));
   printf("sizeof(BigZ)         == %d\n", (int)sizeof(BigZ));
   printf("sizeof(BzInt)        == %d\n", (int)sizeof(BzInt));
   printf("sizeof(BzUInt)       == %d\n", (int)sizeof(BzUInt));
   printf("BZ_MAX_BASE10_DIGITS == %d\n", (int)BZ_MAX_BASE10_DIGITS);
#if defined( _WIN64 )
   printf("BZ_MAX_BASE10        == %I64u\n", BZ_MAX_BASE10);
#else
   printf("BZ_MAX_BASE10        == %lu\n", (unsigned long)BZ_MAX_BASE10);
#endif
   printf("\n");

   T(1,"12", To(From("12"))) ;
   T(2,"12345678910", To(From("12345678910"))) ;
   T(3,"123", To(From("00000123"))) ;
   T(4,"-123", To(From("-123"))) ;
   T(5,"-32768", To(From("-32768"))) ;
   T(6,"-32768", To(Neg(From("32768")))) ;
   T(7,"-32768", To(Add(From("-16384"),From("-16384")))) ;
   T(8,"-32768", To(Add(From("-16383"),From("-16385")))) ;
   T(9,"-32768", To(Mul(From("2"),From("-16384")))) ;
   T(10,"-16384", To(Div(From("-32768"),From("2")))) ;
   NEWLINE;
   T(11,"100000", To(Add(From("1"),From("99999")))) ;
   T(12,"12343994",To(Add(From("-1684"),From("12345678"))));
   T(13,"-12329294",To(Sub(From("16384"),From("12345678"))));
   T(14,"135801",To(Add(From("12345"),From("123456"))));
   T(15,"123456135801",To(Add(From("12345"),From("123456123456"))));
   T(16,"135801",To(Add(From("123456"),From("12345"))));
   T(17,"123456135801",To(Add(From("123456123456"),From("12345"))));
   T(18,"135801",To(Sub(From("12345"),From("-123456"))));
   T(19,"123456135801",To(Sub(From("12345"),From("-123456123456"))));
   T(20,"135801",To(Sub(From("123456"),From("-12345"))));
   NEWLINE;
   T(21,"123456135801",To(Sub(From("123456123456"),From("-12345"))));
   T(22,"-111111",To(Sub(From("12345"),From("123456"))));
   T(23,"111111",To(Sub(From("123456"),From("12345"))));
   T(24,"-123456111111",To(Sub(From("12345"),From("123456123456"))));
   T(25,"123456111111",To(Sub(From("123456123456"),From("12345"))));
   T(26,"-111111",To(Add(From("12345"),From("-123456"))));
   T(27,"111111",To(Add(From("123456"),From("-12345"))));
   T(28,"-123456111111",To(Add(From("12345"),From("-123456123456"))));
   T(29,"123456111111",To(Add(From("123456123456"),From("-12345"))));
   T(30,"2", To(Div(From("264195"),From("97200")))) ;
   NEWLINE;
   T(31,"27405", To(Mod(From("97200"),From("69795")))) ;
   T(32,"4294967295", To(Div(From("22685491128062564230891640495451214097"),From("5281877500950955845296219748")))) ;
   T(33,"99997",To(Add(From("-3"),From("100000"))));
   T(34,"-100003",To(Add(From("-3"),From("-100000"))));
   T(35,"999999",To(Sub(From("1000000"),From("1"))));
   T(36,"999999999",To(Mul(From("12345679"),From("81"))));
   a = From("1234567");
   b = From("123456");
   T(37,"1234567",To(Add(Mul(Div(a,b),b),Mod(a,b))));
   T(38,"-1234567",To(Add(Mul(Div(Neg(a),Neg(b)),Neg(b)),Mod(Neg(a),Neg(b)))));
   T(39,"1234567",To(Add(Mul(Div(a,Neg(b)),Neg(b)),Mod(a,Neg(b)))));
   T(40,"10000000000000000000000",To(Mul(From("-100000000000"),From("-100000000000"))));
   NEWLINE;
   T(41,"-10000000000000000000000",To(Mul(From("-100000000000"),From("100000000000"))));
   T(42,"-10000000000000000000000",To(Mul(From("100000000000"),From("-100000000000"))));
   T(43,"10000000000000000000000",To(Mul(From("100000000000"),From("100000000000"))));
   a = Sub(From("10000000000000"),From("10000000000000"));
   T(44,"0",To(Mod(a,From("1000000000000"))));
   T(45,"0",To(Div(a,From("1000000000000"))));
   T(46,"0",To(Mod(Neg(a),From("10000000000000"))));
   T(47,"0",To(Div(Neg(a),From("10000000000000"))));
   T(48,"2",To(Div(From("3000"),Sub(From("1234567891234"),From("1234567890000")))));
   T(49,"532",To(Mod(From("3000"),Sub(From("1234567891234"),From("1234567890000")))));
   T(50,"9",To(Mod(From("-1234567890"),From("1234567899"))));
   NEWLINE;
   T(51,"2",To(Mod(Sub(From("12345678900000"),From("12345678926887")),From("3"))));
   T(52,"40830949904677684825316369628906250000000000000",To(Mul(From("48270948888581289062500000000"),From("845870049062500000"))));
   T(53,"22666179639240748063923391983020279316955515",To(Mul(From("6956883693"),From("3258093801689886619170103176686855"))));
   T(54,"1405006117752879898543142606244511569936384000000000",To(Fac(From("42"))));
   T(55,"0",To(Mod(Fac(From("13")),Fac(From("9")))));
   T(56,"0",To(Mod(Fac(From("34")),Fac(From("13")))));
   T(57,"0",To(Mod(Fac(From("57")),Fac(From("21")))));
   T(58,"0",To(Mod(Fac(From("40")),Fac(From("39")))));
   T(59,"59",To(Div(Fac(From("59")),Fac(From("58")))));
   T(60,"2",To(Div(From("5"),From("2"))));
   NEWLINE;
   T(61,"1",To(Mod(From("5"),From("2"))));
   T(62,"-3",To(Div(From("-5"),From("2"))));
   T(63,"1",To(Mod(From("-5"),From("2"))));
   T(64,"2",To(Div(From("-5"),From("-2"))));
   T(65,"-1",To(Mod(From("-5"),From("-2"))));
   T(66,"-3",To(Div(From("5"),From("-2"))));
   T(67,"-1",To(Mod(From("5"),From("-2"))));
   T(68,"3",To(Div(From("6"),From("2"))));
   T(69,"0",To(Mod(From("6"),From("2"))));
   T(70,"-3",To(Div(From("-6"),From("2"))));
   NEWLINE;
   T(71,"0",To(Mod(From("-6"),From("2"))));
   T(72,"3",To(Div(From("-6"),From("-2"))));
   T(73,"0",To(Mod(From("-6"),From("-2"))));
   T(74,"-3",To(Div(From("6"),From("-2"))));
   T(75,"0",To(Mod(From("6"),From("-2"))));
   T(76,"0",To(Abs(From("0"))));
   T(77,"1234567890",To(Abs(From("1234567890"))));
   T(78,"1234567890",To(Abs(From("-1234567890"))));
   T(79,"1",BzCompare(From("-1234567890"),From("12345"))<0?"1":"0");
   T(80,"1",BzGetSign(From("-1234567890"))<0?"1":"0");
   NEWLINE; 
#if 1
   T(81,"1026",To(And(From("1234"),From("5679"))));
   T(82,"4654",To(And(From("-1234"),From("5679"))));
   T(83,"208",To(And(From("1234"),From("-5679"))));
   T(84,"-5888",To(And(From("-1234"),From("-5679"))));

   T(85,"32",To(And(From("123458956781234"),From("45"))));
   T(86,"12",To(And(From("-123458956781234"),From("45"))));
   T(87,"-123458956781246",To(And(From("-123458956781234"),From("-45"))));
   T(88,"123458956781202",To(And(From("123458956781234"),From("-45"))));
   T(89,"32",To(And(From("45"),From("123458956781234"))));
   T(90,"12",To(And(From("45"),From("-123458956781234"))));
   NEWLINE; 
   T(91,"-123458956781246",To(And(From("-45"),From("-123458956781234"))));
   T(92,"123458956781202",To(And(From("-45"),From("123458956781234"))));

   T(93,"867583672336",To(And(From("9234589568881234"),From("7899898989896764"))));
   T(94,"9233721985208896",To(And(From("9234589568881234"),From("-7899898989896764"))));
   T(95,"-17133620975105660",To(And(From("-9234589568881234"),From("-7899898989896764"))));
   T(96,"7899031406224428",To(And(From("-9234589568881234"),From("7899898989896764"))));

   /* Or */

   T(97,"5887",To(Or(From("1234"),From("5679"))));
   T(98,"-209",To(Or(From("-1234"),From("5679"))));
   T(99,"-4653",To(Or(From("1234"),From("-5679"))));
   T(100,"-1025",To(Or(From("-1234"),From("-5679"))));
   NEWLINE; 
   T(101,"123458956781247",To(Or(From("123458956781234"),From("45"))));
   T(102,"-123458956781201",To(Or(From("-123458956781234"),From("45"))));
   T(103,"-33",To(Or(From("-123458956781234"),From("-45"))));
   T(104,"-13",To(Or(From("123458956781234"),From("-45"))));
   T(105,"123458956781247",To(Or(From("45"),From("123458956781234"))));
   T(106,"-123458956781201",To(Or(From("45"),From("-123458956781234"))));
   T(107,"-33",To(Or(From("-45"),From("-123458956781234"))));
   T(108,"-13",To(Or(From("-45"),From("123458956781234"))));

   T(109,"17133620975105662",To(Or(From("9234589568881234"),From("7899898989896764"))));
   T(110,"-7899031406224426",To(Or(From("9234589568881234"),From("-7899898989896764"))));
   NEWLINE; 

   T(111,"-867583672338",To(Or(From("-9234589568881234"),From("-7899898989896764"))));
   T(112,"-9233721985208898",To(Or(From("-9234589568881234"),From("7899898989896764"))));
   /* Xor */

   T(113,"4861",To(Xor(From("1234"),From("5679"))));
   T(114,"-4863",To(Xor(From("-1234"),From("5679"))));
   T(115,"-4861",To(Xor(From("1234"),From("-5679"))));
   T(116,"4863",To(Xor(From("-1234"),From("-5679"))));

   T(117,"123458956781215",To(Xor(From("123458956781234"),From("45"))));
   T(118,"-123458956781213",To(Xor(From("-123458956781234"),From("45"))));
   T(119,"123458956781213",To(Xor(From("-123458956781234"),From("-45"))));
   T(120,"-123458956781215",To(Xor(From("123458956781234"),From("-45"))));
   NEWLINE; 

   T(121,"123458956781215",To(Xor(From("45"),From("123458956781234"))));
   T(122,"-123458956781213",To(Xor(From("45"),From("-123458956781234"))));
   T(123,"123458956781213",To(Xor(From("-45"),From("-123458956781234"))));
   T(124,"-123458956781215",To(Xor(From("-45"),From("123458956781234"))));

   T(125,"17132753391433326",To(Xor(From("9234589568881234"),From("7899898989896764"))));
   T(126,"-17132753391433322",To(Xor(From("9234589568881234"),From("-7899898989896764"))));
   T(127,"17132753391433322",To(Xor(From("-9234589568881234"),From("-7899898989896764"))));
   T(128,"-17132753391433326",To(Xor(From("-9234589568881234"),From("7899898989896764"))));
   T(129,"160000000",To(Ash(From("80000000"),1)));
   T(130,"320000000",To(Ash(From("80000000"),2)));
   NEWLINE; 
   T(131,"640000000",To(Ash(From("80000000"),3)));
   T(132,"1280000000",To(Ash(From("80000000"),4)));
   T(133,"5120000000",To(Ash(From("80000000"),6)));
   T(134,"10240000000",To(Ash(From("80000000"),7)));
   T(135,"20480000000",To(Ash(From("80000000"),8)));
   T(136,"81920000000",To(Ash(From("80000000"),10)));
   T(137,"-160000000",To(Ash(From("-80000000"),1)));
   T(138,"-320000000",To(Ash(From("-80000000"),2)));
   T(139,"-640000000",To(Ash(From("-80000000"),3)));
   T(140,"-1280000000",To(Ash(From("-80000000"),4)));
   NEWLINE; 

   T(141,"-5120000000",To(Ash(From("-80000000"),6)));
   T(142,"-10240000000",To(Ash(From("-80000000"),7)));
   T(143,"-20480000000",To(Ash(From("-80000000"),8)));
   T(144,"-81920000000",To(Ash(From("-80000000"),10)));
   NEWLINE; 

#endif
   NEWLINE; 

   return( 0 );
}
