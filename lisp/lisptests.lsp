;;;; -*-Mode: LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Title:     lisptests.lsp
;;;; Author:    C. Jullien
;;;; License:   Simplified BSD license
;;;; CVS:       $Id: lisptests.lsp,v 1.3 2011-12-22 06:36:59 jullien Exp $

;;;
;;; Simplified BSD License
;;;
;;; Copyright (c) 1988-1989, Digital Equipment Corporation & INRIA.
;;; Copyright (c) 1992-2012, Eligis
;;; All rights reserved.
;;;
;;; Redistribution and  use in  source and binary  forms, with  or without
;;; modification, are permitted provided that the following conditions are
;;; met:
;;;
;;; o Redistributions  of  source  code must  retain  the  above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;; o Redistributions  in  binary form  must reproduce the above copyright
;;;   notice, this list of conditions and  the following disclaimer in the
;;;   documentation and/or other materials provided with the distribution.
;;; 
;;; THIS SOFTWARE  IS PROVIDED BY  THE COPYRIGHT HOLDERS  AND CONTRIBUTORS
;;; "AS  IS" AND  ANY EXPRESS  OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT
;;; LIMITED TO, THE IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE  ARE DISCLAIMED. IN NO EVENT  SHALL THE COPYRIGHT
;;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL,  EXEMPLARY,  OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE  GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS  INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF  LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY,  OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING  IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;;;
;;; Generate automatic non-regression tests for OpenLisp.
;;;

(defconstant big1+ #x80000000000000000)
(defconstant big2+ #x80000000000000000)
(defconstant big3+ #x80000000000000001)
(defconstant big4+ #x7ffffffffffffffff)
(defconstant big5+ #x7de0faa32c856dcf1)
(defconstant big6+ #xe58a4cdff104ebb74)

(defconstant big1- (- big1+))
(defconstant big2- (- big2+))
(defconstant big3- (- big3+))
(defconstant big4- (- big4+))
(defconstant big5- (- big5+))
(defconstant big6- (- big6+))

(defconstant small1+ 413)
(defconstant small1- -413)
(defconstant small2+ 223)
(defconstant small2- -223)

(defconstant zero 0)

#+openlisp (defun getvalue (x) (if (numberp x) x (symbol-global x)))
#-openlisp (defun getvalue (x) (if (numberp x) x (symbol-value x)))

#+openlisp
(defun rationalp (x)
   nil)

#-openlisp
(defun bignump (x)
   (or (> x #x7fffffff) (< x #x-7fffffff)))

#-openlisp
(defun reciprocal (x)
   (/ 1.0 x))

#-openlisp
(defun quotient (x y)
   (if (/= (mod x y) 0)
       (/ x (float y))
       (floor x y)))

#-openlisp
(defun div (x y)
   ;; We use floor for CLtL:
   ;; floor converts its argument by truncating toward negative infinity;
   ;; that is, the result is the largest integer that is not larger than the
   ;; argument.
   ;;
   ;; Which is like div for ISLISP:
   ;; div returns the greatest integer less than or equal to the quotient
   ;; of z1 and z2.
   (floor x y))

#-openlisp
(defun string-append (&rest l)
   (apply #'string-concat l))

#-openlisp
(defmacro defglobal (x val)
   `(defconstant ,x ,val))

#-openlisp
(defmacro dynamic-let (&rest l)
   `(let ,@l))

(defun lowercase (x)
   (if (numberp x)
       x
       (string-downcase x)))

(defglobal *header* (list
   ";;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-~%"
   ";;;; Title:     testbign.lsp~%"
   ";;;; Author:    C. Jullien~%"
   ";;;; License:   New BSD license~%"
   ";;;; CVS:       $" "Id" "$~%"
   "~%"
   ";;;~%"
   ";;; Auto-test for ISLISP bignums.~%"
   ";;; (automatically generated by bignum/lisptest.lsp)~%"
   ";;;~%"
   "~%"
   "(progn~%"
   "       (if (not (featurep 'bignum))~%"
   "           (error \"testbign.lsp: bignum module is required~~%\"))~%"
   "~%"
   "       (defglobal big1+ " (lowercase (format nil "#x~x" big1+)) ")~%"
   "       (defglobal big2+ " (lowercase (format nil "#x~x" big2+)) ")~%"
   "       (defglobal big3+ " (lowercase (format nil "#x~x" big3+)) ")~%"
   "       (defglobal big4+ " (lowercase (format nil "#x~x" big4+)) ")~%"
   "       (defglobal big5+ " (lowercase (format nil "#x~x" big5+)) ")~%"
   "       (defglobal big6+ " (lowercase (format nil "#x~x" big6+)) ")~%"
   "~%"
   "       (defglobal big1- (- big1+))~%"
   "       (defglobal big2- (- big2+))~%"
   "       (defglobal big3- (- big3+))~%"
   "       (defglobal big4- (- big4+))~%"
   "       (defglobal big5- (- big5+))~%"
   "       (defglobal big6- (- big6+))~%"
   "~%"
   "       (defglobal small1+ 413)~%"
   "       (defglobal small1- -413)~%"
   "       (defglobal small2+ 223)~%"
   "       (defglobal small2- -223)~%"
   "~%"
   "       (defglobal zero 0)~%"
   "       (defglobal *epsilon* 1e-4)~%"
   "~%"
   "       t)~40tt~%"
   "~%"
   "(test-serie \"\")~40tnil~%"
   "(test-serie \"Bignums\")~40tnil~%"
   "(test-serie \"-------\")~40tnil~%"
))

(defun print-header (fn)
   (format t "~%(test-serie \"Bign - ~a\")~40t()~%~%"
           (string-downcase fn)))

(defun call1 (f x)
   (let ((res (funcall (symbol-function f) (getvalue x))))
        (when (and (integerp res)
                   (/= res 99999999)) ;; make isqrt result looks better
              (setf res (lowercase (format () "#x~x" res))))
        (format t "(~a ~a) ~40t~a~%"
                (lowercase f)
                (lowercase x)
                (lowercase res))))

(defun call2 (f x y)
   (let ((res (funcall (symbol-function f) (getvalue x) (getvalue y))))
        (cond
              ((integerp res)
               (setf res (lowercase (format () "#x~x" res))))
              ((and (eq f '/) (rationalp res))
               (setq res (funcall #'/ (getvalue x) (float (getvalue y))))))
        (format t "(~a ~a ~a) ~40t~a~%"
                (lowercase f)
                (lowercase x)
                (lowercase y)
                (lowercase res))))

(defun call-logtest (x y)
   (format t "(logtest~%  #b~68,'0b~%  #b~68,'0b)~75t~a~%"
           x y (lowercase (logtest x y))))

(defun test-reader (x)
   (setq x (getvalue x))
   (dolist (fmt '("~d" "~@d" "#x~x" "#o~o" "#b~b" "#3r~3r" "#36r~36r"))
      (setq fmt (string-append fmt "~%~40t~a~%"))
      (format t (lowercase (format nil fmt x x)))))

(defun test-parser (x)
   (setq x (getvalue x))
   (dolist (fmt '("~d" "~@d" "#x~x" "#o~o" "#b~b" "#3r~3r" "#36r~36r"))
      (setq fmt (string-append "(parse-number \"" fmt "\")~%~40t~a~%"))
      (format t (lowercase (format nil fmt x x)))))

(defun call-format (fmt x)
   (cond
         ((member fmt '("~e" "~f" "~g") :test #'equal)
          (format t "(parse-number (format nil \"~a\" ~a))~40t~a~%"
                  fmt (lowercase x) (format nil "~1,5e" (getvalue x))))
         (t
          (format t "(format nil \"~a\" ~a) ~40t\"~a\"~%"
                  fmt
                  (lowercase x)
                  (lowercase (format nil fmt (getvalue x)))))))

(defun test-print-base (base x)
   (format t "(test-print-base ~a ~a)~40t\"~a\"~%"
             (lowercase x)
             base
             (lowercase (dynamic-let ((*print-base* base))
                                     (write-to-string (getvalue x))))))

(defun test-read-base (base x)
   (format t "(dynamic-let ((*read-base* ~a))~%" base)
   (format t "   (let ((s \"~a\")~%"
             (lowercase (dynamic-let ((*print-base* base))
                                     (write-to-string (getvalue x)))))
   (format t "         (n 0))~%")
   (format t "        (setq n (read-from-string s))~%")
   (format t "        (and (= n (parse-number s)) n)))~%")
   (format t "#.~a~%" (lowercase x)))

(defun test-read-base (base x)
   (format t "(test-read-base \"~a\" ~a)~40t#.~a~%"
             (lowercase (dynamic-let ((*print-base* base))
                                     (write-to-string (getvalue x))))
             base
             (lowercase x)))

(defun test-conversion (x)
   (format t (lowercase (format nil "(convert \"~a\" <integer>)~40t~a~%"
                                (getvalue x) (getvalue x))))
   (format t (lowercase (format nil "(convert \"#x~x\" <integer>)~40t~a~%"
                                (getvalue x) (getvalue x))))
   (format t (lowercase (format nil "(convert \"~a\" <float>)~40t~a~%"
                                (getvalue x) (float (getvalue x)))))
   (format t (lowercase (format nil "(convert \"#x~x\" <float>)~40t~a~%"
                                (getvalue x) (float (getvalue x)))))
   (format t (lowercase (format nil "(convert ~a <string>)~40t\"~a\"~%"
                                x (getvalue x))))
   t)

(defun test-float (x)
   (format t (lowercase (format nil "(float ~a)~40t~a~%"
                                (getvalue x) (float (getvalue x)))))
   t)

(defun add-test-logbitp ()
   (format t "(defun test-logbitp (x)~%")
   (format t "   (let ((l nil))~%")
   (format t "        (do ((i 0 (1+ i)))~%")
   (format t "            ((= i 70))~%")
   (format t "            (setf l (cons (if (logbitp i x) #\\1 #\\0) l)))~%")
   (format t "        (convert l <string>)))~40t:ignore~%")
   (format t "~%"))

(defun add-test-print-base ()
   (format t "(defun test-print-base (n base)~%")
   (format t "   (dynamic-let ((*print-base* base))~%")
   (format t "           (write-to-string n)))~40t\test-print-base~%~%"))

(defun add-test-read-base ()
   (format t "(defun test-read-base (s base)~%")
   (format t "   (dynamic-let ((*read-base* base))~%")
   (format t "       (let ((n (read-from-string s)))~%")
   (format t "            (and (= n (parse-number s)) n))))~%")
   (format t "test-read-base~%~%"))

(defun add-test-random ()
   (format t "(defun test-random (x)~%")
   (format t "   (and (integerp (random x))~%")
   (format t "        (/= (random x) (random x))~%")
   (format t "        (= (progn (set-random big1+) (random x))~%")
   (format t "           (progn (set-random big1+) (random x)))))~40t:ignore~%")
   (format t "~%"))

(defun test-logbitp (x)
   (setq x (getvalue x))
  (format t "(test-logbitp #x~a)~40t\"" (lowercase (format nil "~x" x)))
   (let ((l nil))
        (do ((i 0 (1+ i)))
            ((= i 70))
            (setf l (cons (logbitp i x) l)))
        (dolist (c l)
           (format t (if c "1" "0")))
        (format t "\"~%" x)))

(defun test-random (x res)
   (setq x (getvalue x))
   (format t "(test-random ~d)~40t~a~%" x res))

(defun make-test ()
   (let ((args '(big1+ big2+ big3+ big4+ big5+ big6+ small1+
                 zero
                 big1- big2- big3- big4- big5- big6- small1-)))
        (dolist (line *header*)
           (format t line))
        ;; reader
        (print-header 'reader)
        (dolist (arg '(big1+ big1- big5+ big5- #36ropenlisp))
           (test-reader arg))
        ;; parser
        (print-header 'parse-number)
        (dolist (arg '(big1+ big1- big5+ big5- #36ropenlisp))
           (test-parser arg))
        ;; format
        (print-header 'format)
        (dolist (arg '(big1+ big1- big5+ big5- #36ropenlisp))
           (call-format "~x" arg)
           (call-format "~@d" arg)
           (call-format "~d" arg)
           (call-format "~o" arg)
           (call-format "~b" arg)
           (call-format "~3r" arg)
           (call-format "~36r" arg)
           (call-format "~f" arg)
           (call-format "~g" arg)
           (call-format "~e" arg)
           t)
        ;; *print-base*
        (print-header '*print-base*)
        (add-test-print-base)
        (dolist (arg '(big5+ big5-))
           (do ((base 2 (1+ base)))
               ((= base 37))
               (test-print-base base arg)))
        ;; *read-base*
        (print-header '*read-base*)
        (add-test-read-base)
        (dolist (arg '(small1+ small1- big5+ big5-))
           (do ((base 2 (1+ base)))
               ((= base 37))
               (test-read-base base arg)))
        ;; tests
        (dolist (f '(integerp numberp bignump floatp consp vectorp symbolp))
           (print-header f)
           (dolist (arg '(big5+ big5- zero))
              (call1 f arg)))
        ;; convert
        (print-header 'convert)
        (dolist (arg args)
           (test-conversion arg))
        ;; float
        (print-header 'float)
        (dolist (arg args)
           (test-float arg))
        ;; basic math
        (dolist (f '(eq eql equal = /= > >= < <=))
           (print-header f)
           (call2 f 'big1+ 'big2+)
           (call2 f 'big1+ 'big1+)
           (call2 f 'big1+ 'big1-)
           (call2 f 'big1+ 'big2-)
           (call2 f 'big1- 'zero)
           (call2 f 'big1- 'big2+)
           (call2 f 'big1- 'big1+)
           (call2 f 'big1- 'big1-)
           (call2 f 'big1- 'big2-)
           (call2 f 'big1- 'zero)
           (call2 f 'big5+ 'small1+)
           (call2 f 'big5+ 'small1-)
           (call2 f 'big5- 'small1+)
           (call2 f 'big5- 'small1-)
           (call2 f 'small1+ 'big5+)
           (call2 f 'small1+ 'big5-)
           (call2 f 'small1- 'big5+)
           (call2 f 'small1- 'big5-)
           t)
        ;; basic math
        (dolist (f '(+ - * / quotient div rem mod gcd lcm))
           (print-header f)
           (call2 f 'big5+ -17)
           (call2 f 'big5+ 17)
           (call2 f 'big5+ 72)
           (call2 f 'big5+ -72)
           (call2 f 'big5- -17)
           (call2 f 'big5- 17)
           (call2 f 'big5- 72)
           (call2 f 'big5- -72)
           (call2 f 'zero 'big5+)
           (call2 f 'zero 'big5-)
           (call2 f 'big5+ 'big5+)
           (call2 f 'big5+ 'big5-)
           (call2 f 'big5- 'big5+)
           (call2 f 'big5- 'big5-)
           (call2 f 'big5+ 'small1+)
           (call2 f 'big5+ 'small1-)
           (call2 f 'big5- 'small1+)
           (call2 f 'big5- 'small1-)
           (call2 f 'small1+ 'big5+)
           (call2 f 'small1+ 'big5-)
           (call2 f 'small1- 'big5+)
           (call2 f 'small1- 'big5-)
           t)
        ;; reciprocal
        (dolist (f '(reciprocal))
           (print-header f)
           (dolist (arg '(big5+ big6+ big5- big6- small1+ small1-))
              (call1 f arg)))
        ;; ash
        (dolist (f '(ash))
           (print-header f)
           (dolist (arg args)
              (call2 f arg -17)
              (call2 f arg +17)
              (call2 f arg -32)
              (call2 f arg +32)
              (call2 f arg +72)
              (call2 f arg -72)
              t))
        ;; isqrt
        (dolist (f '(isqrt))
           (print-header f)
           (dolist (arg '(big1+ big2+ big3+ big4+ big5+ big6+ small1+
                          zero 9999999999999999))
              (call1 f arg)))
        ;; div & alt.
        (dolist (f '(plusp minusp zerop evenp oddp))
           (print-header f)
           (dolist (arg args)
              (call1 f arg)))
        ;; rounding & alt.
        (dolist (f '(ceiling floor truncate round))
           (print-header f)
           (dolist (arg (cons 10000000000000000003 args))
              (call1 f arg)
              (call2 f arg -2)
              (call2 f arg +2)
              (call2 f arg -10)
              (call2 f arg +10)
              (call2 f arg 'big1+)
              (call2 f arg 'big1-)
              (call2 f arg -17)
              (call2 f arg +17)
              (call2 f arg -32)
              (call2 f arg +32)
              (call2 f arg +72)
              (call2 f arg -72)))
        ;; logxxx.
        (dolist (f '(logand logeqv logior lognand lognor logxor
                     logandc1 logandc2 logorc1 logorc2))
           (print-header f)
           (call2 f 'big5+ 'big1+)
           (call2 f 'big5+ 'big2+)
           (call2 f 'big5+ 'big3+)
           (call2 f 'big5+ 'big4+)
           (call2 f 'big5+ 'big5+)
           (call2 f 'big5+ 'big6+)
           (call2 f 'big5+ 'big1-)
           (call2 f 'big5+ 'big2-)
           (call2 f 'big5+ 'big3-)
           (call2 f 'big5+ 'big4-)
           (call2 f 'big5+ 'big5-)
           (call2 f 'big5+ 'big6-)
           (call2 f 'big5+ 'zero)
           (call2 f 'big5- 'big1+)
           (call2 f 'big5- 'big2+)
           (call2 f 'big5- 'big3+)
           (call2 f 'big5- 'big4+)
           (call2 f 'big5- 'big5+)
           (call2 f 'big5- 'big6+)
           (call2 f 'big5- 'big1-)
           (call2 f 'big5- 'big2-)
           (call2 f 'big5- 'big3-)
           (call2 f 'big5- 'big4-)
           (call2 f 'big5- 'big5-)
           (call2 f 'big5- 'big6-)
           (call2 f 'big5- 'zero)
           (call2 f 'small1+ 'small2-)
           (call2 f 'small1+ 'small2+)
           (call2 f 'small1- 'small2-)
           (call2 f 'small1- 'small2+)
           (call2 f 'big5+ 'small1+)
           (call2 f 'big5+ 'small1-)
           (call2 f 'big5- 'small1+)
           (call2 f 'big5- 'small1-)
           (call2 f 'small1+ 'big5+)
           (call2 f 'small1+ 'big5-)
           (call2 f 'small1- 'big5+)
           (call2 f 'small1- 'big5-)
           t)
        ;; lognot.
        (dolist (f '(lognot))
           (print-header f)
           (dolist (arg args)
              (call1 f arg)))
        ;; logtest.
        (dolist (f '(logtest))
           (print-header f)
           (call-logtest 147573952589676412927 147573952589676412928)
           (call-logtest 221360928884514619391 16384)
           (call-logtest 221360928884514619391 562949953421312)
           (call-logtest 221360365934561198079 73787539244791627776)
           (call-logtest 221360365934561198079 147574515539629834240))
        ;; logbitp
        (print-header 'logbitp)
        (add-test-logbitp)
        (dolist (arg args)
           (test-logbitp arg))
        ;; integer-length
        (dolist (f '(integer-length))
           (print-header f)
           (dolist (arg '(big1+ big1- big5+ big5- small1+ small1-
                          1 -1 2 -2 4 -4 -5 -5 0))
              (call1 f arg)))
        ;; random
        (print-header 'random)
        (add-test-random)
        (dolist (arg '(big1+ big3+ big4+ big5+))
           (test-random arg "t"))
        (dolist (arg '(big1- 0))
           (test-random arg ":error"))
        t))

(make-test)
