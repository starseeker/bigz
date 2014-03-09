;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Date:	$Id: gentest.lsp,v 1.8 2014/02/23 06:42:31 jullien Exp $
;;;; Title:	gentest.lsp
;;;; License:   Simplified BSD license
;;;; Author:	C. Jullien

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
;;; Generate C++ test data for CBignum class.
;;;

;;; usage:
;;;
;;; > (load "gentest.lsp")
;;; > (test)
;;; > (quit)
;;;

#+openlisp
(progn
       (defglobal *cnt*     0)
       (defglobal *ftest*   0)
       (defglobal *type*    "CBignum")
       (defglobal *test*    "Tz")
)

#-openlisp
(progn
       (defvar *cnt*     0)
       (defvar *ftest*   0)
       (defvar *type*    "CBignum")
       (defvar *test*    "Tz")
)

(defun show-op (a1 op a2 res)
   (incf *cnt*)
   (if (equal res nil)
       (setq res 0))
   (if (equal res t)
       (setq res 1))
   (format t " ~a(~3d, \"~2a\", ~a(~a) ~2a ~a(~a), \"~s\"~68t);~%"
	     *test* *cnt* op *type* a1 op *type* a2 res))

(defun show-fun1 (a1 op res)
   (incf *cnt*)
   (if (equal res nil) (setq res 0))
   (if (equal res t)   (setq res 1))
   (format t " ~a(~3d, \"~8a\", ~2a(~a(~a)), \"~s\"~68t);~%"
	     *test* *cnt* op op *type* a1 res))

(defun show-fun2 (a1 op a2 res)
   (incf *cnt*)
   (if (equal res nil) (setq res 0))
   (if (equal res t)   (setq res 1))
   (format t " ~a(~3d, \"~8a\", ~2a(~a(~a), ~a(~a)), \"~s\"~68t);~%"
	     *test* *cnt* op op *type* a1 *type* a2 res))

(defun function-header ()
   (format t "void test~s(void);~%~%" *ftest*)
   (format t "void~%")
   (format t "test~s(void)~%{~%" *ftest*)
   (incf *ftest*))

(defun function-footer ()
   (format t "}~%~%")
   t)

(defun call-tests ()
   (format t "void tests(void);~%~%")
   (format t "void~%")
   (format t "tests(void)~%{~%")
   (do ((i 0 (1+ i)))
       ((= i *ftest*) t)
       (format t "~8ttest~s();~%" i))
   (function-footer)
   t)

(defun numeric-operators (X1 X2 Y1 Y2 l1 l2)
   (function-header)
   (format t "~%")
   (format t " X1 = \"~s\";~%" X1)
   (format t " X2 = \"~s\";~%" X2)
   (format t " Y1 = \"~s\";~%" Y1)
   (format t " Y2 = \"~s\";~%" Y2)
   (format t "~%")
   (mapc #'(lambda (op name)
	      (show-op "X1" name "X1" (funcall op X1 X1))
	      (show-op "X2" name "X2" (funcall op X2 X2))
	      (show-op "X1" name "Y1" (funcall op X1 Y1))
	      (show-op "X1" name "Y2" (funcall op X1 Y2))
	      (show-op "X2" name "Y1" (funcall op X2 Y1))
	      (show-op "X2" name "Y2" (funcall op X2 Y2))
	      (show-op "Y1" name "X1" (funcall op Y1 X1))
	      (show-op "Y1" name "X2" (funcall op Y1 X2))
	      (show-op "Y2" name "X1" (funcall op Y2 X1))
	      (show-op "Y2" name "X2" (funcall op Y2 X2)))
	 l1
	 l2)
   (function-footer))

(defun binary-shifts (X1 X2 Y1 Y2 l1 l2)
   (function-header)
   (format t "~%")
   (format t " X1 = \"~s\";~%" X1)
   (format t " X2 = \"~s\";~%" X2)
   (format t " Y1 = \"~s\";~%" Y1)
   (format t " Y2 = \"~s\";~%" Y2)
   (format t "~%")
   (mapc #'(lambda (op name)
	      (show-op "X1" name "Y1" (funcall op X1 Y1))
	      (show-op "X1" name "Y2" (funcall op X1 Y2))
	      (show-op "X2" name "Y1" (funcall op X2 Y1))
	      (show-op "X2" name "Y2" (funcall op X2 Y2)))
	 l1
	 l2)
   (function-footer))

(defun logbitp-test (X1 X2 Y1 Y2 l1 l2)
   (function-header)
   (format t "~%")
   (format t " X1 = \"~s\";~%" X1)
   (format t " X2 = \"~s\";~%" X2)
   (format t " Y1 = \"~s\";~%" Y1)
   (format t " Y2 = \"~s\";~%" Y2)
   (format t "~%")
   (mapc #'(lambda (op name)
	      (show-fun2 "X1" name "Y1" (funcall op X1 Y1))
	      (show-fun2 "X1" name "Y2" (funcall op X1 Y2))
	      (show-fun2 "X2" name "Y1" (funcall op X2 Y1))
	      (show-fun2 "X2" name "Y2" (funcall op X2 Y2)))
	 l1
	 l2)
   (function-footer))

(defun binary-functions (X1 X2 Y1 Y2 l1 l2)
   (function-header)
   (format t "~%")
   (format t " X1 = \"~s\";~%" X1)
   (format t " X2 = \"~s\";~%" X2)
   (format t " Y1 = \"~s\";~%" Y1)
   (format t " Y2 = \"~s\";~%" Y2)
   (format t "~%")
   (mapc #'(lambda (op name)
	      (show-fun2 "X1" name "X1" (funcall op X1 X1))
	      (show-fun2 "X2" name "X2" (funcall op X2 X2))
	      (show-fun2 "Y1" name "Y1" (funcall op Y1 Y1))
	      (show-fun2 "Y2" name "Y2" (funcall op Y2 Y2))
	      (show-fun2 "X1" name "Y1" (funcall op X1 Y1))
	      (show-fun2 "X1" name "Y2" (funcall op X1 Y2))
	      (show-fun2 "X2" name "Y1" (funcall op X2 Y1))
	      (show-fun2 "X2" name "Y2" (funcall op X2 Y2))
	      (show-fun2 "Y1" name "X1" (funcall op Y1 X1))
	      (show-fun2 "Y1" name "X2" (funcall op Y1 X2))
	      (show-fun2 "Y2" name "X1" (funcall op Y2 X1))
	      (show-fun2 "Y2" name "X2" (funcall op Y2 X2)))
	 l1
	 l2)
   (function-footer))

(defun unary-functions (X1 X2 Y1 Y2 l1 l2)
   (function-header)
   (format t "~%")
   (format t " X1 = \"~s\";~%" X1)
   (format t " X2 = \"~s\";~%" X2)
   (format t " Y1 = \"~s\";~%" Y1)
   (format t " Y2 = \"~s\";~%" Y2)
   (format t "~%")
   (mapc #'(lambda (op name)
	      (show-fun1 "X1" name (funcall op X1))
	      (show-fun1 "X2" name (funcall op X2))
	      (show-fun1 "Y1" name (funcall op Y1))
	      (show-fun1 "Y2" name (funcall op Y2)))
	 l1
	 l2)
   (function-footer))

(defun testz ()
   (format t "// automatically generated by gentest.lsp~%~%")

   (binary-shifts 45967 -45967 3 32
	  (list #'ash)
	  (list "<<"))

   (binary-shifts 45967 -45967 -3 -32
	  (list #'ash)
	  (list "<<"))

   (binary-shifts 1 -1 69 -69
	  (list #'ash)
	  (list "<<"))

   (binary-shifts 0 -1 69 -69
	  (list #'ash)
	  (list "<<"))

   (logbitp-test 3 64 40 8000000000000000000000
	  (list #'logbitp)
	  (list "logbitp"))

   (numeric-operators
	    ;; small numbers
	    459 -459 12368 -12368
	    (list #'+ #'- #'floor #'mod #'*
		  #'< #'<= #'= #'> #'>=
		  #'logand #'logior #'logxor)
	    (list "+" "-" "/"   "%"   "*"
		  "<" "<=" "==" ">" ">="
		  "&" "|" "^"))

   (numeric-operators
	    ;; small numbers
	    26887 -26887 3 -3
	    (list #'+ #'- #'floor #'mod #'*
		  #'< #'<= #'= #'> #'>=
		  #'logand #'logior #'logxor)
	    (list "+" "-" "/"   "%"   "*"
		  "<" "<=" "==" ">" ">="
		  "&" "|" "^"))

   (numeric-operators
	    ;; mix small and large number
	    45967 -45967 12345895678124 -12345895678124
	    (list #'+ #'- #'floor #'mod #'*
		  #'< #'<= #'= #'> #'>=
		  #'logand #'logior #'logxor)
	    (list "+" "-" "/"   "%"   "*"
		  "<" "<=" "==" ">" ">="
		  "&" "|" "^"))

   (numeric-operators
	    ;; large number only
	    9734514543535253517 -9734514543535253517 12345895678124 -12345895678124
	    (list #'+ #'- #'floor #'mod #'*
		  #'< #'<= #'= #'> #'>=
		  #'logand #'logior #'logxor)
	    (list "+" "-" "/"   "%"   "*"
		  "<" "<=" "==" ">" ">="
		  "&" "|" "^"))

   (numeric-operators
	    ;; mix 0/1 and large number
	    0 1 973451454389535253517 -973451454389535253517
	    (list #'+ #'- #'*
		  #'< #'<= #'= #'> #'>=
		  #'logand #'logior #'logxor)
	    (list "+" "-" "*"
		  "<" "<=" "==" ">" ">="
		  "&" "|" "^"))
   (numeric-operators
	    ;; mix 0/1/-1
	    0 -1 -1 1
	    (list #'+ #'- #'*
		  #'< #'<= #'= #'> #'>=
		  #'logand #'logior #'logxor)
	    (list "+" "-" "*"
		  "<" "<=" "==" ">" ">="
		  "&" "|" "^"))

   (binary-functions
	   ;; small numbers
	   459 -459 12368 -12368
	   (list #'floor #'ceiling #'round #'gcd #'lcm)
	   (list "floor" "ceiling" "round" "gcd" "lcm"))

   (binary-functions
	    ;; mix small and large number
	   45967 -45967 12345895678124 -12345895678124
	   (list #'floor #'ceiling #'round #'gcd #'lcm)
	   (list "floor" "ceiling" "round" "gcd" "lcm"))

   (binary-functions
	   ;; large number only
	   9734514543535253517 -9734514543535253517 12345895678124 -12345895678124
	   (list #'floor #'ceiling #'round #'gcd #'lcm)
	   (list "floor" "ceiling" "round" "gcd" "lcm"))

   (binary-functions
	   ;; mix 0 / 1 and large number
	   0 1 8912345895678124 -8912345895678124
	   (list #'gcd)
	   (list "gcd"))

   (binary-functions
	   ;; mix 0 / 1 and large number
	   3 10 89 10
	   (list #'expt)
	   (list "pow"))

   (unary-functions
	   45967 -45967 12345895678124 -12345895678124
	   (list #'integer-length)
	   (list "length"))

   (unary-functions
	   0 -1 4596712345895678124894 -555555555555555555555555
	   (list #'integer-length #'evenp #'oddp)
	   (list "length" "evenp" "oddp"))

   (unary-functions
	   0 7 8 9
	   (list #'integer-length #'evenp #'oddp #'isqrt)
	   (list "length" "evenp" "oddp" "isqrt"))

   (unary-functions
	   -6 -7 -8 -9
	   (list #'integer-length #'evenp #'oddp)
	   (list "length" "evenp" "oddp"))

   (unary-functions
	   45967 -45967 1234589567812456786 -1234589567812456786
	   (list #'abs #'lognot #'-)
	   (list "abs" "~" "-"))

   (unary-functions
	   0 1 -1 -2
	   (list #'lognot)
	   (list "~"))

   (unary-functions
	   160000000000 160000000001 159999999999 687653234589567812456786887
	   (list #'isqrt #'lognot #'-)
	   (list "isqrt" "~" "-"))

   t)

(defun testq ()
   (format t "// automatically generated by gentest.lsp~%~%")

   (numeric-operators
	    1/3 2/3 -2/7 1/6
	    (list #'+ #'- #'/ #'*
		  #'< #'<= #'= #'> #'>=)
	    (list "+" "-" "/" "*"
		  "<" "<=" "==" ">" ">="))
   (numeric-operators
	    1/3 1 -2/7 -2
	    (list #'+ #'- #'/ #'*
		  #'< #'<= #'= #'/= #'> #'>=)
	    (list "+" "-" "/" "*"
		  "<" "<=" "==" "!=" ">" ">="))
   (numeric-operators
	    1/3 0 0 -2/7
	    (list #'+ #'- #'*
		  #'< #'<= #'=  #'/= #'> #'>=)
	    (list "+" "-" "*"
		  "<" "<=" "==" "!=" ">" ">="))

   (unary-functions
	    1/3 2/3 -2/7 1/6
	   (list #'abs #'- #'numerator #'denominator)
	   (list "abs" "-" "numerator" "denominator"))
   t)

;;; Generate the tests.

#+openlisp
(progn
      (testz)
      (call-tests))

#-openlisp
(let ((*type* nil)
      (*cnt* 0)
      (*test* nil))
      (setq *type* "CBignum")
      (setq *test* "Tz")
      (testz)
      (setq *type* "CRational")
      (setq *test* "Tz")
      (testq)
      (call-tests))
      
