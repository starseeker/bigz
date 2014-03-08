;;;; -*-Mode: LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Title:     server.lsp
;;;; Author:    C. Jullien
;;;; CVS:       $Id: maxbase10.lsp,v 1.9 2014/03/02 12:04:21 jullien Exp $

(defun all-max-base ()
   (format t "typedef struct {~%")
   (format t "        int         MaxDigits;~%")
   (format t "        BigNumDigit MaxValue;~%")
   (format t "} BzPrintTable;~%")
   (dolist (bits '(32 64))
      (let ((suffix (if (= bits 32)
                        "U"
                        "UL")))
           (format t "~%#if (BZ_BUCKET_SIZE == ~a)~%" bits)
           (format t "static const BzPrintTable BzPrintBase[] = {~%")
           (format t "  {  0, (BigNumDigit)0~a~44t}, /*  0 */~%" suffix)
           (format t "  {  0, (BigNumDigit)0~a~44t}, /*  1 */~%" suffix)
           (do ((base 2 (1+ base)))
               ((> base 36))
               (do ((i 1 (1+ i))
                    (d base)
                    (sep ",")
                    (max (div (1- (ash 1 bits)) base)))
                   ((> d max)
                       (when (= base 36)
                         (setf sep " "))
                         (format t "  { ~2d," i)
                         (format t " (BigNumDigit)~a~a~44t}~a /* ~2d */~%"
                                 d suffix sep base))
                   (setq d (* d base)))))
      (format t "};~%")
   (format t "#endif /* BZ_BUCKET_SIZE == ~a */~%" bits)))

;(maxbase10 32)
;(maxbase10 64)
;(maxbase10 128)

(all-max-base)
