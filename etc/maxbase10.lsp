;;;; -*-Mode: LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Title:     server.lsp
;;;; Author:    C. Jullien
;;;; CVS:       $Id: maxbase10.lsp,v 1.5 2014/02/24 06:23:32 jullien Exp $

(defun maxbase10 (bits)
   (do ((i 1 (1+ i))
        (d 10)
        (max (div (1- (ash 1 bits)) 10)))
       ((> d max)
        (format t "#if defined( _WORD~d )~%" bits)
        (format t "#if !defined( BZ_MAX_BASE10 )~%")
        (format t "#define BZ_MAX_BASE10~30t((BigNumDigit)~a)~%" d)
        (format t "#endif /* BZ_MAX_BASE10 */~%")
        (format t "#if !defined( BZ_MAX_BASE10_DIGITS )~%" i)
        (format t "#define BZ_MAX_BASE10_DIGITS~30t((BigNumLength)~a)~%" i)
        (format t "#endif /* BZ_MAX_BASE10_DIGITS */~%" i)
        (format t "#endif /* _WORD~d */~%~%" bits))
       (setq d (* d 10))))

(defun all-max-base ()
   (format t "typedef struct {~%")
   (format t "        unsigned int MaxDigits;~%")
   (format t "        BigNumDigit  MaxValue;~%")
   (format t "} BzPrintBase;~%")
   (format t "~%")

   (dolist (bits '(32 64))
      (format t "~%#if (BZ_MAX_BASE_BUCKET_SIZE == ~a)~%" bits)
      (format t "static BzPrintBase[BZ_MAX_BASE + 1] = {~%")
      (format t "  /*  0 */ {  0, ((BigNumDigit)0)~58t},~%")
      (format t "  /*  1 */ {  0, ((BigNumDigit)0)~58t},~%")
      (let ((suffix (if (= bits 32) "" "UL")))
           (do ((base 2 (1+ base)))
               ((> base 36))
               (do ((i 1 (1+ i))
                    (d base)
                    (sep ",")
                    (max (div (1- (ash 1 bits)) base)))
                   ((> d max)
                       (when (= base 36)
                         (setf sep ""))
                         (format t "  /* ~2d */ { ~2d,"
                                 base i)
                         (format t " ((BigNumDigit)~a~a~a)~58t}~a~%"
                                 base d suffix sep))
                   (setq d (* d base)))))
      (format t "};~%")
   (format t "#endif /* BZ_MAX_BASE_BUCKET_SIZE == ~a */~%" bits)))

;(maxbase10 32)
;(maxbase10 64)
;(maxbase10 128)

(all-max-base)
