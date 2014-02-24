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
   (dolist (bits '(32 64 128))
      (format t "~%#if defined( _WORD~d )~%" bits)
      (dolist (item '(:value :digits))
        (let ((suffix (if (= bits 32) "" "UL")))
         (cond
          ((eq item :value)
           (format t "static const BigNumDigit value[] = {~%")
           (format t "  /* 00 */ ((BigNumDigit)0~a),~%" suffix)
           (format t "  /* 01 */ ((BigNumDigit)0~a),~%" suffix))
          ((eq item :digits)
           (format t "~%")
           (format t "static const BigNumLength digits[] = {~%")
           (format t "  /* 00 */ ((BigNumLength)0),~%")
           (format t "  /* 01 */ ((BigNumLength)0),~%")))
         (do ((base 2 (1+ base)))
            ((> base 36))
            (do ((i 1 (1+ i))
                 (d base)
                 (sep ",")
                 (max (div (1- (ash 1 bits)) base)))
                ((> d max)
                 (when (= base 36)
                       (setf sep ""))
                 (if (eq item :value)
                     (format t "  /* ~02d */ ((BigNumDigit)~a~a)~a~%"
                             base d suffix sep)
                     (format t "  /* ~02d */ ((BigNumLength)~a)~a~%"
                             base i sep)))
                (setq d (* d base))))
         (format t "};~%")))
      (format t "#endif /* _WORD~d */~%" bits)))

;(maxbase10 32)
;(maxbase10 64)
;(maxbase10 128)

(all-max-base)
