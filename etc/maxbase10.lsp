;;;; -*-Mode: LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Title:     server.lsp
;;;; Author:    C. Jullien
;;;; CVS:       $Id: maxbase10.lsp,v 1.2 2012-01-01 21:35:37 jullien Exp $

(defun maxbase10 (bits)
   (do ((i 1 (1+ i))
        (d 10)
        (max (div (ash 2 (- bits 1)) 10)))
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

(maxbase10 32)
(maxbase10 64)
(maxbase10 128)
