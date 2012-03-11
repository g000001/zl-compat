;;;; package.lisp

(defpackage :zl
  (:use)
  (:export :once-only
           :let-if
           :neq
           :defun
           :car-safe
           :cdr-safe
           :caar-safe
           :cadr-safe
           :cdar-safe
           :cddr-safe
           :caddr-safe
           :cdddr-safe
           :cadddr-safe
           :cddddr-safe
           :nth-safe
           :nthcdr-safe))

(defpackage #:zl-internal
  (:use #:cl #:fiveam :zl)
  (:shadowing-import-from :zl :defun))
