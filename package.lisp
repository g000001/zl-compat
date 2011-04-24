;;;; package.lisp

(defpackage :zl
  (:export :once-only
           :let-if
           :neq
           :defun
           :car-safe))

(defpackage #:zl-internal
  (:use #:cl #:fiveam))



