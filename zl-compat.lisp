;;;; zl-compat.lisp

(in-package #:zl-internal)

(def-suite tests)

(in-suite tests)

(defmacro zl:defun (name (&rest args) &body body)
  (etypecase name
    ((atom)
     `(cl:defun ,name (,@args) ,@body))
    ((cons)
     (ecase (car name)
       ((:property)
        (destructuring-bind (ignore symbol property)
                            name
          (declare (ignore ignore))
          `(setf (get ',symbol ',property)
                 (lambda (,@args)
                   ,@body))))))))

(defun sharp-quote (stream sub-char numarg)
  (sb-impl::ignore-numarg sub-char numarg)
  ;; The fourth arg tells READ that this is a recursive call.
  (let ((exp (read stream t nil t)))
    (etypecase exp
      ((atom)
       `(function ,exp))
      ((cons)
       (ecase (car exp)
         ((setf)
          `(function ,exp))
         ((:property)
          (destructuring-bind (ignore symbol property)
                              exp
            (declare (ignore ignore))
            `(get ',symbol ',property))))))))

(test sharp-quote
  (is (equal (with-input-from-string (srm "#'(:property foo bar)")
               (let ((*readtable* (copy-readtable nil)))
                 (set-dispatch-macro-character #\# #\' #'sharp-quote)
                 (read srm nil nil)))
             ;;
             '(GET 'FOO 'BAR)))
  ;;
  (is (equal (with-input-from-string (srm "#'(setf foo)")
               (let ((*readtable* (copy-readtable nil)))
                 (set-dispatch-macro-character #\# #\' #'sharp-quote)
                 (read srm nil nil)))
             '#'(SETF FOO))))

#+sbcl
(defun enable-extended-sharp-quote ()
  (set-dispatch-macro-character #\# #\' #'sharp-quote))

#+sbcl
(defun disable-extended-sharp-quote ()
  (set-dispatch-macro-character #\# #\' #'sb-impl::sharp-quote))

;; once-only
;; import from alexandria

;; car-safe

(declaim (inline zl:car-safe))
(defun zl:car-safe (form)
  (if (consp form)
      (car form)
      form))

(test zl:car-safe
  (is (= (zl:car-safe '(1 2 3 4))
         1))
  (is (= (zl:car-safe 1)
         1)))

;; neq
(declaim (inline zl:neq))
(defun zl:neq (x y)
  (not (eq x y)))

(test zl:neq
  (is (eq 'T (zl:neq nil 'T)))
  (is (eq nil (zl:neq 'T 'T))))

;;; (LET-IF <COND> ((VAR-1 VAL-1) (VAR-2 VAL-2) ... (VAR-N VAL-N)) &BODY BODY)
;;; If <COND> is not nil, binds VAR-I to VAL-I (evaluated) during execution of BODY,
;;; otherwise just evaluates BODY.
#|(defun let-if (cond &quote var-list &quote &rest body)
  "Perform the bindings in VAR-LIST only if COND is non-NIL; the execute the BODY.
Aside from the presence of COND, LET-IF is just like LET.
The variables are always bound as specials if they are bound;
therefore, strictly speaking only variables declared special should be used."
  (declare (zwei:indentation 2 1))
  (if (not cond)
      (if (eq *interpreter-function-environment* t)
	  (eval-body body)
	(gobble-declarations-from-body (vars-env body)
	  (eval-body body)))
    ;; Cannot use PROGW here; it calls EVAL rather than EVAL1.
    (if (eq *interpreter-function-environment* t)
	(zl-parallel-binding-list (var-list)
	  (eval-body body))
      (gobble-declarations-from-body (vars-env body)
	(parallel-binding-list (var-list vars-env)
	  (eval-body body))))))|#


#|(defmacro let-if (cond (&rest var-list) &body body)
  (flet ((remove-decls (form)
           (remove 'declare form :key #'car-safe)))
    `(if ,cond
         (let (,@var-list)
           ,@body)
         (progn
           ,@(remove-decls body)))))|#


#|(zl:let-if (probe-file "foo")
           ((foo 3) (bar 4))
  (declare (fixnum foo))
  (list foo bar))|#

