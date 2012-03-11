;;;; zl-compat.lisp

(in-package #:zl-internal)

(def-suite tests)

(in-suite tests)

(defmacro defun (name (&rest args) &body body)
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
  (declare (ignore sub-char numarg))
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

(declaim (inline
          car-safe
          cdr-safe
          caar-safe
          cadr-safe
          cdar-safe
          cddr-safe
          caddr-safe
          cdddr-safe
          cadddr-safe
          cddddr-safe
          nth-safe
          nthcdr-safe ))

(defun nthcdr-safe (n object)
  (and (consp object)
       (nthcdr n object)))

(defun nth-safe (n object)
  (and (nthcdr-safe n object)
       (nth n object)))

(declaim (inline car-safe))
(defun car-safe (object)
  (nth-safe 0 object))

(defun cdr-safe (object)
  (nthcdr-safe 1 object))

(defun caar-safe (object)
  (car-safe (car-safe object)))

(defun cadr-safe (object)
  (nth-safe 1 object))

(defun cddr-safe (object)
  (nthcdr-safe 2 object))

(defun caddr-safe (object)
  (nth-safe 2 object))

(defun cdddr-safe (object)
  (nthcdr-safe 3 object))

(defun cadddr-safe (object)
  (nth-safe 3 object))

(defun cddddr-safe (object)
  (nthcdr-safe 4 object))

(test car-safe
  (is (= (car-safe '(1 2 3 4))
         1))
  (is (null (car-safe 1))))

;; neq
(declaim (inline neq))
(defun neq (x y)
  (not (eq x y)))

(test neq
  (is (eq 'T (neq nil 'T)))
  (is (eq nil (neq 'T 'T))))

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


#|(let-if (probe-file "foo")
           ((foo 3) (bar 4))
  (declare (fixnum foo))
  (list foo bar))|#
