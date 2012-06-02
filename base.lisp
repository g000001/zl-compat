(in-package #:zl-internal)

(defmacro defun (name (&rest args) &body body)
  (etypecase name
    (atom
     `(cl:defun ,name (,@args) ,@body) )
    (cons
     (ecase (car name)
       ((:property)
        (destructuring-bind (ignore symbol property)
            name
          (declare (ignore ignore))
          `(setf (get ',symbol ',property)
             (lambda (,@args)
               ,@body ))))))))