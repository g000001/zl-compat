;;;; zl-compat.asd

(in-package :asdf)

(defsystem #:zl-compat
  :serial t
  :depends-on (:fiveam)
  :components ((:file "package")
               (:file "variables")
               (:file "base")
               (:file "zl-compat")
               (:file "setup")))

(defmethod perform ((o test-op) (c (eql (find-system :zl-compat))))
  (load-system :zl-compat)
  (or (flet ((_ (sym)
         (intern (symbol-name sym)
                 (find-package :zl-internal))))
        (let ((result (funcall (_ :run) (_ :tests))))
          (funcall (_ :explain!) result)
          (funcall (_ :results-status) result)))
      (error "test-op failed") ))
