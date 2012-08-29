;;;; package.lisp

;; (g1::delete-package* :zl)
(defpackage :zl
  (:use)
  (:export
   :negative-powers-of-10f0-table
   :powers-of-10f0-table
   :powers-of-10f0-table-length
   :%string-search-char :\\
   :adjust-array-size :allocate-resource
   :fixnump
   :*nopoint
   :array-mem :ass :assq :bit-test :caar-safe :cadddr-safe :caddr-safe
   :cadr-safe :car-safe :cddddr-safe :cdddr-safe :cddr-safe :cdr-safe
   :char-bits :character-needs-quoting-p :condition-case :copy-array-portion
   :deallocate-resource :defconst :deff :define-site-variable :defprop
   :defsubst :defun :delq :disable-extended-sharp-quote :do-forever
   :enable-extended-sharp-quote :ensure-no-error-clause :exploden :ferror
   :find-position-in-list :fix :fixp :flatc :flatsize :g-l-p :getl
   :inhibit-style-warnings :let-if :lsh :mem :memq :minus :multiple-value
   :multiple-value-bind :neq :nth-safe :nthcdr-safe :parse-number
   :print-fixnum :pttbl-slash :putprop :remainder :rest1 :select
   :select-processor :selectq  :si>princ-function
   :string-reverse-search-char :string-search :string-search-char
   :string-search-not-char :string-search-not-set :string-search-set :symeval
   :time-increment :with-stack-list :with-stack-list* :without-interrupts
   :xr-table-setup
   :≤ :≥ :≠ :get-pname :string-length :time-difference :substring
   :nsubstring :substring
   :^
   :string-pluralize
   ))

(defpackage #:zl-internal
  (:use #:cl #:fiveam)
  (:shadowing-import-from
   :zl
   :negative-powers-of-10f0-table
   :powers-of-10f0-table
   :powers-of-10f0-table-length
   :%string-search-char :\\ :adjust-array-size :allocate-resource
   :fixnump :*nopoint
   :array-mem :ass :assq :bit-test :caar-safe :cadddr-safe :caddr-safe
   :cadr-safe :car-safe :cddddr-safe :cdddr-safe :cddr-safe :cdr-safe
   :char-bits :character-needs-quoting-p :condition-case :copy-array-portion
   :deallocate-resource :defconst :deff :define-site-variable :defprop
   :defsubst :defun :delq :disable-extended-sharp-quote :do-forever
   :enable-extended-sharp-quote :ensure-no-error-clause :exploden :ferror
   :find-position-in-list :fix :fixp :flatc :flatsize :g-l-p :getl
   :inhibit-style-warnings :let-if :lsh :mem :memq :minus :multiple-value
   :multiple-value-bind :neq :nth-safe :nthcdr-safe :parse-number
   :print-fixnum :pttbl-slash :putprop :remainder :rest1 :select
   :select-processor :selectq  :si>princ-function
   :string-reverse-search-char :string-search :string-search-char
   :string-search-not-char :string-search-not-set :string-search-set :symeval
   :time-increment :with-stack-list :with-stack-list* :without-interrupts
   :xr-table-setup
   :≤ :≥ :≠ :get-pname :string-length :time-difference :substring
   :nsubstring :substring
   :^
   ))
