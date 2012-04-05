(in-package #:zl-internal)

(DEFVAR *NOPOINT NIL)

(DEFVAR NEGATIVE-POWERS-OF-10f0-TABLE :UNBOUND
  "Vector which indexed by i contains (- (^ 10. I)) as a single-float.")

(DEFVAR POWERS-OF-10f0-TABLE :UNBOUND
  "Vector which indexed by I contains (^ 10. I) as a single-float.")

(DEFVAR POWERS-OF-10f0-TABLE-LENGTH 308.)
