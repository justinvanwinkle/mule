(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "packages")(make-package "packages")(use-package "builtins")))
;(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(CL:USE-PACKAGE "COMMON-LISP")
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

