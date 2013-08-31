(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "case.py")(make-package "case.py")(use-package "builtins")))
;(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(DEFPARAMETER |a| 1)
(DEFPARAMETER |A| 2)
(ASSERT (|__eq__| (+ |a| 1) |A|) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

