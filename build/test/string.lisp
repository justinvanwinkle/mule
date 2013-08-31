(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "string")(make-package "string")(use-package "builtins")))
;(proclaim '(optimize (space 0) (safety 0) (speed 3)))
#| equality of STRING and str
|#
(ASSERT (|__eq__| "test" (|str| "test")) )
(ASSERT (|__eq__| (|str| "") "") )
(ASSERT (|__eq__| (|len| (|str| "") ) 0) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

