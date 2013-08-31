(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "literal")(make-package "literal")(use-package "builtins")))
;(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(ASSERT (|__eq__| "I WORK" "I WORK") )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

