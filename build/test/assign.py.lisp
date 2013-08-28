(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "assign.py")(make-package "assign.py" :use '("COMMON-LISP"))(use-package "builtins")))
;(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(DEFPARAMETER |x| 1)
(DEFPARAMETER |y| 2)
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

