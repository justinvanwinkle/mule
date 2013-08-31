(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "exception.py")(make-package "exception.py")(use-package "builtins")))
;(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(DEFUN |foo| (  ) (LET ((|x| 0)) (LET ((|y| NIL)) None
(RETURN-FROM |foo| |y|))))
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

