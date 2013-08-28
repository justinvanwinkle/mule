(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "add_names.py")(make-package "add_names.py" :use '("COMMON-LISP"))(use-package "builtins")))
;(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(DEFUN |test| (  ) (LET ((|x| 1)) (LET ((|y| 2)) (ASSERT (|__eq__| (+ |x| |y|) 3) ))))
(|test|  )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

