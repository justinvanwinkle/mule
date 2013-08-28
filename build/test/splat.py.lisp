(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "splat.py")(make-package "splat.py" :use '("COMMON-LISP"))(use-package "builtins")))
;(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(DEFUN |f| (|a|  &REST |args|) (LET ((|x| |a|)) (LOOP FOR |arg| BEING THE ELEMENTS OF |args| DO (SETF |x| (+ |x| |arg|)))
(RETURN-FROM |f| |x|)))
(ASSERT (|__eq__| (|f| 1 1 2 ) 4) )
(ASSERT (|__eq__| (|f| 1 ) 1) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

