(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "global_scope")(make-package "global_scope" :use '("COMMON-LISP"))(use-package "builtins")))
(DEFPARAMETER |x| 1)
(DEFUN |x_plus| (|delta|  ) (SETF |x| (+ |x| |delta|)))
(ASSERT (|__eq__| |x| 1) )
(|x_plus| 5 )
(ASSERT (|__eq__| |x| 6) )
(|x_plus| (- 6 ) )
(ASSERT (|__eq__| |x| 0) )
(|x_plus| (- 5 (- 3 )) )
(ASSERT (|__eq__| |x| 8) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

