(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "tuple.py")(make-package "tuple.py" :use '("COMMON-LISP"))(use-package "builtins")))
;(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(DEFPARAMETER |x| (|tuple| '(1 2 3)))
(ASSERT (|__eq__| (|getitem| |x| 0) 1) )
(DEFPARAMETER |y| (|tuple| '(1 2 3)))
(ASSERT (|__eq__| (|getitem| |y| 1) 2) )
(SETF |x| (|tuple| '(1 2 3 4 5)))
(SETF |y| (|tuple| '(1 2.0 3 4 5.0)))
(ASSERT (|__eq__| (|hash| |x| ) (|hash| |y| )) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

