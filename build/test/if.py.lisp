(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "if.py")(make-package "if.py" :use '("COMMON-LISP"))(use-package "builtins")))
(DEFPARAMETER |x| 0)
(DEFUN |foo| (  ) (COND ((|__eq__| |x| 1) (RETURN-FROM |foo| 1)) ((|__eq__| |x| 2) (RETURN-FROM |foo| 2)) (t (RETURN-FROM |foo| NIL))))
(ASSERT (|__eq__| (|foo|  ) NIL) )
(SETF |x| 1)
(ASSERT (|__eq__| (|foo|  ) 1) )
(SETF |x| 2)
(ASSERT (|__eq__| (|foo|  ) 2) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

