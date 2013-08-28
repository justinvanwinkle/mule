(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "tuple.py")(make-package "tuple.py" :use '("COMMON-LISP"))) ) (in-package "tuple.py")
(use-package "builtins")(DEFPARAMETER |x| (|tuple| '(1 2 3)))
(ASSERT (EQUALP (|getitem| |x| 0) 1) )
(DEFPARAMETER |y| (|tuple| '(1 2 3)))
(ASSERT (EQUALP (|getitem| |y| 1) 2) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

