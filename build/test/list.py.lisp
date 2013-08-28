(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "list.py")(make-package "list.py" :use '("COMMON-LISP"))) ) (in-package "list.py")
(use-package "builtins")(DEFPARAMETER |x| (|list| '()))
(DEFPARAMETER |y| (|list| '(9)))
(DEFPARAMETER |z| (|list| '(1 2 3)))
(DEFUN |foo| (  ) (LET ((|l| (|list| '()))) (SETF |x| 0)
(LOOP FOR |e| BEING THE ELEMENTS OF |l| DO (SETF |x| 1))
(RETURN-FROM |foo| |x|)))
(ASSERT (EQUALP (|getitem| |z| 0) 1) )
(ASSERT (EQUALP (|getitem| |y| 0) 9) )
(ASSERT (EQUALP (|foo|  ) 0) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

