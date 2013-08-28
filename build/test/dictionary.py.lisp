(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "dictionary.py")(make-package "dictionary.py" :use '("COMMON-LISP"))) ) (in-package "dictionary.py")
(use-package "builtins")(DEFPARAMETER |d| (|dict|  ))
(|setitem| |d| 1 2)
(ASSERT (EQUALP (|getitem| |d| 1) 2) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

