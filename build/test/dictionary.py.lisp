(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "dictionary.py")(make-package "dictionary.py" :use '("COMMON-LISP"))(use-package "builtins")))
(DEFPARAMETER |d| (|dict|  ))
(|setitem| |d| 1 2)
(|setitem| |d| "hey" 3)
(|setitem| |d| 4.0 "yup")
(ASSERT (|__eq__| (|getitem| |d| 1) 2) )
(ASSERT (|__eq__| (|len| |d| ) 3) )
(ASSERT (|__eq__| (|sorted| (|keys| |d| ) ) (|sorted| (|list| '(1 "hey" 4.0)) )) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

