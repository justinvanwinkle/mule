(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "keyword_params.py")(make-package "keyword_params.py" :use '("COMMON-LISP"))(use-package "builtins")))
(DEFUN |f| (|a| &KEY (|b| (|str| "yep")) ) (RETURN-FROM |f| |b|))
(ASSERT (|__eq__| (|f| 1 ) (|str| "yep")) )
(ASSERT (|__eq__| (|f| 1 :|b| (|str| "NO")) (|str| "NO")) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

