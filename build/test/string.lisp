(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "string")(make-package "string" :use '("COMMON-LISP"))(use-package "builtins")))
#| equality of STRING and str
|#
(ASSERT (|__eq__| "test" (|str| "test")) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

