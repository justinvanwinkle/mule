(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "literal")(make-package "literal" :use '("COMMON-LISP"))) ) (in-package "literal")
(use-package "builtins")(ASSERT (EQUALP "I WORK" "I WORK") )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

