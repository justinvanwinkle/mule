(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "tuple")(make-package "tuple" :use '("COMMON-LISP"))) ) (in-package "tuple")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "builtins")(DEFPARAMETER |x| (|tuple| '(1 2 3))) (ASSERT (EQUALP (|getitem| |x| 0) 1) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

