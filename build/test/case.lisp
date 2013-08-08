(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "case")(make-package "case" :use '("COMMON-LISP"))) ) (in-package "case")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(DEFPARAMETER |a| 1) (DEFPARAMETER |A| 2) (|ASSERT| (EQUALP (+ |a| 1) |A|) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

