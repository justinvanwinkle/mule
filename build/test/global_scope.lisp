(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "global_scope")(make-package "global_scope" :use '("COMMON-LISP"))) ) (in-package "global_scope")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(DEFPARAMETER |x| 1) (+ |x| 1) (+ |x| 2)
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

