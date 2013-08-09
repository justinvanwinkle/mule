(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "assign")(make-package "assign" :use '("COMMON-LISP"))) ) (in-package "assign")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "builtins")(DEFPARAMETER |x| 1) (DEFPARAMETER |y| 2)
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

