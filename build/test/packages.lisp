(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "packages")(make-package "packages" :use '("COMMON-LISP"))) ) (in-package "packages")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "COMMON-LISP")
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

