(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "attribute_access")(make-package "attribute_access" :use '("COMMON-LISP"))) ) (in-package "attribute_access")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "builtins")(SETF (SLOT-VALUE |x| '|y|) 5)
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

