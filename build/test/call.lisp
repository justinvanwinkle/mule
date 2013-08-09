(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "call")(make-package "call" :use '("COMMON-LISP"))) ) (in-package "call")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "builtins")(|t|  ) (DEFPARAMETER |x| (|t|  )) (|b| |a| ) (|c| (SLOT-VALUE |a| '|b|) 1 ) ((|x|  )  )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

