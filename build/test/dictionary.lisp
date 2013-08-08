(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "dictionary")(make-package "dictionary" :use '("COMMON-LISP"))) ) (in-package "dictionary")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "builtins") (DEFPARAMETER |d| (|dict|  )) (|setitem| |d| 1 2) (|ASSERT| (EQUALP (|getitem| |d| 1) 2) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

