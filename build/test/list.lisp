(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "list")(make-package "list" :use '("COMMON-LISP"))) ) (in-package "list")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(DEFPARAMETER |x| (List)) (DEFPARAMETER |y| (List)) (DEFPARAMETER |z| (List)) (|ASSERT| (EQUALP |z| (List)) ) (|ASSERT| (EQUALP (|getitem| |y| 1) 9) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

