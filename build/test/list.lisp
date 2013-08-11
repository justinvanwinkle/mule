(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "list")(make-package "list" :use '("COMMON-LISP"))) ) (in-package "list")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "builtins")(DEFPARAMETER |x| (|list| '())) (DEFPARAMETER |y| (|list| '(9))) (DEFPARAMETER |z| (|list| '(1 2 3))) (ASSERT (EQUALP (|getitem| |z| 0) 1) ) (ASSERT (EQUALP (|getitem| |y| 0) 9) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

