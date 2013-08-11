(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "if")(make-package "if" :use '("COMMON-LISP"))) ) (in-package "if")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "builtins")(DEFPARAMETER |x| 0) (DEFUN |foo| (  ) (COND ((EQUALP |x| 1) (RETURN-FROM |foo| 1)) ((EQUALP |x| 2) (RETURN-FROM |foo| 2)) (t (RETURN-FROM |foo| NIL)))) (ASSERT (EQUALP (|foo|  ) NIL) ) (SETF |x| 1) (ASSERT (EQUALP (|foo|  ) 1) ) (SETF |x| 2) (ASSERT (EQUALP (|foo|  ) 2) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

