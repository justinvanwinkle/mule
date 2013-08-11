(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "global_scope")(make-package "global_scope" :use '("COMMON-LISP"))) ) (in-package "global_scope")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "builtins")(DEFPARAMETER |x| 1) (DEFUN |x_plus| (|delta|  ) (SETF |x| (+ |x| |delta|))) (ASSERT (EQUALP |x| 1) ) (|x_plus| 5 ) (ASSERT (EQUALP |x| 6) ) (|x_plus| (- 6 ) ) (ASSERT (EQUALP |x| 0) ) (|x_plus| (- 5 (- 3 )) ) (ASSERT (EQUALP |x| 8) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

