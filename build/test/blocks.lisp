(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "blocks")(make-package "blocks" :use '("COMMON-LISP"))) ) (in-package "blocks")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(DEFUN |x| (  ) (FLET ((|y| (  ) (FLET ((|z| (  ) (RETURN-FROM |z| 1))) (RETURN-FROM |y| (+ 2 (|z|  )))))) (RETURN-FROM |x| (+ 3 (|y|  ))))) (|ASSERT| (EQUALP (|x|  ) 6) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

