(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "namespaces")(make-package "namespaces" :use '("COMMON-LISP"))) ) (in-package "namespaces")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(DEFPARAMETER |x| 1) (DEFUN |test| (|y|  ) (SETF |x| 2) (RETURN-FROM |test| (+ |x| |y|))) (DEFUN |test2| (|x|  ) (SETF |x| 3) (RETURN-FROM |test2| |x|)) (DEFUN |test3| (  ) (LET ((|y| 10)) (LOOP FOR |x| FROM 1 BELOW 10 BY 2 DO (SETF |y| |x|)) (RETURN-FROM |test3| |y|))) (|ASSERT| (EQUALP (|test3|  ) 9) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

