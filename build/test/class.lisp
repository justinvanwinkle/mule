(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "class")(make-package "class" :use '("COMMON-LISP"))) ) (in-package "class")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "builtins")(DEFPARAMETER |a| 1) (DEFCLASS |A| () (|a|)) (DEFMETHOD |meth| ((|self| |A|)   ) (RETURN-FROM |meth| (+ (SLOT-VALUE |self| '|a|) 1))) (DEFUN |A| (|a|  ) (LET ((|self| (make-instance '|A|))) (SETF (SLOT-VALUE |self| '|a|) |a|) |self|)) (SETF |a| (|A| 1 )) (|ASSERT| (EQUALP (SLOT-VALUE |a| '|a|) 1) ) (|ASSERT| (EQUALP (|meth| |a| ) 2) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

