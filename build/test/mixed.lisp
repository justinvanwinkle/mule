(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "mixed")(make-package "mixed" :use '("COMMON-LISP"))) ) (in-package "mixed")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "builtins")(DEFCLASS |CounterX| () (|count|)) (DEFMETHOD |increment| ((|self| |CounterX|)   ) (SETF (SLOT-VALUE |self| '|count|) (+ (SLOT-VALUE |self| '|count|) 1))) (DEFUN |CounterX| (  ) (LET ((|self| (make-instance '|CounterX|))) (SETF (SLOT-VALUE |self| '|count|) 0) |self|)) (DEFUN |foo| (  ) (LET ((|c| (|CounterX|  ))) (|increment| |c| ) (LOOP WHILE (< (SLOT-VALUE |c| '|count|) 1000) DO (|increment| |c| )) (RETURN-FROM |foo| (SLOT-VALUE |c| '|count|)))) (DEFPARAMETER |x| (|foo|  )) (|ASSERT| (EQUALP |x| 1000) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

