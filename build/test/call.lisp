(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "call")(make-package "call" :use '("COMMON-LISP"))) ) (in-package "call")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "builtins")(DEFCLASS |A| () (|b|)) (DEFMETHOD |ack| ((|self| |A|)   ) (RETURN-FROM |ack| 1)) (DEFUN |A| (  ) (LET ((|self| (make-instance '|A|))) |self|)) (DEFCLASS |B| () NIL) (DEFMETHOD |c| ((|self| |B|) |arg|  ) (RETURN-FROM |c| |arg|)) (DEFUN |B| (  ) (LET ((|self| (make-instance '|B|))) |self|)) (DEFPARAMETER |a| (|A|  )) (SETF (SLOT-VALUE |a| '|b|) (|B|  )) (ASSERT (EQUALP (|ack| |a| ) 1) ) (ASSERT (EQUALP (|c| (SLOT-VALUE |a| '|b|) 1 ) 1) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

