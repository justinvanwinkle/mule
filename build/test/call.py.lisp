(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "call.py")(make-package "call.py" :use '("COMMON-LISP"))(use-package "builtins")))
(DEFUN |foo| (  ) (RETURN-FROM |foo| 1))
(DEFCLASS |A| () (|b|)) (DEFMETHOD |ack| ((|self| |A|)   ) (RETURN-FROM |ack| 1)) (DEFUN |A| (  ) (LET ((|self| (make-instance '|A|))) (SETF (SLOT-VALUE |self| '|b|) NIL)
|self|))
(DEFCLASS |B| () (|x|)) (DEFMETHOD |c| ((|self| |B|) |arg|  ) (RETURN-FROM |c| |arg|)) (DEFUN |B| (  ) (LET ((|self| (make-instance '|B|))) (SETF (SLOT-VALUE |self| '|x|) (|foo|  ))
|self|))
(DEFPARAMETER |a| (|A|  ))
(SETF (SLOT-VALUE |a| '|b|) (|B|  ))
(ASSERT (|__eq__| (|ack| |a| ) 1) )
(ASSERT (|__eq__| (|c| (SLOT-VALUE |a| '|b|) 1 ) 1) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

