(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "mixed.py")(make-package "mixed.py" :use '("COMMON-LISP"))(use-package "builtins")))
(DEFCLASS |CounterX| () (|count|)) (DEFMETHOD |increment| ((|self| |CounterX|)   ) (SETF (SLOT-VALUE |self| '|count|) (+ (SLOT-VALUE |self| '|count|) 1))) (DEFUN |CounterX| (  ) (LET ((|self| (make-instance '|CounterX|))) (SETF (SLOT-VALUE |self| '|count|) 0)
|self|))
(DEFUN |foo| (  ) (LET ((|c| (|CounterX|  ))) (|increment| |c| )
(LOOP WHILE (< (SLOT-VALUE |c| '|count|) 1000) DO (|increment| |c| ))
(RETURN-FROM |foo| (SLOT-VALUE |c| '|count|))))
(DEFPARAMETER |x| (|foo|  ))
(ASSERT (|__eq__| |x| 1000) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

