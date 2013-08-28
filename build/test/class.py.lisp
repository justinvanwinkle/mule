(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "class.py")(make-package "class.py" :use '("COMMON-LISP"))) ) (in-package "class.py")
(use-package "builtins")(DEFPARAMETER |a| 1)
(DEFCLASS |A| () (|a| |b|)) (DEFMETHOD |meth| ((|self| |A|)   ) (RETURN-FROM |meth| (+ (SLOT-VALUE |self| '|a|) 1))) (DEFUN |A| (|a|  ) (LET ((|self| (make-instance '|A|))) (SETF (SLOT-VALUE |self| '|a|) |a|)
(SETF (SLOT-VALUE |self| '|b|) NIL)
|self|))
(SETF |a| (|A| 1 ))
(ASSERT (EQUALP (SLOT-VALUE |a| '|a|) 1) )
(ASSERT (EQUALP (|meth| |a| ) 2) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

