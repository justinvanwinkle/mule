(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "mixed")(make-package "mixed" :use '("COMMON-LISP"))) ) (in-package "mixed")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(DEFCLASS |CounterX| () (|count|)) (DEFMETHOD |__init__| ((|self| |CounterX|)   ) (SETF (SLOT-VALUE |self| '|count|) 0)) (DEFMETHOD |incr| ((|self| |CounterX|)   ) (SETF (SLOT-VALUE |self| '|count|) (+ (SLOT-VALUE |self| '|count|) 1))) (DEFUN |CounterX| ()
                    (LET ((|self| (MAKE-INSTANCE '|CounterX|)))
                       
                       |self|)) (DEFUN |foo| (  ) (LET ((|c| (|CounterX|  ))) (LOOP WHILE (< (SLOT-VALUE |c| '|count|) 10000000) DO (|incr| |c| )) (RETURN-FROM |foo| (SLOT-VALUE |c| '|count|)))) (DEFPARAMETER |x| (|foo|  )) |print| |x|
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

