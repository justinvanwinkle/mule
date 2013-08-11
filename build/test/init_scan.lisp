(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "init_scan")(make-package "init_scan" :use '("COMMON-LISP"))) ) (in-package "init_scan")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "builtins")(DEFCLASS |A| () (|a| |b| |c| |d|)) (DEFUN |A| (  ) (LET ((|self| (make-instance '|A|))) (SETF (SLOT-VALUE |self| '|a|) 1) (COND (1 (SETF (SLOT-VALUE |self| '|b|) 2))) (LOOP WHILE t DO (SETF (SLOT-VALUE |self| '|c|) 3)) (LOOP FOR |x| FROM 0 BELOW 10 BY 1 DO (SETF (SLOT-VALUE |self| '|d|) "xxx")) |self|))
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

