(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "init_scan.py")(make-package "init_scan.py" :use '("COMMON-LISP"))(use-package "builtins")))
;(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(DEFCLASS |A| () (|a| |b| |c| |d|)) (DEFUN |A| (  ) (LET ((|self| (make-instance '|A|))) (SETF (SLOT-VALUE |self| '|a|) 1)
(COND (1 (SETF (SLOT-VALUE |self| '|b|) 2)))
(LOOP WHILE t DO (SETF (SLOT-VALUE |self| '|c|) 3))
(LOOP FOR |x| FROM 0 BELOW 10 BY 1 DO (SETF (SLOT-VALUE |self| '|d|) (|str| "xxx")))
|self|))
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

