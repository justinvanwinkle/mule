(CL:EVAL-WHEN (:compile-toplevel :load-toplevel :execute)(CL:UNLESS (CL:FIND-PACKAGE "init_scan.py")(make-package "init_scan.py")(CL:USE-PACKAGE "builtins")))
(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(CL:DEFCLASS |A| () (|a| |b| |c| |d|)) (CL:DEFUN |A| (  ) 
(CL:LET ((|self| (CL:MAKE-INSTANCE '|A|)))
(CL:SETF (CL:SLOT-VALUE |self| '|a|) 1)
(COND (1 (CL:SETF (CL:SLOT-VALUE |self| '|b|) 2)))
(CL:LOOP WHILE t DO (CL:SETF (CL:SLOT-VALUE |self| '|c|) 3))
(CL:LOOP FOR |x| FROM 0 BELOW 10 BY 1 DO (CL:SETF (CL:SLOT-VALUE |self| '|d|) "xxx"))
|self|))
(CL:LOOP FOR S BEING EACH PRESENT-SYMBOL IN CL:*PACKAGE*
   WHEN (OR (CL:FBOUNDP S) (CL:BOUNDP S) (CL:FIND-CLASS S NIL))
   DO (CL:EXPORT S))

