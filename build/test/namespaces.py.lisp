(CL:EVAL-WHEN (:compile-toplevel :load-toplevel :execute)(CL:UNLESS (CL:FIND-PACKAGE "namespaces.py")(make-package "namespaces.py")(CL:USE-PACKAGE "builtins")))
(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(CL:DEFPARAMETER |x| 1)
(CL:DEFUN |test| (|y|  ) 
(CL:SETF |x| 2)
(CL:RETURN-FROM |test| (+ |x| |y|)))
(CL:DEFUN |test2| (|x|  ) 
(CL:SETF |x| 3)
(CL:RETURN-FROM |test2| |x|))
(CL:DEFUN |test3| (  ) 
(CL:LET ((|y| 10))
(CL:LOOP FOR |x| FROM 1 BELOW 10 BY 2 DO (CL:SETF |y| |x|))
(CL:RETURN-FROM |test3| |y|)))
(|muleassert| (|__eq__| (|test3|  ) 9) )
(CL:LOOP FOR S BEING EACH PRESENT-SYMBOL IN CL:*PACKAGE*
   WHEN (OR (CL:FBOUNDP S) (CL:BOUNDP S) (CL:FIND-CLASS S NIL))
   DO (CL:EXPORT S))

