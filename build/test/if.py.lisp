(CL:EVAL-WHEN (:compile-toplevel :load-toplevel :execute)(CL:UNLESS (CL:FIND-PACKAGE "if.py")(make-package "if.py")(CL:USE-PACKAGE "builtins")))
(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(CL:DEFPARAMETER |x| 0)
(CL:DEFUN |foo| (  ) 
(COND ((|__eq__| |x| 1) (CL:RETURN-FROM |foo| 1)) ((|__eq__| |x| 2) (CL:RETURN-FROM |foo| 2)) (t (CL:RETURN-FROM |foo| NIL))))
(|muleassert| (|__eq__| (|foo|  ) NIL) )
(CL:SETF |x| 1)
(|muleassert| (|__eq__| (|foo|  ) 1) )
(CL:SETF |x| 2)
(|muleassert| (|__eq__| (|foo|  ) 2) )
(CL:LOOP FOR S BEING EACH PRESENT-SYMBOL IN CL:*PACKAGE*
   WHEN (OR (CL:FBOUNDP S) (CL:BOUNDP S) (CL:FIND-CLASS S NIL))
   DO (CL:EXPORT S))

