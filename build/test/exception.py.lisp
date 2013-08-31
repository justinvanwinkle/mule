(CL:EVAL-WHEN (:compile-toplevel :load-toplevel :execute)(CL:UNLESS (CL:FIND-PACKAGE "exception.py")(make-package "exception.py")(CL:USE-PACKAGE "builtins")))
(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(CL:DEFUN |foo| (|x|  ) 
(CL:LET ((|y| NIL))
(CL:LET ((|z| 1))
(CL:UNWIND-PROTECT (CL:HANDLER-CASE 
(CL:PROGN (CL:SETF |y| (/ 1 |x|)))
 
(|DIVISION-BY-ZERO| () (CL:SETF |y| 1))
(CL:CONDITION (|e|) (CL:SETF |y| 3))) (CL:SETF |y| (+ |y| |z|)))
(CL:RETURN-FROM |foo| |y|))))
(|muleassert| (|__eq__| (|foo| 0 ) 2) )
(|muleassert| (|__eq__| (|foo| "herp" ) 4) )
(CL:HANDLER-CASE 
(CL:PROGN (|muleassert| (|__eq__| 1 0) ))
 
(|AssertionError| () NIL))
(CL:LOOP FOR S BEING EACH PRESENT-SYMBOL IN CL:*PACKAGE*
   WHEN (OR (CL:FBOUNDP S) (CL:BOUNDP S) (CL:FIND-CLASS S NIL))
   DO (CL:EXPORT S))

