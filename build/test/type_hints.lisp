(CL:EVAL-WHEN (:compile-toplevel :load-toplevel :execute)(CL:UNLESS (CL:FIND-PACKAGE "type_hints")(make-package "type_hints")(CL:USE-PACKAGE "builtins")))
(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(CL:DEFUN |foo| (  ) 
(CL:LET ((|x| 0))
(DECLARE (TYPE |FIXNUM| |x| ) )
(CL:LET ((|lim| 4000))
(CL:LOOP WHILE (< (the |FIXNUM| |x|) |lim|) DO (CL:SETF |x| (the |FIXNUM| (+ |x| 1))))
(CL:RETURN-FROM |foo| |x|))))
(|muleassert| (|__eq__| (|foo|  ) 4000) )
(CL:LOOP FOR S BEING EACH PRESENT-SYMBOL IN CL:*PACKAGE*
   WHEN (OR (CL:FBOUNDP S) (CL:BOUNDP S) (CL:FIND-CLASS S NIL))
   DO (CL:EXPORT S))

