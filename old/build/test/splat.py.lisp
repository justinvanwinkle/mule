(CL:EVAL-WHEN (:compile-toplevel :load-toplevel :execute)(CL:UNLESS (CL:FIND-PACKAGE "splat.py")(make-package "splat.py")(CL:USE-PACKAGE "builtins")))
;(proclaim '(optimize (speed 3)))
(CL:DEFUN |f| (|a|  CL:&REST |args|) 
(CL:LET ((|x| |a|))
(CL:LOOP FOR |arg| BEING THE ELEMENTS OF |args| DO (CL:SETF |x| (+ |x| |arg|)))
(CL:RETURN-FROM |f| |x|)))
(|muleassert| (|__eq__| (|f| 1 1 2 ) 4) )
(|muleassert| (|__eq__| (|f| 1 ) 1) )
(CL:LOOP FOR S BEING EACH PRESENT-SYMBOL IN CL:*PACKAGE*
   WHEN (OR (CL:FBOUNDP S)
            (CL:BOUNDP S)
            (CL:FIND-CLASS S NIL))
   DO (CL:EXPORT S))

