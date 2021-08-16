(CL:EVAL-WHEN (:compile-toplevel :load-toplevel :execute)(CL:UNLESS (CL:FIND-PACKAGE "add_names.py")(make-package "add_names.py")(CL:USE-PACKAGE "builtins")))
;(proclaim '(optimize (speed 3)))
(CL:DEFUN |test| (  ) 
(CL:LET ((|x| 1))
(CL:LET ((|y| 2))
(|muleassert| (|__eq__| (+ |x| |y|) 3) ))))
(|test|  )
(CL:LOOP FOR S BEING EACH PRESENT-SYMBOL IN CL:*PACKAGE*
   WHEN (OR (CL:FBOUNDP S)
            (CL:BOUNDP S)
            (CL:FIND-CLASS S NIL))
   DO (CL:EXPORT S))

