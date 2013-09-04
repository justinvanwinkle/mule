(CL:EVAL-WHEN (:compile-toplevel :load-toplevel :execute)(CL:UNLESS (CL:FIND-PACKAGE "literal")(make-package "literal")(CL:USE-PACKAGE "builtins")))
;(proclaim '(optimize (speed 3)))
(|muleassert| (|__eq__| "I WORK" "I WORK") )
(CL:DEFPARAMETER |x| 5)
(setf |x| 3)
(|muleassert| (|__eq__| |x| 3) )
(CL:LOOP FOR S BEING EACH PRESENT-SYMBOL IN CL:*PACKAGE*
   WHEN (OR (CL:FBOUNDP S)
            (CL:BOUNDP S)
            (CL:FIND-CLASS S NIL))
   DO (CL:EXPORT S))

