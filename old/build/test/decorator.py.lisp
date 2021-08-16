(CL:EVAL-WHEN (:compile-toplevel :load-toplevel :execute)(CL:UNLESS (CL:FIND-PACKAGE "decorator.py")(make-package "decorator.py")(CL:USE-PACKAGE "builtins")))
;(proclaim '(optimize (speed 3)))
(CL:DEFUN |add1| (|fun|  ) 
(CL:FLET ((|inner| (  ) (CL:RETURN-FROM |inner| (+ (|fun|  ) 1)))) (CL:RETURN-FROM |add1| #'inner)))
(CL:DEFUN |f| (  ) 
(CL:RETURN-FROM |f| 1))
(|muleassert| (|__eq__| (|f|  ) 2) )
(CL:LOOP FOR S BEING EACH PRESENT-SYMBOL IN CL:*PACKAGE*
   WHEN (OR (CL:FBOUNDP S)
            (CL:BOUNDP S)
            (CL:FIND-CLASS S NIL))
   DO (CL:EXPORT S))

