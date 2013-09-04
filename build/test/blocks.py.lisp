(CL:EVAL-WHEN (:compile-toplevel :load-toplevel :execute)(CL:UNLESS (CL:FIND-PACKAGE "blocks.py")(make-package "blocks.py")(CL:USE-PACKAGE "builtins")))
;(proclaim '(optimize (speed 3)))
(CL:DEFUN |x| (  ) 
(CL:FLET ((|y| (  ) (CL:FLET ((|z| (  ) (CL:RETURN-FROM |z| 1))) (CL:RETURN-FROM |y| (+ 2 (|z|  )))))) (CL:RETURN-FROM |x| (+ 3 (|y|  )))))
(|muleassert| (|__eq__| (|x|  ) 6) )
(CL:LOOP FOR S BEING EACH PRESENT-SYMBOL IN CL:*PACKAGE*
   WHEN (OR (CL:FBOUNDP S)
            (CL:BOUNDP S)
            (CL:FIND-CLASS S NIL))
   DO (CL:EXPORT S))

