(CL:EVAL-WHEN (:compile-toplevel :load-toplevel :execute)(CL:UNLESS (CL:FIND-PACKAGE "keyword_params.py")(make-package "keyword_params.py")(CL:USE-PACKAGE "builtins")))
(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(CL:DEFUN |f| (|a| CL:&KEY (|b| "yep") ) 
(CL:RETURN-FROM |f| |b|))
(|muleassert| (|__eq__| (|f| 1 ) "yep") )
(|muleassert| (|__eq__| (|f| 1 :|b| "NO") "NO") )
(CL:LOOP FOR S BEING EACH PRESENT-SYMBOL IN CL:*PACKAGE*
   WHEN (OR (CL:FBOUNDP S) (CL:BOUNDP S) (CL:FIND-CLASS S NIL))
   DO (CL:EXPORT S))

