(CL:EVAL-WHEN (:compile-toplevel :load-toplevel :execute)(CL:UNLESS (CL:FIND-PACKAGE "loop.py")(make-package "loop.py")(CL:USE-PACKAGE "builtins")))
(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(CL:DEFUN |simple_loop| (  ) 
(CL:LOOP FOR |x| BEING THE ELEMENTS OF (|tuple| '(1 2 3)) DO (CL:PROGN (|print| |x| )
(|print| (+ |x| |x|) ))
))
#| def range_loop():
|#
#|     for x in range(0, 10):
|#
#|         print(x)
|#
#|         print(x + x)
|#
#|     for x in range(10):
|#
#|         print(x)
|#
#|         print(x * x)
|#
#| simple_loop()
|#
(CL:LOOP FOR S BEING EACH PRESENT-SYMBOL IN CL:*PACKAGE*
   WHEN (OR (CL:FBOUNDP S) (CL:BOUNDP S) (CL:FIND-CLASS S NIL))
   DO (CL:EXPORT S))

