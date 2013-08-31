(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "loop.py")(make-package "loop.py")(use-package "builtins")))
;(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(DEFUN |simple_loop| (  ) (LOOP FOR |x| BEING THE ELEMENTS OF (|tuple| '(1 2 3)) DO (CL:PROGN (|print| |x| )
(|print| (+ |x| |x|) ))))
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
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

