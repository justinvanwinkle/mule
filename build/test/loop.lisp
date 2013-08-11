(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "loop")(make-package "loop" :use '("COMMON-LISP"))) ) (in-package "loop")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "builtins")(DEFUN |simple_loop| (  ) (LOOP FOR |x| BEING THE ELEMENTS OF (|tuple| '(1 2 3)) DO (PROGN (|print| |x| ) (|print| (+ |x| |x|) )))) #| def range_loop():
|# #|     for x in range(0, 10):
|# #|         print(x)
|# #|         print(x + x)
|# #|     for x in range(10):
|# #|         print(x)
|# #|         print(x * x)
|# #| simple_loop()
|#
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

