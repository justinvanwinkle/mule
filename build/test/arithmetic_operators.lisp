(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "arithmetic_operators")(make-package "arithmetic_operators" :use '("COMMON-LISP"))) ) (in-package "arithmetic_operators")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(+ (+ 1 1) 1) (* 2 2) (- 3 3) (/ 5 2) (DEFPARAMETER |x| 1) (DEFPARAMETER |y| 2) (- (* |x| |y|) 7) (- 7 (* |x| |y|)) (* (+ |x| |y|) 3) (* 3 (+ 1 2)) (+ (* 3 1) 2)
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

