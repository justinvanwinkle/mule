
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (UNLESS (FIND-PACKAGE "arithmetic_operators")
    (MAKE-PACKAGE "arithmetic_operators" :USE '("COMMON-LISP"))))
(IN-PACKAGE "arithmetic_operators")
(REQUIRE 'ASDF)
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (ASDF/OPERATE:LOAD-SYSTEM :MULE))
(USE-PACKAGE "builtins")
(+ (+ 1 1) 1)
(* 2 2)
(- 3 3)
(/ 5 2)
(DEFPARAMETER |x| 1)
(DEFPARAMETER |y| 2)
(- (* |x| |y|) 7)
(- 7 (* |x| |y|))
(* (+ |x| |y|) 3)
(* 3 (+ 1 2))
(+ (* 3 1) 2)
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))