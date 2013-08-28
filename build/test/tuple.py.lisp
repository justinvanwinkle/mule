
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (UNLESS (FIND-PACKAGE "tuple.py")
    (MAKE-PACKAGE "tuple.py" :USE '("COMMON-LISP"))))
(IN-PACKAGE "tuple.py")
(REQUIRE :ASDF)
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (ASDF:LOAD-SYSTEM :MULE))
(USE-PACKAGE "builtins")
(DEFPARAMETER |x| (|tuple| '(1 2 3)))
(ASSERT (EQUALP (|getitem| |x| 0) 1))
(DEFPARAMETER |y| (|tuple| '(1 2 3)))
(ASSERT (EQUALP (|getitem| |y| 1) 2))
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))