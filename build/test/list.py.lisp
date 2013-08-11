
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (UNLESS (FIND-PACKAGE "list.py")
    (MAKE-PACKAGE "list.py" :USE '("COMMON-LISP"))))
(IN-PACKAGE "list.py")
(REQUIRE 'ASDF)
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (ASDF/OPERATE:LOAD-SYSTEM :MULE))
(USE-PACKAGE "builtins")
(DEFPARAMETER |x| (|list| 'NIL))
(DEFPARAMETER |y| (|list| '(9)))
(DEFPARAMETER |z| (|list| '(1 2 3)))
(ASSERT (EQUALP (|getitem| |z| 0) 1))
(ASSERT (EQUALP (|getitem| |y| 0) 9))
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))