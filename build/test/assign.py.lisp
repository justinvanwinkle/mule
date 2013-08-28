
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (UNLESS (FIND-PACKAGE "assign.py")
    (MAKE-PACKAGE "assign.py" :USE '("COMMON-LISP"))))
(IN-PACKAGE "assign.py")
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (LOAD "/home/jvanwink/repos/mule/build/lib/builtins.fasl"))
(USE-PACKAGE "builtins")
(DEFPARAMETER |x| 1)
(DEFPARAMETER |y| 2)
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))