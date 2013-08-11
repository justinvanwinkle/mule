
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (UNLESS (FIND-PACKAGE "add_names")
    (MAKE-PACKAGE "add_names" :USE '("COMMON-LISP"))))
(IN-PACKAGE "add_names")
(REQUIRE 'ASDF)
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (ASDF/OPERATE:LOAD-SYSTEM :MULE))
(USE-PACKAGE "builtins")
(DEFUN |test| ()
  (LET ((|x| 1))
    (LET ((|y| 2))
      (ASSERT (EQUALP (+ |x| |y|) 3)))))
(|test|)
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))