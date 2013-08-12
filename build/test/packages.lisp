
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (UNLESS (FIND-PACKAGE "packages")
    (MAKE-PACKAGE "packages" :USE '("COMMON-LISP"))))
(IN-PACKAGE "packages")
(REQUIRE 'ASDF)
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (ASDF:LOAD-SYSTEM :MULE))
(USE-PACKAGE "builtins")
(USE-PACKAGE "COMMON-LISP")
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))