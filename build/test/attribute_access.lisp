
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (UNLESS (FIND-PACKAGE "attribute_access")
    (MAKE-PACKAGE "attribute_access" :USE '("COMMON-LISP"))))
(IN-PACKAGE "attribute_access")
(REQUIRE 'ASDF)
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (ASDF/OPERATE:LOAD-SYSTEM :MULE))
(USE-PACKAGE "builtins")
(SETF (SLOT-VALUE |x| '|y|) 5)
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))