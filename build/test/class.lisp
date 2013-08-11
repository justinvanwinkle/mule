
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (UNLESS (FIND-PACKAGE "class") (MAKE-PACKAGE "class" :USE '("COMMON-LISP"))))
(IN-PACKAGE "class")
(REQUIRE 'ASDF)
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (ASDF/OPERATE:LOAD-SYSTEM :MULE))
(USE-PACKAGE "builtins")
(DEFPARAMETER |a| 1)
(DEFCLASS A NIL (|a| |b|))
(DEFMETHOD |meth| ((|self| A))
  (RETURN-FROM |meth| (+ (SLOT-VALUE |self| '|a|) 1)))
(DEFUN A (|a|)
  (LET ((|self| (MAKE-INSTANCE 'A)))
    (SETF (SLOT-VALUE |self| '|a|) |a|)
    |self|))
(SETF |a| (A 1))
(ASSERT (EQUALP (SLOT-VALUE |a| '|a|) 1))
(ASSERT (EQUALP (|meth| |a|) 2))
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))