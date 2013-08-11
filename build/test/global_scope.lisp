
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (UNLESS (FIND-PACKAGE "global_scope")
    (MAKE-PACKAGE "global_scope" :USE '("COMMON-LISP"))))
(IN-PACKAGE "global_scope")
(REQUIRE 'ASDF)
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (ASDF/OPERATE:LOAD-SYSTEM :MULE))
(USE-PACKAGE "builtins")
(DEFPARAMETER |x| 1)
(DEFUN |x_plus| (|delta|) (SETF |x| (+ |x| |delta|)))
(ASSERT (EQUALP |x| 1))
(|x_plus| 5)
(ASSERT (EQUALP |x| 6))
(|x_plus| (- 6))
(ASSERT (EQUALP |x| 0))
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))