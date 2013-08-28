
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (UNLESS (FIND-PACKAGE "splat.py")
    (MAKE-PACKAGE "splat.py" :USE '("COMMON-LISP"))))
(IN-PACKAGE "splat.py")
(REQUIRE :ASDF)
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (ASDF:LOAD-SYSTEM :MULE))
(USE-PACKAGE "builtins")
(DEFUN |f| (|a| &REST |args|)
  (LET ((|x| |a|))
    (LOOP FOR |arg| BEING THE ELEMENTS OF |args|
          DO (SETF |x| (+ |x| |arg|)))
    (RETURN-FROM |f| |x|)))
(ASSERT (EQUALP (|f| 1 1 2) 4))
(ASSERT (EQUALP (|f| 1) 1))
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))