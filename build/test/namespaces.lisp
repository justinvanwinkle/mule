
(DEFPACKAGE "namespaces"
  (:USE "CL" "SB-EXT"))
(IN-PACKAGE "namespaces")
(REQUIRE 'ASDF)
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (ASDF/OPERATE:LOAD-SYSTEM :MULE))
(DEFPARAMETER |x| 1)
(DEFUN TEST (|y|) (SETF |x| 2) (RETURN-FROM |test| (+ |x| |y|)))
(DEFUN TEST2 (|x|) (SETF |x| 3) (RETURN-FROM |test2| |x|))
(DEFUN TEST3 ()
  (LET ((|y| 10))
    (LOOP FOR |x| FROM 1 BELOW 10 BY 2
          DO (SETF |y| |x|))
    (RETURN-FROM |test3| |y|)))
(ASSERT (EQUALP (|test3|) 9))
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))