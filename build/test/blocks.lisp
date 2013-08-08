
(DEFPACKAGE "blocks"
  (:USE "CL" "SB-EXT"))
(IN-PACKAGE "blocks")
(REQUIRE 'ASDF)
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (ASDF/OPERATE:LOAD-SYSTEM :MULE))
(DEFUN X ()
  (FLET ((|y| ()
           (FLET ((|z| ()
                    (RETURN-FROM |z| 1)))
             (RETURN-FROM |y| (+ 2 (|z|))))))
    (RETURN-FROM |x| (+ 3 (|y|)))))
(ASSERT (EQUALP (|x|) 6))
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))