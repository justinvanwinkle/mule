
(DEFPACKAGE "keyword_params"
  (:USE "CL" "SB-EXT"))
(IN-PACKAGE "keyword_params")
(REQUIRE 'ASDF)
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (ASDF/OPERATE:LOAD-SYSTEM :MULE))
(DEFUN F (|a| &KEY (|b| "yep")) (RETURN-FROM |f| |b|))
(ASSERT (EQUALP (|f| 1) "yep"))
(ASSERT (EQUALP (|f| 1 :|b| "NO") "NO"))
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))