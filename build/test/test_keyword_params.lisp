
(SETF *READTABLE* (COPY-READTABLE NIL))
(SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
(DEFPACKAGE "test_keyword_params"
  (:USE "CL" "SB-EXT"))
(IN-PACKAGE "test_keyword_params")
(DEFUN f (a &KEY (b "yep")) (RETURN-FROM f b))
(ASSERT (EQUALP (f 1) "yep"))
(ASSERT (EQUALP (f 1 :b "NO") "NO"))