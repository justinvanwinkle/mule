
(SETF *READTABLE* (COPY-READTABLE NIL))
(SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
(DEFPACKAGE "test_splat"
  (:USE "CL" "SB-EXT"))
(IN-PACKAGE "test_splat")
(DEFUN f (a &REST args)
  (LET ((x a))
    (LOOP FOR arg BEING THE ELEMENTS OF args
          DO (INCF x arg))
    (RETURN-FROM f x)))
(ASSERT (EQUALP (f 1 1 2) 4))
(ASSERT (EQUALP (f 1) 1))