
(SETF *READTABLE* (COPY-READTABLE NIL))
(SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
(DEFPACKAGE "namespaces"
  (:USE "CL" "SB-EXT"))
(IN-PACKAGE "namespaces")
(DEFPARAMETER x 1)
(DEFUN test (y) (SETF x 2) (RETURN-FROM test (+ x y)))
(DEFUN test2 (x) (SETF x 3) (RETURN-FROM test2 x))
(DEFUN test3 ()
  (LET ((y 10))
    (LOOP FOR x FROM 1 BELOW 10 BY 2
          DO (SETF y x))
    (RETURN-FROM test3 y)))
(ASSERT (EQUALP (test3) 9))