
(EVAL-WHEN (:COMPILE-TOPLEVEL)
  (SETF *READTABLE* (COPY-READTABLE NIL))
  (SETF (READTABLE-CASE *READTABLE*) :PRESERVE))
(DEFPACKAGE "defun"
  (:USE "CL" "SB-EXT"))
(IN-PACKAGE "defun")
(DEFUN test (x) (RETURN-FROM test (+ x 1)))
(DEFUN test2 (x y)
  (LET ((z (+ x y)))
    (RETURN-FROM test2 z)))
(DEFUN test3 (x) (test 1) (test2 5 7) (RETURN-FROM test3 (+ (test 1) 55)))
(DEFUN test_outer (x)
  (FLET ((test_inner (y)
           (RETURN-FROM test_inner (+ y 1))))
    (RETURN-FROM test_outer (test_inner (+ x 1)))))
(ASSERT (EQUALP (test 5) 6))
(ASSERT (EQUALP (test2 1 2) 3))
(ASSERT (EQUALP (test3 1) 57))
(ASSERT (EQUALP (test_outer 5) 7))